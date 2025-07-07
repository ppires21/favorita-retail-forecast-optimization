# Interface.R ---------------------------------------------------------------
library(shiny)
library(DT)

# Carrega utilitários e módulos de otimização
source("otiutils.R")
source("mc_algo.R")    # define optimize_mc()
source("pso_algo.R")   # define optimize_pso()
source("spea-2_algo.R") # define optimize_spea2()
source("hill_algo.R") # define optimize_hill()
source("hillClimbing.R") #define optimize_hillClimbing()
source("ga_algo1.R")   # define optimize_ga1()
source("ga_algo2.R")   # define optimize_ga2()
source("sa_algo.R")         # define optimize_sa()
source("sa_weighted_algo.R")# define optimize_sa_weighted()
source("O1_F2_DE.R")    #define optimize_de()
source("O2_NSGA-2.R") #define optimize_nsga2()


# ---- Dados de previsão ----------------------------------------------------
df_pred     <- read.csv("ITER_Sales_prev.csv")
sales_cols  <- c("s_d11","s_d12","s_e11","s_e12","s_b11","s_b12")
sem_choices <- sort(unique(df_pred$Iteracao))

# preparar as matrizes de previsão de todas as semanas
pred_list <- lapply(sem_choices, function(s)
  as.matrix(df_pred[df_pred$Iteracao == s, sales_cols]))

# ---- Datas para detectar fins-de-semana ----------------------------------
sales_dates <- read.csv("sales_clean.csv", sep = ";")$date
sales_dates <- as.Date(sales_dates)
sales_dates <- tail(sales_dates, 365)
get_weekend_vec <- function(idx) {
  dias <- sales_dates[((idx - 1) * 7 + 1):(idx * 7)]
  Sys.setlocale("LC_TIME", "pt_PT.UTF-8")
  weekdays(dias) %in% c("sábado", "domingo")
}

# ---- UI -------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Planeamento semanal"),
  sidebarLayout(
    sidebarPanel(
      selectInput("alg", "Algoritmo:",
                  choices = c("Monte Carlo" = "MC",
                              "PSO"         = "PSO",
                              "Hill Climbing"             = "HC",
                              "Genetic Algorithm"             = "GA",
                              "SPEA-2"      = "SPEA2",
                              "Genetic Algorithm com Pesos"   = "GAP",
                              "Hill Climbing com Pesos"   = "HCP",
                              "Simulated Annealing" = "SA",
                              "Simulated Annealing Penalizada" = "SAW",
                              "Differential Evolution" = "DE",
                              "NSGA-2" = "NSGA2")),
      selectInput("semana","Semana:", choices = sem_choices),
      actionButton("run", "Executar"),
      br(), textOutput("msg"),
      hr(),
      h4(textOutput("kpiLucro")),
      h4(textOutput("kpiRecursos"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Previsões",
                 DTOutput("tabPrev")
        ),
        tabPanel("Planeamento",
                 uiOutput("planeamentoUI")
        )
      )
    )
  )
)

# ---- SERVER ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # 1) Previsões da semana escolhida (para preview)
  pred_mat <- reactive({
    req(input$semana)
    as.matrix(df_pred[df_pred$Iteracao == input$semana, sales_cols])
  })
  
  # 2) Armazena resultados
  vals <- reactiveValues(
    det               = NULL,
    lucro             = NULL,
    recursos          = NULL,
    custoW            = NULL,
    custoV            = NULL,
    custoStock        = NULL,
    pareto_recursos   = NULL,
    pareto_lucros     = NULL,
    pareto_tab        = NULL,
    msg               = ""
  )
  
  # 3) Tabela de previsões
  output$tabPrev <- renderDT({
    datatable(pred_mat(),
              caption = "Previsões de vendas (7 dias × 6 produtos)",
              rownames = FALSE, options = list(dom = 't'))
  })
  
  # 4) Mensagem de estado
  output$msg <- renderText(vals$msg)
  
  # 5) Dispara otimização ao clicar
  observeEvent(input$run, {
    # 1) Semana e algoritmo em formato legível
    semana_alvo <- as.integer(input$semana)
    display_alg <- switch(input$alg,
                          MC    = "Monte Carlo",
                          PSO   = "PSO",
                          HC    = "Hill Climbing",
                          SPEA2 = "SPEA-2",
                          HCP    = "Hill Climbing com Pesos",
                          SA    = "Simmulated Annealing",
                          SAW   = "Simmulated Annealing Penalizada",
                          DE = "Differential Evolution ",
                          NSGA2 = "NSGA-II")
    
    # 2) Mensagem imediata
    vals$msg <- paste0("A executar ", display_alg,
                       " para a semana ", semana_alvo, "… aguarde.")
    
    # 3) Seed conforme algoritmo
    if (input$alg == "PSO") {
  set.seed(123)
} else if (input$alg == "MC") {
  set.seed(12345)
} else if (input$alg == "HCP") {
  set.seed(54321)
} else if (input$alg == "HC") {
  set.seed(45)
} else if (input$alg == "DE") {
  set.seed(12345)
} else {  # SPEA-2 & NSGA-2
  set.seed(42)
}
    
    # 4) Dados dessa semana
    wk_vec <- get_weekend_vec(semana_alvo)
    pred   <- pred_list[[semana_alvo]]
    
    # 5) Rama SPEA-2 (corre só nessa semana)
    if (input$alg == "SPEA2") {
      spea2_res <- optimize_spea2(
        pred_sales  = pred,
        weekend_vec = wk_vec,
        n.pop  = 100L, n.arch = 100L, n.gen = 49L
      )
      vals$pareto_recursos <- spea2_res$recursos
      vals$pareto_lucros   <- spea2_res$lucros
      vals$pareto_tab <- data.frame(
        Recursos = head(vals$pareto_recursos, 10),
        Lucro    = head(vals$pareto_lucros,   10)
      )
      # 5.a) limpar KPIs antigos
      vals$lucro    <- NULL
      vals$recursos <- NULL
      
      vals$msg <- paste0(display_alg,
                         " concluído para a semana ", semana_alvo)
      return()
    }
    
    # 6) Rama PSO / Monte Carlo: “queimar” semanas até semana_alvo
    sel_res <- NULL
    wk     <- get_weekend_vec(semana_alvo)
    pr     <- pred_list[[semana_alvo]]
    if (input$alg == "PSO") {
      sel_res <- optimize_pso(pr, wk,
                                  max_iters   = 10, swarm_size = 50,
                                  inertia     = 0.5, cognitive_c = 1, social_c = 2)
    } 
    
    else {
        sel_res <- optimize_mc(pr, wk, N = 500)
    } 
    
    # Rama Hill Climbing simples
    detHC <- NULL
    if (input$alg == "HC") {
      hillc_res <- optimize_hillClimbing(
        pred_sales  = pred,
        weekend_vec = wk_vec,
        max_iters   = 500
      )
      detHC <- hillc_res$detalhes
    }
    
    # Rama Hill Climbing com Pesos
    if (input$alg == "HCP") {
      hill_res <- optimize_hill(
        pred_sales  = pred,
        weekend_vec = wk_vec
      )
      vals$pareto_tab      <- hill_res
      vals$pareto_recursos <- hill_res$Recursos
      vals$pareto_lucros   <- hill_res$Lucro
      vals$lucro    <- NULL; vals$recursos <- NULL
      vals$msg <- paste0(display_alg, " concluído.")
      return()
    }
    
    # Rama Genetic Algorithm simples (GA)
    if (input$alg == "GA") {
      ga_simple <- optimize_ga1(
        pred_sales  = pred,
        weekend_vec = wk_vec
      )
      
      # Verificar se a função retorna os dados corretamente
      if (!is.null(ga_simple) && is.list(ga_simple)) {
        if (!is.null(ga_simple$detalhes) && is.list(ga_simple$detalhes)) {
          vals$det <- as.data.frame(ga_simple$detalhes)
        } else {
          vals$msg <- "Erro: 'detalhes' não contém um data.frame válido."
        }
        
        
        if (!is.null(ga_simple$lucro) && is.numeric(ga_simple$lucro)) {
          vals$lucro <- round(ga_simple$lucro, 2)
        } else {
          vals$msg <- "Erro: 'lucro' não contém um valor numérico válido."
        }
        
        if (!is.null(ga_simple$recursos) && is.numeric(ga_simple$recursos)) {
          vals$recursos <- ga_simple$recursos
        } else {
          vals$msg <- "Erro: 'recursos' não contém um valor numérico válido."
        }
      } else {
        vals$msg <- "Erro: A função optimize_ga1 não retornou um objeto válido."
      }
      
      vals$msg <- paste0("Genetic Algorithm concluído para a semana ", semana_alvo)
      if (!is.null(vals$det) && is.data.frame(vals$det)) {
        print("Dados GA corretamente carregados!")
      } else {
        vals$msg <- "Erro: Detalhes do GA não foram gerados corretamente."
      }
      
      return()
    }
    
    
    # Rama Genetic Algorithm com Pesos → gera frente de Pareto
    if (input$alg == "GAP") {
      ga_weights <- optimize_ga2(
        pred_sales  = pred,
        weekend_vec = wk_vec
      )
      # supondo que optimize_ga2 devolve um data.frame com colunas
      #   Lucro, Recursos e Alpha
      vals$pareto_tab      <- ga_weights
      vals$pareto_recursos <- ga_weights$Recursos
      vals$pareto_lucros   <- ga_weights$Lucro
      # limpa KPIs individuais
      vals$lucro    <- NULL
      vals$recursos <- NULL
      vals$msg      <- paste0(display_alg, " concluído para a semana ", semana_alvo)
      return()
    }
    
    # Simulated Annealing
    if (input$alg == "SA") {
      sa_res <- optimize_sa(
        pred_sales   = pred,
        weekend_vec  = wk_vec,
        max.call     = 5000
      )
      det <- sa_res$detalhes
      vals$det        <- det
      vals$lucro      <- round(sa_res$lucro, 2)
      vals$recursos   <- sum(det$W) + sum(det$Veic1 + det$Veic2 + det$Veic3)
      vals$custoW     <- sum(det$CustoW)
      vals$custoV     <- sum(det$CustoV)
      vals$custoStock <- sum(det$CustoStock)
      vals$msg <- paste0(display_alg, " concluído.")
      return()
    }
    
    # SA Weighted (Penalizada)
    if (input$alg == "SAW") {
      sa_weights <- optimize_sa2(pred_sales = pred)
      
      vals$pareto_tab      <- sa_weights
      vals$pareto_recursos <- sa_weights$Recursos
      vals$pareto_lucros   <- sa_weights$Lucro
      vals$lucro           <- NULL
      vals$recursos        <- NULL
      vals$msg             <- paste0("Simulated Annealing com Pesos concluído para a semana ", semana_alvo)
      return()
    }
    
    # Differential Evolution
    if (input$alg == "DE") {
      de_res <- optimize_de(pred, wk_vec, NP = 50L, itermax = 100L)
      vals$det        <- de_res$detalhes
      vals$lucro      <- round(de_res$lucro, 2)
      # soma de recursos = armazém + veículos
      vals$recursos   <- de_res$recursos
      vals$custoW     <- sum(de_res$detalhes$CustoW)
      vals$custoV     <- sum(de_res$detalhes$CustoV)
      vals$custoStock <- sum(de_res$detalhes$CustoStock)
      vals$msg        <- paste0(display_alg, " concluído para a semana ", semana_alvo)
      return()
    }
    
    # 5.b) Rama NSGA-II (corre só nessa semana)
    if (input$alg == "NSGA2") {
      display_alg <- "NSGA-II"
      
      # corre NSGA-II
      nsga2_res <- optimize_nsga2(
        pred_sales  = pred,
        weekend_vec = wk_vec,
        n.pop       = 100L,
        n.gen       = 49L
      )
      
      # guarda resultados no reactiveValues
      vals$pareto_recursos <- nsga2_res$recursos
      vals$pareto_lucros   <- nsga2_res$lucros
      vals$pareto_tab <- data.frame(
        Recursos = head(vals$pareto_recursos, 10),
        Lucro    = head(vals$pareto_lucros,   10)
      )
      
      # limpar KPIs antigos
      vals$lucro    <- NULL
      vals$recursos <- NULL
      
      # mensagem de estado
      vals$msg <- paste0(display_alg, " concluído para a semana ", semana_alvo)
      return()
    }
    
    
    
    #  Actualizar KPIs PSO/MC
    det <- sel_res$detalhes
    vals$det        <- det
    vals$lucro      <- round(sel_res$lucro, 2)
    vals$recursos   <- sum(det$W) + sum(det$Veic1 + det$Veic2 + det$Veic3)
    vals$custoW     <- sum(det$CustoW)
    vals$custoV     <- sum(det$CustoV)
    vals$custoStock <- sum(det$CustoStock)
    
    vals$msg <- paste0(display_alg,
                       " concluído para a semana ", semana_alvo)
  })
  
  # 6) KPIs
  output$kpiLucro    <- renderText({ req(vals$lucro);    paste0("Lucro total: ",    vals$lucro, " EUR") })
  output$kpiRecursos <- renderText({ req(vals$recursos); paste0("Recursos totais: ", vals$recursos) })
  
  # 7) Renderizações PSO/MC (Armazém, Veículos, Dist, Ind)
  output$tabArmazem <- renderDT({
    datatable(vals$det[, c("Dia","W")], caption = "Armazém (por dia)", rownames = FALSE, options = list(dom = 't'))
  })
  output$custoArmazem <- renderText({ paste0("Custo total de armazém: ", vals$custoW, " EUR") })
  output$tabVeic <- renderDT({
    datatable(vals$det[, c("Dia","Veic1","Veic2","Veic3")], caption = "Veículos (por dia)", rownames = FALSE, options = list(dom = 't'))
  })
  output$custoVeic <- renderText({ paste0("Custo total de veículos: ", vals$custoV, " EUR") })
  output$tabDist <- renderDT({
    datatable(vals$det[, c("Dia", paste0("Dist",1:6))], caption = "Distribuição (por dia)", rownames = FALSE, options = list(dom = 't'))
  })
  output$custoDist <- renderText({ paste0("Custo total de stock: ", vals$custoStock, " EUR") })
  output$tabInd <- renderDT({
    datatable(vals$det[, c("Dia","VendasPot","VendasReais","CustoW","CustoV","Receita","LucroDia")],
              caption = "Indicadores (por dia)", rownames = FALSE, options = list(dom = 't'))
  })
  
  # 8) UI condicional para 'Planeamento'
  output$planeamentoUI <- renderUI({
    req(input$run)
    if (input$alg %in% c("SPEA2", "HCP", "GAP", "NSGA2", "SAW")) {
      tagList(
        plotOutput("paretoPlot"),
        DTOutput("paretoTab")
      )
    } else {
      tagList(
        h3("Recursos — Armazém"), DTOutput("tabArmazem"), verbatimTextOutput("custoArmazem"), tags$hr(),
        h3("Recursos — Veículos"), DTOutput("tabVeic"),   verbatimTextOutput("custoVeic"),   tags$hr(),
        h3("Distribuição"),            DTOutput("tabDist"),  verbatimTextOutput("custoDist"),   tags$hr(),
        h3("Indicadores Diários"),      DTOutput("tabInd")
      )
    }
  })
  
  # 9) Outputs SPEA-2
  output$paretoPlot <- renderPlot({
    req(vals$pareto_recursos, vals$pareto_lucros)
    main_title <- if (input$alg == "SPEA2") {
      "Curva de Pareto — SPEA-2"
    } else if (input$alg == "HCP") {
      "Curva de Pareto — Hill Climbing com Pesos"
    } else if (input$alg == "GAP") {
      "Curva de Pareto — Genetic Algorithm com Pesos"
    } else if (input$alg == "NSGA2"){
      "Curva de Pareto - NSGA-II"
    } else if (input$alg == "SAW"){
      "Curva de Pareto - Simulated Annealing Penalizada"
    }
    plot(vals$pareto_recursos, vals$pareto_lucros,
         type = "b", pch = 16,
         xlab = "Uso de Recursos", ylab = "Lucro (EUR)",
         main = main_title)
  })
  
  output$paretoTab <- renderDT({
    req(vals$pareto_tab)
    caption_text <- if (input$alg == "SPEA2") {
      "Top 10 com menor número de recursos — SPEA-2"
    } else if (input$alg == "HCP") {
      "Top 10 com menor número de recursos — Hill Climbing com Pesos"
    } else if (input$alg == "NSGA2") {
      "Top 10 com menor número de recursos - NSGA-II"
    } else if (input$alg == "SAW"){
      "Top 10 com menor número de recursos - - Simulated Annealing Penalizada"
    }
    datatable(head(vals$pareto_tab[order(vals$pareto_tab$Recursos), ], 10),
              caption = caption_text,
              rownames = FALSE, options = list(dom = 't'))
  })
}

shinyApp(ui, server)