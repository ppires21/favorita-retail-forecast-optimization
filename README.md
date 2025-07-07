# Favorita Retail Demand Forecast & Optimization

AI-driven demand forecasting and logistics optimization system for Favorita Stores retail chain.

## Table of Contents

- [About](#about)  
- [Features](#features)  
- [Methodology](#methodology)  
- [Project Structure](#project-structure)  
- [Getting Started](#getting-started)  
  - [Prerequisites](#prerequisites)  
  - [Installation](#installation)  
  - [Usage](#usage)  
- [Results](#results)  
- [Contributors](#contributors)  
- [License](#license)

## About

This project implements a decision support system to:

1. **Forecast** daily sales for dairy, eggs, and bakery categories in two store locations using multiple time-series and machine learning models.  
2. **Optimize** weekly allocation of warehouse and transport resources to maximize profit and minimize logistical effort.

It was developed as part of the “Artificial Intelligence Techniques in Business Systems” course at Universidade do Minho.

## Features

- **Forecasting Models**  
  - XGBoost (univariate & multivariate)  
  - Holt-Winters  
  - ARIMA / ARIMAX  
  - Neural Networks  
  - Random Forest  

- **Optimization Algorithms**  
  - Genetic Algorithm  
  - Particle Swarm Optimization (PSO)  
  - Simulated Annealing  
  - Hill Climbing  
  - Differential Evolution  
  - NSGA-II & SPEA-II (multi-objective)  

- **Interactive Dashboard**  
  - R Shiny interface to visualize forecasts and optimization plans  
  - Comparative plots and convergence analysis

## Methodology

We followed the **CRISP-DM** framework:

1. **Business Understanding**  
2. **Data Understanding & Preparation**  
3. **Modeling (Forecasting & Optimization)**  
4. **Evaluation**  
5. **Deployment / Interface**

## Getting Started

### Prerequisites

- R (>= 4.0)  
- RStudio (optional)  
- R packages: `forecast`, `xgboost`, `randomForest`, `rminer`, `shiny`, `zoo`, etc.

### Installation

1. Clone the repo:  
   ```bash
   git clone https://github.com/teu-utilizador/favorita-retail-forecast-optimization.git
   cd favorita-retail-forecast-optimization
   ```
2. Install required R packages (in R/RStudio):  
   ```r
   install.packages(c("forecast","xgboost","randomForest","rminer","shiny","zoo"))
   ```

### Usage

- **Forecasting**  
  - Run forecasting scripts in `/forecasting/` to generate predictions and error metrics.
- **Optimization**  
  - Execute scripts in `/optimization/` to compute optimal resource plans.
- **Dashboard**  
  ```bash
  Rscript -e "shiny::runApp('interface/')"
  ```

## Results

- **Forecasting** surpassed the SNaïve baseline, achieving NMAE reductions up to 70%.  
- **Average weekly profit improvement**: **21.87%** over baseline.  
- **Optimized logistics** reduced total transport and warehouse usage while meeting demand.

## Contributors

- **Pedro Pires**  
- **Lucas Pereira**  
- **Miguel Miranda**  
- **João Afonso**  
- **José Guilherme**

