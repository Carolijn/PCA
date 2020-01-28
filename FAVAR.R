## Function for estimating a FAVAR model in a two step FAVAR estimation approach

# Input:
# y (timeseries format) ........ kxT matrix: k variable that are to be predicted, T observations: this matrix has to be 
#                               adjusted if we want to forecast multiple steps ahead
# Fa (timeseries format)....... mxT matrix: m Factors obtained in first step, T observations
# x_star ... pxT matrix: p seperate explanatory variables, T observations
# lagorder . lag order used in the model (default: lagorder = 1)
# NOG DOEN: EXTRA: verschllende estimation manieren?

# Output
# A list with the following components:
# estimated_model     .... estimated model
# coefficients        .... estimated regression coefficients 
# evluation_criterion ....  estimated criterion for criterion that is put in


FAVAR.estimation <- function(y, Fa, x_star, lagorder = 1, deterministic_regressors = "both") {
  #1. Preliminaries: combine data and make training and testset####
  
  fulldata <- ts.union(Fa, x_star)
  
  #2. Estimation####
          #Standard VAR estimation: OLS equation-by-equation####
          library(vars)
          estimated_model <- VAR(y,
                                 p = lagorder,
                                 type = deterministic_regressors,
                                 season = NULL,
                                 exogen = fulldata,
                                 lag.max = NULL,
                                 ic = criterion)
  #3. Generate output
  return(estimated_model)
}

## Function for making predictions in FAVAR framework 

# Input:
# y ........ kxT matrix: k variable that are to be predicted, T observations
# Fa ....... mxT matrix: m Factors obtained in first step, T observations
# x_star ... pxT matrix: p seperate explanatory variables, T observations
# lagorder . lag order used in the model (default: lagorder = 1)
# NOG DOEN: EXTRA: estimation manierkiezen! Ook erros(?)
# NOG DOEN: EXTRA: output die je krijgt kiezen

# Output
# A list with the following components:
# coefficients .... estimated regression coefficients 
# predictions  .... predictions obtained from resulting model 
# MSPE_values  .... means squared prediction error for resulting model
# residuals    .... residuals from predictions, such that they can be studies for structure
FAVAR.prediction <- function(FAVAR_model, y, Fa, x_star, evaluation_measure = "MSPE"){
  predictions <- predict(FAVAR_model, n.ahead = 1, ci = 0.95, dumvar = NULL)
}

## Function for in-sample residual analysis

# Input:
# residuals ... Tx1 vector: T residuals obtained from predictions from time-series model

# Output
# A list with the following components:
# correlation_analysis     .... plot of correlations between residuals of different lags 
# bias_analysis            .... mean of residuals 
# variance_analysis        .... means squared prediction error for resulting model
# distributional_analysis  .... residuals from predictions, such that they can be studies for structure

FAVAR.residualAnalysis <- function(FAVAR_model){
  #Auto correlation analysis
  library(vars)
  PT_asymptotic <- serial.test(FAVAR_model, lags.pt = 16, lags.bg = 5, type = "PT.adjusted")
  PT_adjusted <- serial.test(FAVAR_model, lags.pt = 16, lags.bg = 5, type = "PT.adjusted")
  BG <- serial.test(FAVAR_model, lags.pt = 16, lags.bg = 5, type = "BG")
  ES <- serial.test(FAVAR_model, lags.pt = 16, lags.bg = 5, type = "ES")
  
  nicePlotModel <- plot(FAVAR_model)
  residualPlot <-  plot(residuals(FAVAR_model))
  
  #Variance analysis
  
  #Normality tests
  distributional_analysis <- normality.test(FAVAR_model, multivariate.only = TRUE)
  }
