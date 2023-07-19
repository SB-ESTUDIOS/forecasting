#Autor: Italo López
#Departamento de Estudios Económicos, Superintendencia de Bancos de la República Dominicana
#Fecha;: Julio 2023
#Título: Prediciendo series de Tiempo de Variables Económicas Dominicanas
#Variable: Crédito (Interanual)

library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(plotly)
library(tibble)
library(pracma)
library(forecast)
library(tseries)
library(dplyr)
library(numbers)
library(tictoc)
library(tidyverse)
library(timetk)

# Esta variable controla si se ejecuta la parte del código que realiza el proceso con la data sintética
cross_validate_with_synth_data = FALSE

source("funciones.R")
#Para prevenir notación científica
options(scipen = 999)
#Leyendo la data
credito <- read.csv("credito.csv")
credito <-credito%>%select(DEUDA_TOTAL)%>%ts(start = c(2007,1),frequency = 12)
credito <-credito%>%as.zoo()


#Plot de la serie en nivel
fig <- plot_ly(y=credito[,1],x=index(credito), type = 'scatter', mode = 'lines')%>%
  layout(title="Cartera de Crédito Privada MN")
fig

#Plot de la serie en variación interanual
fig <- plot_ly(y=diff(log(credito[,1]),12)*100,x=index(diff(log(credito[,1]),12)), type = 'scatter', mode = 'lines')%>%
  layout(title="Cartera de Crédito Privada MN")
fig

#Variable en variación interanual
credito_variacion <- diff(log(credito[,1]),12)*100

#Training set en variación interanual
credito_variacion_training <- window(credito_variacion,end=tail(index(credito_variacion),60)[1])

#Número de obervaciones en el training set
length(credito_variacion_training)

#Tests de estacionaridad
adf.test(credito_variacion_training)
pp.test(credito_variacion_training)
kpss.test(credito_variacion_training)

adf.test(diff(credito_variacion_training))
pp.test(diff(credito_variacion_training))
kpss.test(diff(credito_variacion_training))

#ACF y PACF
acf(diff(credito_variacion_training))
pacf(diff(credito_variacion_training))

#Calibración y diagnosis para el SARIMA
fit_arima_diag(credito_variacion_training,reg_order=c(1,1,1),seas_order=c(0,0,0))#1,2,1


#Time series cross-validation para el SARIMA
credito_sarima_table <- sarima_table_function(model_data=credito_variacion,l_test_set=60,forecast_horizon=6,reg_order=c(1,1,1),seas_order=c(0,0,0))

#Tests de optimalidad
opt_tests_function_single(credito_sarima_table)

#Parámetros de la distribución de los errores
sapply(credito_sarima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(credito_sarima_table,"two")

#Calibración y time series cross-validation para el ETS
credito_ets_table <- ets_table_function(model_data=credito_variacion,l_test_set=60,forecast_horizon=6,"NNA")

sd_hit_single(credito_ets_table,"one")


opt_tests_function_single(credito_ets_table)


sapply(credito_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#Función que identifica el Grid/Best ARIMA
arimas <- best_arima_identification(credito_variacion_training,30,6)

#Orden del mejor ARIMA
arimas[[2]][[arimas[[1]]%>%which.min()]]


credito_best_arima_table <- sarima_table_function(model_data=credito_variacion,l_test_set=60,forecast_horizon=6,reg_order=c(0,1,1),seas_order=c(0,0,0))

sd_hit_single(credito_best_arima_table,"one")


opt_tests_function_single(credito_best_arima_table)

sapply(credito_best_arima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


etss <- best_ets_identification(credito_variacion_training)

etss[[2]][[etss[[1]]%>%which.min()]]


credito_best_ets_table <- ets_table_function_fixed(model_data=credito_variacion,l_test_set=60,forecast_horizon=6,alpha=0.3,beta=0.1,gamma=0.4)

sd_hit_single(credito_best_ets_table,"one")


opt_tests_function_single(credito_best_ets_table)

sapply(credito_best_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


credito_sarima_table$model <- "SARIMA"
credito_ets_table$model <- "ETS"
credito_best_arima_table$model <- "Best ARIMA"
credito_best_ets_table$model <- "Best ETS"

classic_models_credito <- rbind(credito_sarima_table,credito_ets_table,credito_best_arima_table,credito_best_ets_table)

#Calibración y time series cross-validation para el average
#forecast combination
avg_forecast_combination_table_credito <- avg_forecast_combination_function_real(classic_models_credito)

sd_hit_single(avg_forecast_combination_table_credito,"one")

avg_forecast_combination_table_credito$model <- "Average"

opt_tests_function_single(avg_forecast_combination_table_credito)

sapply(avg_forecast_combination_table_credito%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Calibración y time series cross-validation para 
#el regression forecast combination
regression_forecast_combination_table_credito <- regression_forecast_combination_complete(classic_models_credito,30)
weights <- regression_forecast_combination_table_credito[[2]]
regression_forecast_combination_table_credito <- regression_forecast_combination_table_credito[[1]] 

sapply(regression_forecast_combination_table_credito%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

regression_forecast_combination_table_credito[regression_forecast_combination_table_credito["step"]!="in_sample","train_test"] <- "test"
regression_forecast_combination_table_credito[regression_forecast_combination_table_credito["step"]=="in_sample","train_test"] <- "train"

#Le agrega las desviaciones estándar y los hit de las bandas
regression_forecast_combination_table_credito <- hits_and_sd(regression_forecast_combination_table_credito)

regression_forecast_combination_table_credito$model <- "Regression Forecast Combination"

#Proporción de hits de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(regression_forecast_combination_table_credito,"one")

#Tests de optimalidad
opt_test_function_combination(regression_forecast_combination_table_credito)


all_models_real_credito <- rbind(classic_models_credito,avg_forecast_combination_table_credito,
                                 regression_forecast_combination_table_credito)


