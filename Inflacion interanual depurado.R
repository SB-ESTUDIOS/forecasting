#Autor: Italo López
#Departamento de Estudios Económicos, Superintendencia de Bancos de la República Dominicana
#Fecha;: Julio 2023
#Título: Prediciendo series de Tiempo de Variables Económicas Dominicanas
#Variable: Inflación (interanual)

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
library(data.table)
library(outliers)
library(numbers)
library(tictoc)
library(tidyverse)
library(timetk)


#Cargando funciones
source("funciones.R")
#Para prevenir notación científica
options(scipen = 999)

#Cargando la series en nivel
cpi_series <- read.csv("ipc.csv",header = F)

cpi_series <- ts(cpi_series,start = c(1984,1),frequency = 12)

#Plot de la serie en nivel
fig <- plot_ly(y=cpi_series,x=index(cpi_series), type = 'scatter', mode = 'lines')%>%
  layout(title="IPC RD")
fig

#Plot de la serie en variación interanual
fig <- plot_ly(y=diff(log(cpi_series),12)*100,x=index(diff(log(cpi_series),12)), type = 'scatter', mode = 'lines')%>%
  layout(title="IPC RD")
fig

inflacion_interanual <- diff(log(cpi_series),12)*100

#Training set
inflacion_interanual_training <- window(inflacion_interanual,end=tail(index(inflacion_interanual),60)[1])

#Plot del training set
fig <- plot_ly(y=inflacion_interanual_training,x=index(inflacion_interanual_training), type = 'scatter', mode = 'lines')%>%
  layout(title="CPI")
fig
#Número de observaciones del training set
nrow(inflacion_interanual_training)

#Tests de estacionaridad de la variación interanual
adf.test(inflacion_interanual_training)
pp.test(inflacion_interanual_training)
kpss.test(inflacion_interanual_training)

#Tests de estacionaridad de la diferencia de la variación interanual
adf.test(diff(inflacion_interanual_training))
pp.test(diff(inflacion_interanual_training))
kpss.test(diff(inflacion_interanual_training))

#ACF y PACF de la diferencia de la variación interanual
acf(diff(inflacion_interanual_training))
pacf(diff(inflacion_interanual_training))


#Calibración y diagnosis del Box-Jenkins SARIMA
fit_arima_diag(inflacion_interanual_training,reg_order=c(1,1,0),seas_order=c(1,0,2))


#Time series cross-validation del SARIMA
cpi_sarima_table <- sarima_table_function(model_data=inflacion_interanual,l_test_set=60,forecast_horizon=6,reg_order=c(1,1,0),seas_order=c(1,0,2))


#Función con Tests de optimalidad
opt_tests_function_single(cpi_sarima_table)

#Función con pproporción de hit de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(cpi_sarima_table,"one")

#Parámetros de la distribución de los errores
sapply(cpi_sarima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#Time series cross-validation del Minimum AIC ETS
cpi_ets_table <- ets_table_function(model_data=inflacion_interanual,l_test_set=60,forecast_horizon=6,"NAA")

sd_hit_single(cpi_ets_table,"one")


opt_tests_function_single(cpi_ets_table)

sapply(cpi_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#Función para identificar el Grid/Best ARIMA
arimas <- best_arima_identification(inflacion_interanual_training)

#Grid/Best ARIMA identificado
arimas[[2]][[arimas[[1]]%>%which.min()]]


#Time series cross-validation del Grid/Best ARIMA
best_arima_table <- sarima_table_function(model_data=inflacion_interanual,l_test_set=60,forecast_horizon=6,reg_order=c(1,0,0),seas_order=c(0,0,0))

sd_hit_single(best_arima_table,"one")


opt_tests_function_single(best_arima_table)


sapply(best_arima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Función que identifica el Grid/Best ETS
etss <- best_ets_identification(inflacion_interanual_training)


etss[[2]][[etss[[1]]%>%which.min()]]



cpi_best_ets_table <- ets_table_function_fixed(model_data=inflacion_interanual,l_test_set=60,forecast_horizon=6,alpha=0.4,beta=0.1,gamma=0.4)



sd_hit_single(cpi_best_ets_table,"one")

opt_tests_function_single(cpi_best_ets_table)

sapply(cpi_best_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

cpi_sarima_table$model <- "SARIMA"
cpi_ets_table$model <- "ETS"
best_arima_table$model <- "Best ARIMA"
cpi_best_ets_table$model <- "Best ETS"

individual_models_table <- rbind(cpi_sarima_table,cpi_ets_table,best_arima_table,cpi_best_ets_table)

#Time series cross-validation del average forecast combination
avg_forecast_combination_table <- avg_forecast_combination_function_real(individual_models_table)

sd_hit_single(avg_forecast_combination_table,"one")

opt_tests_function_single(avg_forecast_combination_table)

sapply(avg_forecast_combination_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})



avg_forecast_combination_table$model <- "Average"

individual_models_table <- rbind(individual_models_table,avg_forecast_combination_table)

# regression_forecast_combination_table <- regression_forecast_combination_function(individual_models_table,30)

#Time series cross-validation del regression forecast combination
regression_forecast_combination_table_inflacion_interanual <- regression_forecast_combination_complete(individual_models_table,30)
weights <- regression_forecast_combination_table_inflacion_interanual[[2]]
regression_forecast_combination_table_inflacion_interanual <- regression_forecast_combination_table_inflacion_interanual[[1]] 

sapply(regression_forecast_combination_table_inflacion_interanual%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

regression_forecast_combination_table_inflacion_interanual[regression_forecast_combination_table_inflacion_interanual["step"]!="in_sample","train_test"] <- "test"
regression_forecast_combination_table_inflacion_interanual[regression_forecast_combination_table_inflacion_interanual["step"]=="in_sample","train_test"] <- "train"

#Agragando la desviación estándar y el hit de las bandas al dataframe del regression forecast combination
regression_forecast_combination_table_inflacion_interanual <- hits_and_sd(regression_forecast_combination_table_inflacion_interanual)

regression_forecast_combination_table_inflacion_interanual$model <- "Regression Forecast Combination"

sd_hit_single(regression_forecast_combination_table_inflacion_interanual,"one")


#Tests de optimalidad para regression forecast combination
opt_test_function_combination(regression_forecast_combination_table_inflacion_interanual)


sapply(regression_forecast_combination_table_inflacion_interanual%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})



all_models_real_inflacion_interanual <- rbind(individual_models_table,
                                              regression_forecast_combination_table_inflacion_interanual%>%select(-Average))

bla2<-all_models_real_inflacion_interanual%>%filter(step!="in_sample")
tapply(bla2$error,bla2$model,function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})
