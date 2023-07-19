#Autor: Italo López
#Departamento de Estudios Económicos, Superintendencia de Bancos de la República Dominicana
#Fecha;: Julio 2023
#Título: Prediciendo series de Tiempo de Variables Económicas Dominicanas
#Variable: Remesas

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


# Esta variable controla si se ejecuta la parte del código que realiza el proceso con la data sintética
cross_validate_with_synth_data = FALSE


#Cargando funciones
source("funciones.R")
#Para prevenir notación científica
options(scipen = 999)

#Cargando data
remesas <- read.csv("remesas.csv")
remesas <-remesas%>%select(REMESAS)%>%ts(start = c(2010,1),frequency = 12)
remesas <-remesas%>%as.zoo()

#Plot de las remesas en nivel
fig <- plot_ly(y=remesas[,1],x=index(remesas), type = 'scatter', mode = 'lines')%>%
  layout(title="Remesas RD")
fig

#Número de observaciones
nrow(remesas)

#Training set
remesas_training <- window(remesas,end=tail(index(remesas),60)[1])


#Tests de estacionaridad en diferencia estacional
adf.test(diff(remesas_training,12))
pp.test(diff(remesas_training,12))
kpss.test(diff(remesas_training,12))

#Tests de estacionaridad en diferencia regular
adf.test(diff(remesas_training,1))
pp.test(diff(remesas_training,1))
kpss.test(diff(remesas_training,1))

#Tests de estacionaridad en variación interanual
adf.test(diff(log(remesas_training,12)))
pp.test(diff(log(remesas_training,12)))
kpss.test(diff(log(remesas_training,12)))


#ACF y PACF de la diferencia regular
acf(diff(remesas_training[,1]))
pacf(diff(remesas_training[,1]))


#Calibración y diagnosis del Box-Jenkins SARIMA
fit_arima_diag(remesas_training[,1],reg_order=c(0,1,1),seas_order=c(0,1,1))

#Time series cross-validation del Box-Jenkins SARIMA
remesas_sarima_table <- sarima_table_function(model_data=remesas,l_test_set=60,forecast_horizon=6,reg_order=c(0,1,1),seas_order=c(0,1,1))


#Tests de optimalidad para el SARIMA
opt_tests_function_single(remesas_sarima_table)

sd_hit_single(remesas_sarima_table,"one")

#Parámetros de la distribución de los errores para el SARIMA
sapply(remesas_sarima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#ACF y PACF de la diferencia estacional
acf(diff(remesas_training[,1],12))
pacf(diff(remesas_training[,1],12))

#Time series cross-validation del Minimum AIC ETS con los tres componentes
remesas_ets_table <- ets_table_function(model_data=remesas,l_test_set=60,forecast_horizon=6,"AAA")

#Proporción del hit de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(remesas_ets_table,"one")

#Tests de optimalidad
opt_tests_function_single(remesas_ets_table)

#Parámetros de la distribución de los errores para el Minimum AIC ETS
sapply(remesas_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Identificación del mejor ARIMA
arimas <- best_arima_identification(remesas_training[,1])

#Best ARIMA is ARIMA (1,2,1)
#Orden del mejor ARIMA
arimas[[2]][[arimas[[1]]%>%which.min()]]

#Time series cross-validation del Grid/Best ARIMA
best_arima_table <- sarima_table_function(model_data=remesas,l_test_set=60,forecast_horizon=6,reg_order=c(2,2,5),seas_order=c(0,0,0))

#Proporción de hit de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(best_arima_table,"one")


opt_tests_function_single(best_arima_table)

#Parámetros de la distribución de los errores para el Grid/Best ARIMA
sapply(best_arima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Identificación del Grid/Best ETS
etss <- best_ets_identification(remesas_training[,1])

#Best ETS identificado
etss[[2]][[etss[[1]]%>%which.min()]]

#Time series cross-validation del Grid/Best ETS
remesas_best_ets_table <- ets_table_function_fixed(model_data=remesas,l_test_set=60,forecast_horizon=6,alpha=0.2,beta=0.1,gamma=0.1)

#Esta función calcula la proporción del hit de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(remesas_best_ets_table,"one")

#Tests de optimalidad
opt_tests_function_single(remesas_best_ets_table)

#Parámetros de la distribución de los errores para Grid/Best ETS
sapply(remesas_best_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


remesas_sarima_table$model <- "SARIMA"
remesas_ets_table$model <- "ETS"
best_arima_table$model <- "Best ARIMA"
remesas_best_ets_table$model <- "Best ETS"

classic_models_remesas <- rbind(remesas_sarima_table,remesas_ets_table,best_arima_table,remesas_best_ets_table)


#Plots de los errores de los modelos individuales
classic_models_remesas_plots <- list()
for (i in 1:sum(lengths(classic_models_remesas["model"]%>%unique()))){
  classic_models_remesas_plots[[i]] <- classic_models_remesas %>% filter((model == classic_models_remesas["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=classic_models_remesas["model"]%>%unique()%>%slice(i)%>%as.character()
    )
}




for (i in 1:sum(lengths(classic_models_remesas["model"]%>%unique()))){
  print(classic_models_remesas_plots[[i]])
}

#Plot de las predicciones y el valor realizado para los modelos individuales
step_plots <- list()
n_steps <- 6
for (i in 1:n_steps){
  step_plots[[i]] <- classic_models_remesas%>%filter(step==paste("step",i%>%as.character(),sep = " "))%>%
    group_by(model)%>%
    plot_ly(x = ~date,y=~predicted, type = 'scatter',mode="marker",
            color=~factor(model)) %>% 
    layout(title = paste("step",i%>%as.character(),sep = " "))%>%
    add_lines(x =~date,y=~realized_value,name="Realized Value",color=I("red"))
  
}


for (i in 1:n_steps){
  step_plots[[i]]%>%print()
}

#Time series cross-validation para el average forecast combination
avg_forecast_combination_table <- avg_forecast_combination_function_real(classic_models_remesas)

#Proporción del hit de las bandas para una, dos y tres desviaciones estándar
sd_hit_single(avg_forecast_combination_table,"one")

avg_forecast_combination_table$model <- "Average"

#Tests de optimalidad
opt_tests_function_single(avg_forecast_combination_table)

#Parámetros de la distribución de los errores del average forecast combination
sapply(avg_forecast_combination_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Time series cross-validation para el regression forecast combination
regression_forecast_combination_table <- regression_forecast_combination_complete(classic_models_remesas,30)
weights <- regression_forecast_combination_table[[2]]
regression_forecast_combination_table <- regression_forecast_combination_table[[1]] 

#Parámetros de la distribución de los errores para el regression forecast combination
sapply(regression_forecast_combination_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

regression_forecast_combination_table[regression_forecast_combination_table["step"]!="in_sample","train_test"] <- "test"
regression_forecast_combination_table[regression_forecast_combination_table["step"]=="in_sample","train_test"] <- "train"

#Agregando desviaciones estándar y hit de las bandas para el regression forecast combination
regression_forecast_combination_table <- hits_and_sd(regression_forecast_combination_table)

regression_forecast_combination_table$model <- "Regression Forecast"

#Proporción de los hit de las bandas a una, dos y tres desviaciones estándar 
sd_hit_single(regression_forecast_combination_table,"one")

#Tests de optimalidad
opt_test_function_combination(regression_forecast_combination_table)



all_models_real <- rbind(classic_models_remesas,avg_forecast_combination_table,
                         regression_forecast_combination_table)

#Plot de los errores de todos los modelos
all_models_real_plots <- list()
for (i in 1:sum(lengths(all_models_real["model"]%>%unique()))){
  all_models_real_plots[[i]] <- all_models_real %>% filter((model == all_models_real["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=all_models_real["model"]%>%unique()%>%slice(i)%>%as.character()
    )
}



for (i in 1:sum(lengths(all_models_real["model"]%>%unique()))){
  print(all_models_real_plots[[i]])
}




#Plot de las predicciones de todos los modelos y el valor realizado
step_plots <- list()
n_steps <- 6
for (i in 1:n_steps){
  step_plots[[i]] <- all_models_real%>%filter(step==paste("step",i%>%as.character(),sep = " "))%>%
    group_by(model)%>%
    plot_ly(x = ~date,y=~predicted, type = 'scatter',mode="marker",
            color=~factor(model)) %>% 
    layout(title = paste("step",i%>%as.character(),sep = " "))%>%
    add_lines(x =~date,y=~realized_value,name="Realized Value",color=I("red"))
  
}

#Plots 
for (i in 1:n_steps){
  step_plots[[i]]%>%print()
}


#En esta parte del código trabajaremos con la data sintética
#El usuario debe tomar en cuenta que esta parte del código puede tomar
#Una cantidad considerable de tiempo para correr
if (cross_validate_with_synth_data) {
  diff(remesas)%>%nrow()
  diff(remesas)%>%nrow()%>%divisors()
  
  
  #Función que genera la data sintética
  
  synthetic_data <- synthetic_data_function(diff(remesas),2,1000,13,remesas)
  
  
  #Time series cross-validation para el SARIMA con la data sintética
  tic()
  sarima_model_synthetic_table <- synthetic_model_table_function(final_synthetic_data =final_synthetic_data,
                                                                 sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(0,1,1),seas_order=c(0,1,1))
  toc()
  
  
  #Formateo de los resultados anteriores
  tic()
  model_synthetic_df_sarima <- data_frame_synthetic_conversion_function(remesas_sarima_table,
                                                                        sarima_model_synthetic_table,1000)
  toc()
  
  
  #Parámetros de la distribución de los errores para el SARIMA con la data sintética
  sapply(model_synthetic_df_sarima%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})
  
  #Proporción del hit de las bandas a una, dos y tres desviaciones estándar con la data sintética
  sd_hit_single(model_synthetic_df_sarima,"one")
  
  #Esta función hace el time series cross-validation para el Minimum AIC ETS
  tic()
  ets_model_synthetic_table <- synthetic_model_table_function(
    final_synthetic_data =final_synthetic_data,
    ets_table_function,l_test_set=60,forecast_horizon=6)
  toc()
  
  
  #Formateo de los resultados anteriores
  tic()
  model_synthetic_df_ets <- data_frame_synthetic_conversion_function(
    remesas_ets_table,
    ets_model_synthetic_table,1000)
  toc()
  
  #Parámetros de la distribución de los errores del ETS con la data sintética
  sapply(model_synthetic_df_ets%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})
  
  #Proporción del hit de las bandas con la data sintética a una, dos y tres desviaciones estándar
  sd_hit_single(model_synthetic_df_ets,"one")
  
  #Time series cross-validation para el Grid/Best ARIMA con la data sintética
  tic()
  best_arima_model_synthetic_table <- synthetic_model_table_function(final_synthetic_data =final_synthetic_data,
                                                                     sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(2,2,5),seas_order=c(0,0,0))
  toc()
  
  
  #Formateo de los resultados anteriores
  tic()
  model_synthetic_df_best_arima <- data_frame_synthetic_conversion_function(
    best_arima_table,
    best_arima_model_synthetic_table,1000)
  toc()
  
  #Parámetros de la distribución de los errores para el Grid/Best ARIMA con la data sintética
  sapply(model_synthetic_df_best_arima%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})
  
  #Proporción del hit de las bandas a una, dos y tres desviaciones estándar con la data sintética
  sd_hit_single(model_synthetic_df_best_arima,"one")
  
  #Time series cross-validation del Grid/Best ETS con la data sintética
  tic()
  best_ets_model_synthetic_table <- synthetic_model_table_function(final_synthetic_data =final_synthetic_data,
                                                                   ets_table_function_fixed,l_test_set=60,forecast_horizon=6,alpha=0.2,beta=0.1,gamma=0.1)
  toc()
  
  
  #Formateando los resultados anteriores
  tic()
  model_synthetic_df_best_ets <- data_frame_synthetic_conversion_function(
    remesas_best_ets_table,
    best_ets_model_synthetic_table,1000)
  toc()
  
  #Parámetros de la distribución de los errores del Grid/Best ETS con la data sintética
  sapply(model_synthetic_df_best_ets%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})
  
  #Proporción del hit de las bandas a una, dos y tres desviaciones estándar
  sd_hit_single(model_synthetic_df_best_ets,"one")
  
  model_synthetic_df_sarima$model <- "SARIMA"
  model_synthetic_df_ets$model <- "ETS"
  model_synthetic_df_best_arima$model <- "Best ARIMA"
  model_synthetic_df_best_ets$model <- "Best ETS"
  
  classic_models_remesas_synthetic <- rbind(model_synthetic_df_sarima,model_synthetic_df_ets,
                                            model_synthetic_df_best_arima,model_synthetic_df_best_ets)
  
  #Time series cross-validation del average forecast combination con la data sintética
  tic()
  avg_forecast_combination_table_synthetic <- avg_forecast_combination_function_synthetic(classic_models_remesas_synthetic)
  toc()
  
  #Parámetros de la  distribución de los errores del average forecast combination con la data sintética
  sapply(avg_forecast_combination_table_synthetic%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})
  
  #Proporción del hit de las bandas para una, dos y tres desviaciones estándar con la data sintética para el average forecast combination
  sd_hit_single(avg_forecast_combination_table_synthetic,"one")
  
  
  #Time series cross-validation del regression forecast combination con la data sintética
  tic()
  regression_forecast_combination_synthetic_table <- regression_forecast_combination_complete_synthetic(classic_models_remesas_synthetic,
                                                                                                        30)
  toc()
  weights <- regression_forecast_combination_synthetic_table[[2]]
  regression_forecast_combination_synthetic_table <- regression_forecast_combination_synthetic_table[[1]]
  
  #Agregando el hit de las bandas y la desviación estándar al data frame
  regression_forecast_combination_synthetic_table <- hits_and_sd_synthetic(regression_forecast_combination_synthetic_table)
  
  regression_forecast_combination_synthetic_table$model <- "Regression Forecast"
  
  #Parámetros de la distribución de los errores para el regression forecast combination con la data sintética
  sapply(regression_forecast_combination_synthetic_table%>%filter(step!="in_sample")%>%select(error)%>%drop_na(),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})
  
  #Proporción del hit de las bandas para una, dos y tres desviaciones estándar para el regression forecast combination con la data sintética
  sd_hit_single(regression_forecast_combination_synthetic_table,"one")
  
  
  regression_forecast_combination_synthetic_table[regression_forecast_combination_synthetic_table["step"]!="in_sample","train_test"] <- "test"
  regression_forecast_combination_synthetic_table[regression_forecast_combination_synthetic_table["step"]=="in_sample","train_test"] <- "train"
  
  
  synthetic_models <- rbind(classic_models_remesas_synthetic,
                            avg_forecast_combination_table_synthetic,
                            regression_forecast_combination_synthetic_table)
  #Aquí terminamos con la data sintética
}

#ACF del hit de las bandas del step 1 para dos desviaciones estándar
best_arima_table%>%filter(step=="step 1")%>%
  select(hit_two)%>%filter(hit_two!="NA")%>%acf()

#Test Kolmogorov-Smirnoff del hit de las bandas del step 1 para dos desviaciones estándar
#con H0 que la misma se distribuye Bernoulli con parámetro de éxito 95%
ks.test(best_arima_table%>%filter(step=="step 1")%>%
          select(hit_two)%>%filter(hit_two!="NA"),c(rep(1,950),rep(0,50)))


#Plot de las predicciones de todos los modelos y valor realizado en una misma hoja
fig_mult <- subplot(step_plots[[1]],
                    step_plots[[2]],
                    step_plots[[3]],
                    step_plots[[4]],
                    step_plots[[5]],
                    step_plots[[6]],
                    nrows=3)%>%
  layout(title="Steps")
annotations <- list(list(x = 0.2,  
                         y = 1.0, text="Step 1", xref = "paper",  
                         yref = "paper",  
                         xanchor = "center",  
                         yanchor = "bottom",  
                         showarrow = FALSE),list(x = 0.75,  
                                                 y = 1.0, xref = "paper",  
                                                 yref = "paper",  
                                                 xanchor = "center",  
                                                 yanchor = "bottom", text="Step 2",  
                                                 showarrow = FALSE),
                    list(x = 0.2,  
                         y = 0.60, xref = "paper",  
                         yref = "paper",  
                         xanchor = "center",  
                         yanchor = "bottom", text="Step 3",  
                         showarrow = FALSE),list(x = 0.75,  
                                                 y = 0.60, xref = "paper",  
                                                 yref = "paper",  
                                                 xanchor = "center",  
                                                 yanchor = "bottom", text="Step 4",  
                                                 showarrow = FALSE),
                    list(x = 0.2,  
                         y = 0.25, xref = "paper",  
                         yref = "paper",  
                         xanchor = "center",  
                         yanchor = "bottom", text="Step 5",  
                         showarrow = FALSE),list(x = 0.75,  
                                                 y = 0.25, xref = "paper",  
                                                 yref = "paper",  
                                                 xanchor = "center",  
                                                 yanchor = "bottom", text="Step 6",  
                                                 showarrow = FALSE))
fig_mult <- fig_mult %>%layout(annotations = annotations)
fig_mult
