#Autor: Italo López
#Departamento de Estudios Económicos, Superintendencia de Bancos de la República Dominicana
#Fecha: Julio 2023
#Título: Prediciendo series de Tiempo de Variables Económicas Dominicanas
#Variable: Llegada de Turistas

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

turistas <- read.csv("turistas.csv")
turistas <-turistas%>%select(TUR_MENSUAL)%>%ts(start = c(1978,1),frequency = 12)

#Plot de la variable en nivel
fig <- plot_ly(y=turistas[,1],x=index(turistas), type = 'scatter', mode = 'lines')%>%
  layout(title="Llegada de Turistas")
fig

#training set
turistas_training <- window(turistas,end=tail(index(turistas),60)[1])

#Número de observaciones
nrow(turistas_training)

#Tests de estacionaridad
adf.test(turistas_training)
pp.test(turistas_training)
kpss.test(turistas_training)

adf.test(diff(turistas_training))
pp.test(diff(turistas_training))
kpss.test(diff(turistas_training))

adf.test(diff(turistas_training,12))
pp.test(diff(turistas_training,12))
kpss.test(diff(turistas_training,12))

#ACF y PACF en nivel
acf(turistas_training[,1])
pacf(turistas_training[,1])

#ACF y PACF en diferencias
acf(diff(turistas_training[,1]))
pacf(diff(turistas_training[,1]))

#Calibración y diagnosis de SARIMA
fit_arima_diag(turistas_training[,1],reg_order=c(1,1,1),seas_order=c(1,1,1))

 
#Time series cross-validation para el SARIMA
turistas_sarima_table <- sarima_table_function2(model_data=turistas,l_test_set=60,forecast_horizon=6,reg_order=c(1,1,1),seas_order=c(1,1,1))

#Función que realiza los tests de optimalidad
opt_tests_function_single(turistas_sarima_table)


#Parámetros de la distribución de los errores
sapply(turistas_sarima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(turistas_sarima_table,"one")

#Time series cross-validation para el ETS
turistas_ets_table <- ets_table_function(model_data=turistas,l_test_set=60,forecast_horizon=6,"AAA")

sd_hit_single(turistas_ets_table,"one")


opt_tests_function_single(turistas_ets_table)

sapply(turistas_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#Función que identifica el Grid/Best ARIMA con data escalada con factor de 1000
arimas <- best_arima_identification(turistas_training/1000,30,6)

#ARIMA con menor SSE
arimas[[2]][[arimas[[1]]%>%which.min()]]
#Best ARIMA is c(5,1,5)

turistas_best_arima_table <- sarima_table_function(model_data=turistas,l_test_set=60,forecast_horizon=6,reg_order=c(5,1,5),seas_order=c(0,0,0))


sd_hit_single(turistas_best_arima_table,"one")


opt_tests_function_single(turistas_best_arima_table)

sapply(turistas_best_arima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Función que identifica el mejor ETS
etss <- best_ets_identification(turistas_training)

etss[[2]][[etss[[1]]%>%which.min()]]

#Función que hace el time series cross-validation
#Del ETS
turistas_best_ets_table <- ets_table_function_fixed(model_data=turistas,l_test_set=60,forecast_horizon=6,alpha=0.1,beta=0.1,gamma=0.4)

sd_hit_single(turistas_best_ets_table,"one")


opt_tests_function_single(turistas_best_ets_table)

sapply(turistas_best_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})



turistas_sarima_table$model <- "SARIMA"
turistas_ets_table$model <- "ETS"
turistas_best_arima_table$model <- "Best ARIMA"
turistas_best_ets_table$model <- "Best ETS"

classic_models_turistas <- rbind(turistas_sarima_table,turistas_ets_table,turistas_best_arima_table,turistas_best_ets_table)


#Plots de los errores de los modelos individuales
classic_models_turistas_plots <- list()
for (i in 1:sum(lengths(classic_models_turistas["model"]%>%unique()))){
  classic_models_turistas_plots[[i]] <- classic_models_turistas %>% filter((model == classic_models_turistas["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=classic_models_turistas["model"]%>%unique()%>%slice(i)%>%as.character()
    )#,histnorm = "probability"
}

for (i in 1:sum(lengths(classic_models_turistas["model"]%>%unique()))){
  print(classic_models_turistas_plots[[i]])
}



#Plots de las predicciones y valor realizado
step_plots <- list()
n_steps <- 6
for (i in 1:n_steps){
  step_plots[[i]] <- classic_models_turistas%>%filter(step==paste("step",i%>%as.character(),sep = " "))%>%
    group_by(model)%>%
    plot_ly(x = ~date,y=~predicted, type = 'scatter',mode="marker",
            color=~factor(model)) %>% 
    layout(title = paste("step",i%>%as.character(),sep = " "))%>%
    add_lines(x =~date,y=~realized_value,name="Realized Value",color=I("red"))
  
}

#Plots ----
for (i in 1:n_steps){
  step_plots[[i]]%>%print()
}


#Time series cross-validation del Average forecast combination y hit de las bandas
avg_forecast_combination_table_turistas <- avg_forecast_combination_function_real(classic_models_turistas)

sd_hit_single(avg_forecast_combination_table_turistas,"one")

avg_forecast_combination_table_turistas$model <- "Average"

opt_tests_function_single(avg_forecast_combination_table_turistas)

sapply(avg_forecast_combination_table_turistas%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#Time series cross-validation del regression forecast combination
regression_forecast_combination_table_turistas <- regression_forecast_combination_complete(classic_models_turistas,30)
weights <- regression_forecast_combination_table_turistas[[2]]
regression_forecast_combination_table_turistas <- regression_forecast_combination_table_turistas[[1]] 

sapply(regression_forecast_combination_table_turistas%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

regression_forecast_combination_table_turistas[regression_forecast_combination_table_turistas["step"]!="in_sample","train_test"] <- "test"
regression_forecast_combination_table_turistas[regression_forecast_combination_table_turistas["step"]=="in_sample","train_test"] <- "train"

#Función que agrega las desviaciones estándar y hits de las bandas
#para una, dos y tres desviaciones estándar
regression_forecast_combination_table_turistas <- hits_and_sd(regression_forecast_combination_table_turistas)

regression_forecast_combination_table_turistas$model <- "Regression Forecast Combination"

sd_hit_single(regression_forecast_combination_table_turistas,"one")


#Tests de optimalidad para el regression forecast combination
opt_test_function_combination(regression_forecast_combination_table_turistas)



all_models_real_turistas <- rbind(classic_models_turistas,avg_forecast_combination_table_turistas,
                                  regression_forecast_combination_table_turistas)
all_models_real_turistas_plots <- list()
for (i in 1:sum(lengths(all_models_real_turistas["model"]%>%unique()))){
  all_models_real_turistas_plots[[i]] <- all_models_real_turistas %>% filter((model == all_models_real_turistas["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=all_models_real_turistas["model"]%>%unique()%>%slice(i)%>%as.character()
    )
}
#Plot de los errores de todos los modelos
for (i in 1:sum(lengths(all_models_real_turistas["model"]%>%unique()))){
  print(all_models_real_turistas_plots[[i]])
}

#Predicciones de todos los modelos
step_plots <- list()
n_steps <- 6
for (i in 1:n_steps){
  step_plots[[i]] <- all_models_real_turistas%>%filter(step==paste("step",i%>%as.character(),sep = " "))%>%
    group_by(model)%>%
    plot_ly(x = ~date,y=~predicted, type = 'scatter',mode="marker",
            color=~factor(model)) %>% 
    layout(title = paste("step",i%>%as.character(),sep = " "))%>%
    add_lines(x =~date,y=~realized_value,name="Realized Value",color=I("red"))
  
}

#Plots ----
for (i in 1:n_steps){
  step_plots[[i]]%>%print()
}



#Plot de las predicciones de todos los modelos
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

if (cross_validate_with_synth_data) {

diff(turistas)%>%nrow()
(diff(turistas)%>%nrow()-2)%>%divisors()
synthetic_data <- synthetic_data_function(diff(turistas),2,1000,54,turistas)


#Función que hace el time series cross-validation para la data sintética
tic()
sarima_model_synthetic_table_turistas <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                        sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(1,1,1),seas_order=c(1,1,1))
toc()


tic()
model_synthetic_df_sarima_turistas <- data_frame_synthetic_conversion_function(turistas_sarima_table,
                                                                               sarima_model_synthetic_table_turistas,1000)
toc()


sapply(model_synthetic_df_sarima_turistas%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_sarima_turistas,"one")


tic()
ets_model_synthetic_table_turistas <- synthetic_model_table_function(
  final_synthetic_data =synthetic_data,
  ets_table_function,l_test_set=60,forecast_horizon=6)
toc()


tic()
model_synthetic_df_ets_turistas <- data_frame_synthetic_conversion_function(turistas_ets_table,
                                                                            ets_model_synthetic_table_turistas,1000)
toc()


sapply(model_synthetic_df_ets_turistas%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_ets_turistas,"one")



tic()
best_arima_model_synthetic_table_turistas <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                            sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(5,1,5),seas_order=c(0,0,0))
toc()


tic()
model_synthetic_df_best_arima_turistas <- data_frame_synthetic_conversion_function(turistas_best_arima_table,
                                                                                   best_arima_model_synthetic_table_turistas ,100)
toc()


sapply(model_synthetic_df_best_arima_turistas%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_best_arima_turistas,"one")



tic()
best_ets_model_synthetic_table_turistas <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                          ets_table_function_fixed,l_test_set=60,forecast_horizon=6,alpha=0.1,beta=0.1,gamma=0.4)
toc()


tic()
model_synthetic_df_best_ets_turistas <- data_frame_synthetic_conversion_function(turistas_best_ets_table,
                                                                                 best_ets_model_synthetic_table_turistas ,100)
toc()



sapply(model_synthetic_df_best_ets_turistas%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_best_ets_turistas,"one")

model_synthetic_df_sarima_turistas$model <- "SARIMA"
model_synthetic_df_ets_turistas$model <- "ETS"
model_synthetic_df_best_arima_turistas$model <- "Best ARIMA"
model_synthetic_df_best_ets_turistas$model <- "Best ETS"

classic_models_turistas_synthetic <- rbind(model_synthetic_df_sarima_turistas,model_synthetic_df_ets_turistas,
                                           model_synthetic_df_best_arima_turistas,model_synthetic_df_best_ets_turistas)


tic()
avg_forecast_combination_table_synthetic_turistas <- avg_forecast_combination_function_synthetic(classic_models_turistas_synthetic)
toc()

sapply(avg_forecast_combination_table_synthetic_turistas%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(avg_forecast_combination_table_synthetic_turistas,"one")


#Time series cross-validation para regression forecast combination con la data sintética
tic()
regression_forecast_combination_synthetic_table_turistas <- regression_forecast_combination_complete_synthetic(classic_models_turistas_synthetic,
                                                                                                               30)
toc()

weights <- regression_forecast_combination_synthetic_table_turistas[[2]]
regression_forecast_combination_synthetic_table_turistas <- regression_forecast_combination_synthetic_table_turistas[[1]]

#Añadiendo las desviaciones estándar y los hits de las bandas para regression forecast combination con la data sintética
regression_forecast_combination_synthetic_table_turistas <- hits_and_sd_synthetic(regression_forecast_combination_synthetic_table_turistas)

regression_forecast_combination_synthetic_table_turistas$model <- "Regression Forecast Combination"

#Parámetros de la distribución de los errores de regression forecast combination con la data sintética
sapply(regression_forecast_combination_synthetic_table_turistas%>%filter(step!="in_sample")%>%select(error)%>%drop_na(),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(regression_forecast_combination_synthetic_table_turistas,"one")
}