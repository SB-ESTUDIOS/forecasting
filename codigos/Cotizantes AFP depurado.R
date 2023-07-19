#Autor: Italo López
#Departamento de Estudios Económicos, Superintendencia de Bancos de la República Dominicana
#Fecha;: Julio 2023
#Título: Prediciendo series de Tiempo de Variables Económicas Dominicanas
#Variable: Cotizantes AFP

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


current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_dir)
# Esta variable controla si se ejecuta la parte del código que realiza el proceso con la data sintética
cross_validate_with_synth_data = FALSE

#Extraemos primero la data 
cotizantes_afp <- read.csv("series/cotizantes_afp.csv")
cotizantes_afp <-cotizantes_afp%>%select(COTIZANTES)%>%ts(start = c(2003,9),frequency = 12)


#Ploteamos la variable
fig <- plot_ly(y=cotizantes_afp[,1],x=index(cotizantes_afp), type = 'scatter', mode = 'lines')%>%
  layout(title="Cotizantes AFP")
fig

#Tomamos el training set
cotizantes_afp_training <- window(cotizantes_afp,end=tail(index(cotizantes_afp),60)[1])

### New models forecasts and diagnosis -----

#Ploteamos el traning set
fig <- plot_ly(y=cotizantes_afp_training[,1],x=index(cotizantes_afp_training), type = 'scatter', mode = 'lines')%>%
  layout(title="Cotizantes AFP")
fig

#Numero de observaciones
nrow(cotizantes_afp_training)

#Tests de estacionaridad

#En nivel
adf.test(cotizantes_afp_training)
pp.test(cotizantes_afp_training)
kpss.test(cotizantes_afp_training)

#En diferencia intermensual
adf.test(diff(cotizantes_afp_training))
pp.test(diff(cotizantes_afp_training))
kpss.test(diff(cotizantes_afp_training))

#En diferencia estacional
adf.test(diff(cotizantes_afp_training,12))
pp.test(diff(cotizantes_afp_training,12))
kpss.test(diff(cotizantes_afp_training,12))

#Ploteamos la variable en diferencia estacional
fig <- plot_ly(y=diff(cotizantes_afp_training[,1],12),x=index(diff(cotizantes_afp_training,12)), type = 'scatter', mode = 'lines')%>%
  layout(title="Cotizantes AFP")
fig


source("auxiliares/funciones.R")

#Esta función nos calibra el modelo en el traning set y nos da output de la diagnosis
fit_arima_diag(cotizantes_afp_training[,1],reg_order=c(2,1,0),seas_order=c(1,1,0))



#Con esta función hacemos el time series cross-validation 
cotizantes_afp_sarima_table <- sarima_table_function(model_data=cotizantes_afp,l_test_set=60,forecast_horizon=6,reg_order=c(2,1,0),seas_order=c(1,1,0))


#Esta función hace los tests de optimalidad
opt_tests_function_single(cotizantes_afp_sarima_table)

#Esta función nos da los parámetros de la distribución de los errores
sapply(cotizantes_afp_sarima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#Esta función nos da la proporción del hit de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(cotizantes_afp_sarima_table,"one")



#Esta función hace el time series cross-validation del ETS
cotizantes_afp_ets_table <- ets_table_function(model_data=cotizantes_afp,l_test_set=60,forecast_horizon=6,"AAA")

sd_hit_single(cotizantes_afp_ets_table,"one")

opt_tests_function_single(cotizantes_afp_ets_table)

sapply(cotizantes_afp_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#Esta función estima un grid de arimas en el training set dejando la última parte de la muestra para
#Evaluar la suma de errores al cuadrado de las predicciones.
#Escalamos con mil porque a veces la función interna que hace la estimación tiene problemas con números grandes
arimas <- best_arima_identification(cotizantes_afp_training/1000,30,6)

#Mejor ARIMA
arimas[[2]][[arimas[[1]]%>%which.min()]]


cotizantes_afp_best_arima_table <- sarima_table_function(model_data=cotizantes_afp,l_test_set=60,forecast_horizon=6,reg_order=c(4,2,4),seas_order=c(0,0,0))

sd_hit_single(cotizantes_afp_best_arima_table,"one")


opt_tests_function_single(cotizantes_afp_best_arima_table)

sapply(cotizantes_afp_best_arima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Esta función hace los mismo con un grid para el ets que la anterior función con el ARIMA
etss <- best_ets_identification(cotizantes_afp_training)

#Best ETS
etss[[2]][[etss[[1]]%>%which.min()]]

#Esta función hace el time series cross-validation para el Grid/Best ETS
cotizantes_afp_best_ets_table <- ets_table_function_fixed(model_data=cotizantes_afp,l_test_set=60,forecast_horizon=6,alpha=0.7,beta=0.1,gamma=0.1)


sd_hit_single(cotizantes_afp_best_ets_table,"one")


opt_tests_function_single(cotizantes_afp_best_ets_table)

sapply(cotizantes_afp_best_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


cotizantes_afp_sarima_table$model <- "SARIMA"
cotizantes_afp_ets_table$model <- "ETS"
cotizantes_afp_best_arima_table$model <- "Best ARIMA"
cotizantes_afp_best_ets_table$model <- "Best ETS"


#Data frame con los modelos individuales clásicos
classic_models_cotizantes_afp <- rbind(cotizantes_afp_sarima_table,cotizantes_afp_ets_table,cotizantes_afp_best_arima_table,cotizantes_afp_best_ets_table)


#Plots de los errores de los modelos individuales
classic_models_cotizantes_afp_plots <- list()
for (i in 1:sum(lengths(classic_models_cotizantes_afp["model"]%>%unique()))){
  classic_models_cotizantes_afp_plots[[i]] <- classic_models_cotizantes_afp %>% filter((model == classic_models_cotizantes_afp["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=classic_models_cotizantes_afp["model"]%>%unique()%>%slice(i)%>%as.character()
    )#,histnorm = "probability"
}

for (i in 1:sum(lengths(classic_models_cotizantes_afp["model"]%>%unique()))){
  print(classic_models_cotizantes_afp_plots[[i]])
}


#Plots de las predicciones por step de los modelos individuales y valor realizado
step_plots <- list()
n_steps <- 6
for (i in 1:n_steps){
  step_plots[[i]] <- classic_models_cotizantes_afp%>%filter(step==paste("step",i%>%as.character(),sep = " "))%>%
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



 
#Esta función hace el time series cross-validation para el average forecast combination
avg_forecast_combination_table_cotizantes_afp <- avg_forecast_combination_function_real(classic_models_cotizantes_afp)

sd_hit_single(avg_forecast_combination_table_cotizantes_afp,"one")

avg_forecast_combination_table_cotizantes_afp$model <- "Average"

opt_tests_function_single(avg_forecast_combination_table_cotizantes_afp)

sapply(avg_forecast_combination_table_cotizantes_afp%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})





regression_forecast_combination_table_cotizantes_afp <- regression_forecast_combination_complete(classic_models_cotizantes_afp,30)
weights <- regression_forecast_combination_table_cotizantes_afp[[2]]
regression_forecast_combination_table_cotizantes_afp <- regression_forecast_combination_table_cotizantes_afp[[1]] 

sapply(regression_forecast_combination_table_cotizantes_afp%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

regression_forecast_combination_table_cotizantes_afp[regression_forecast_combination_table_cotizantes_afp["step"]!="in_sample","train_test"] <- "test"
regression_forecast_combination_table_cotizantes_afp[regression_forecast_combination_table_cotizantes_afp["step"]=="in_sample","train_test"] <- "train"



#Esta función le añade los hits al data frame del regression forecast combination
regression_forecast_combination_table_cotizantes_afp <- hits_and_sd(regression_forecast_combination_table_cotizantes_afp)

regression_forecast_combination_table_cotizantes_afp$model <- "Regression Forecast Combination"

sd_hit_single(regression_forecast_combination_table_cotizantes_afp,"one")

#Tests de optimalidad para regression forecasts combination
opt_test_function_combination(regression_forecast_combination_table_cotizantes_afp)


#All plots-----

#Plots de los errores de todos los modelos
all_models_real_cotizantes_afp <- rbind(classic_models_cotizantes_afp,avg_forecast_combination_table_cotizantes_afp,
                                        regression_forecast_combination_table_cotizantes_afp)
all_models_real_cotizantes_afp_plots <- list()
for (i in 1:sum(lengths(all_models_real_cotizantes_afp["model"]%>%unique()))){
  all_models_real_cotizantes_afp_plots[[i]] <- all_models_real_cotizantes_afp %>% filter((model == all_models_real_cotizantes_afp["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=all_models_real_cotizantes_afp["model"]%>%unique()%>%slice(i)%>%as.character()
    )#,histnorm = "probability"
}

for (i in 1:sum(lengths(all_models_real_cotizantes_afp["model"]%>%unique()))){
  print(all_models_real_cotizantes_afp_plots[[i]])
}


#Plots de las predicciones de todos los modelos y valor realizado
step_plots <- list()
n_steps <- 6
for (i in 1:n_steps){
  step_plots[[i]] <- all_models_real_cotizantes_afp%>%filter(step==paste("step",i%>%as.character(),sep = " "))%>%
    group_by(model)%>%
    plot_ly(x = ~date,y=~predicted, type = 'scatter',mode="marker",
            color=~factor(model)) %>% 
    layout(title = paste("step",i%>%as.character(),sep = " "))%>%
    add_lines(x =~date,y=~realized_value,name="Realized Value",color=I("red"))
  
}


for (i in 1:n_steps){
  step_plots[[i]]%>%print()
}


#Plot de todas las predicciones en una misma gráfica
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



# Synthetic data ------

if (cross_validate_with_synth_data) {
diff(cotizantes_afp)%>%nrow()
(diff(cotizantes_afp)%>%nrow()-1)%>%divisors()

#Esta función crea la data sintética
synthetic_data <- synthetic_data_function(diff(cotizantes_afp),1,1000,18,cotizantes_afp)


#Esta función hace el time series cross-validation con la data sintético
#para el modelo respectivo (sarima)
tic()
sarima_model_synthetic_table_cotizantes_afp <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                              sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(2,1,0),seas_order=c(1,1,0))
toc()


#Función que covierte el output anterior en un data frame con el formato querido
tic()
model_synthetic_df_sarima_cotizantes_afp <- data_frame_synthetic_conversion_function(cotizantes_afp_sarima_table,
                                                                                     sarima_model_synthetic_table_cotizantes_afp,100)
toc()


sapply(model_synthetic_df_sarima_cotizantes_afp%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_sarima_cotizantes_afp,"one")


tic()
ets_model_synthetic_table_cotizantes_afp <- synthetic_model_table_function(
  final_synthetic_data =synthetic_data,
  ets_table_function,l_test_set=60,forecast_horizon=6)
toc()


tic()
model_synthetic_df_ets_cotizantes_afp <- data_frame_synthetic_conversion_function(cotizantes_afp_ets_table,
                                                                                  ets_model_synthetic_table_cotizantes_afp,100)
toc()

sapply(model_synthetic_df_ets_cotizantes_afp%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_ets_cotizantes_afp,"one")



tic()
best_arima_model_synthetic_table_cotizantes_afp <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                                  sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(4,2,4),seas_order=c(0,0,0))
toc()


tic()
model_synthetic_df_best_arima_cotizantes_afp <- data_frame_synthetic_conversion_function(cotizantes_afp_best_arima_table,
                                                                                         best_arima_model_synthetic_table_cotizantes_afp ,100)
toc()


sapply(model_synthetic_df_best_arima_cotizantes_afp%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_best_arima_cotizantes_afp,"one")



tic()
best_ets_model_synthetic_table_cotizantes_afp <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                                ets_table_function_fixed,l_test_set=60,forecast_horizon=6,alpha=0.7,beta=0.1,gamma=0.1)
toc()


tic()
model_synthetic_df_best_ets_cotizantes_afp <- data_frame_synthetic_conversion_function(cotizantes_afp_best_ets_table,
                                                                                       best_ets_model_synthetic_table_cotizantes_afp ,100)
toc()


sapply(model_synthetic_df_best_ets_cotizantes_afp%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_best_ets_cotizantes_afp,"one")


model_synthetic_df_sarima_cotizantes_afp$model <- "SARIMA"
model_synthetic_df_ets_cotizantes_afp$model <- "ETS"
model_synthetic_df_best_arima_cotizantes_afp$model <- "Best ARIMA"
model_synthetic_df_best_ets_cotizantes_afp$model <- "Best ETS"

classic_models_cotizantes_afp_synthetic <- rbind(model_synthetic_df_sarima_cotizantes_afp,model_synthetic_df_ets_cotizantes_afp,
                                                 model_synthetic_df_best_arima_cotizantes_afp,model_synthetic_df_best_ets_cotizantes_afp)



tic()
avg_forecast_combination_table_synthetic_cotizantes_afp <- avg_forecast_combination_function_synthetic(classic_models_cotizantes_afp_synthetic)
toc()

sapply(avg_forecast_combination_table_synthetic_cotizantes_afp%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(avg_forecast_combination_table_synthetic_cotizantes_afp,"one")





#Función que calibra y hace el time series cross-validation 
#del regression forecast combination con la data sintética
tic()
regression_forecast_combination_synthetic_table_cotizantes_afp <- regression_forecast_combination_complete_synthetic(classic_models_cotizantes_afp_synthetic,
                                                                                                                     30)
toc()


weights <- regression_forecast_combination_synthetic_table_cotizantes_afp[[2]]
regression_forecast_combination_synthetic_table_cotizantes_afp <- regression_forecast_combination_synthetic_table_cotizantes_afp[[1]]



regression_forecast_combination_synthetic_table_cotizantes_afp <- hits_and_sd_synthetic(regression_forecast_combination_synthetic_table_cotizantes_afp)

regression_forecast_combination_synthetic_table_cotizantes_afp$model <- "Regression Forecast Combination"


sapply(regression_forecast_combination_synthetic_table_cotizantes_afp%>%filter(step!="in_sample")%>%select(error)%>%drop_na(),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(regression_forecast_combination_synthetic_table_cotizantes_afp,"one")

regression_forecast_combination_synthetic_table_cotizantes_afp[regression_forecast_combination_synthetic_table_cotizantes_afp["step"]!="in_sample","train_test"] <- "test"
regression_forecast_combination_synthetic_table_cotizantes_afp[regression_forecast_combination_synthetic_table_cotizantes_afp["step"]=="in_sample","train_test"] <- "train"


synthetic_models <- rbind(classic_models_cotizantes_afp_synthetic,
                          avg_forecast_combination_table_synthetic_cotizantes_afp,
                          regression_forecast_combination_synthetic_table_cotizantes_afp)

}
#ACF de las bandas de dos desviaciones estándar del mejor modelo
regression_forecast_combination_table_cotizantes_afp%>%filter(step=="step 1")%>%
  select(hit_two)%>%filter(hit_two!="NA")%>%acf()

#Test de Kolmogorov-Smirnoff para las bandas de dos desviaciones estándar
ks.test(regression_forecast_combination_table_cotizantes_afp%>%filter(step=="step 1")%>%
          select(hit_two)%>%filter(hit_two!="NA"),c(rep(1,950),rep(0,50)))
