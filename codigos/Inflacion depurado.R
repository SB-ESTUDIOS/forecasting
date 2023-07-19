#Autor: Italo López
#Departamento de Estudios Económicos, Superintendencia de Bancos de la República Dominicana
#Fecha;: Julio 2023
#Título: Prediciendo series de Tiempo de Variables Económicas Dominicanas
#Variable: Inflación (nivel)

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

#Cargando la data
cpi_series <- read.csv("ipc.csv",header = F)

cpi_series <- ts(cpi_series,start = c(1984,1),frequency = 12)

#Plot en nivel de la series
fig <- plot_ly(y=cpi_series,x=index(cpi_series), type = 'scatter', mode = 'lines')%>%
  layout(title="IPC RD")
fig

#Plot en variación interanual
fig <- plot_ly(y=diff(log(cpi_series),12)*100,x=index(diff(log(cpi_series),12)), type = 'scatter', mode = 'lines')%>%
  layout(title="IPC RD")

#Training set
cpi_training <- window(cpi_series,end=tail(index(cpi_series),60)[1])



#Número de observaciones
nrow(cpi_training)

#Tests de estacionaridad
adf.test(diff(cpi_training,12))
pp.test(diff(cpi_training,12))
kpss.test(diff(cpi_training,12))

adf.test(diff(cpi_training,1))
pp.test(diff(cpi_training,1))
kpss.test(diff(cpi_training,1))

acf(diff(cpi_training,1))
pacf(diff(cpi_training,1))

#Calibración y diagnosis Box-Jenkins SARIMA
fit_arima_diag(cpi_training,reg_order=c(1,1,0),seas_order=c(0,2,0))

#Time series cross-validation del SARIMA en nivel
cpi_sarima_table <- sarima_table_function(model_data=cpi_series,l_test_set=60,forecast_horizon=6,reg_order=c(1,1,0),seas_order=c(0,2,0))

#Esta función hace los tests de optimalidad
opt_tests_function_single(cpi_sarima_table)

#Proporción del hit de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(cpi_sarima_table,"one")

#Parámetros de la distribución de los errores de predicción
sapply(cpi_sarima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#ACF y PACF en nivel
acf(cpi_training)
pacf(cpi_training)

#ACF y PACF en diferencia estacional
acf(diff(cpi_training,12))
pacf(diff(cpi_training,12))

#ACF y PACF en diferencia intermensual
acf(diff(cpi_training,1))
pacf(diff(cpi_training,1))


#Calibración y time series cross-validation del ETS en nivel
cpi_ets_table <- ets_table_function(model_data=cpi_series,l_test_set=60,forecast_horizon=6,"AAA")

sd_hit_single(cpi_ets_table,"one")


opt_tests_function_single(cpi_ets_table)

sapply(cpi_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#Identificación del mejor ARIMA
arimas <- best_arima_identification(cpi_training)

#Best ARIMA is ARIMA (1,2,1)
#Orden del mejor ARIMA
arimas[[2]][[arimas[[1]]%>%which.min()]]

#Time series cross-validation del Grid/Best ARIMA
best_arima_table <- sarima_table_function(model_data=cpi_series,l_test_set=60,forecast_horizon=6,reg_order=c(1,2,1),seas_order=c(0,0,0))

sd_hit_single(best_arima_table,"one")


opt_tests_function_single(best_arima_table)

sapply(best_arima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Identificación del Grid/Best ETS
etss <- best_ets_identification(cpi_training)

#Best ETS identificado
etss[[2]][[etss[[1]]%>%which.min()]]

cpi_best_ets_table <- ets_table_function_fixed(model_data=cpi_series,l_test_set=60,forecast_horizon=6,alpha=0.6,beta=0.1,gamma=0.3)

sd_hit_single(cpi_best_ets_table,"one")


opt_tests_function_single(cpi_best_ets_table)

sapply(cpi_best_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})



cpi_sarima_table$model <- "SARIMA"
cpi_ets_table$model <- "ETS"
best_arima_table$model <- "Best ARIMA"
cpi_best_ets_table$model <- "Best ETS"

individual_models_table <- rbind(cpi_sarima_table,cpi_ets_table,best_arima_table,cpi_best_ets_table)

#TIme series cross-validation del average forecast combination
avg_forecast_combination_table <- avg_forecast_combination_function_real(individual_models_table)

sd_hit_single(avg_forecast_combination_table,"one")

opt_tests_function_single(avg_forecast_combination_table)

#Parámetros de la distribución de los errores de predicción del average forecast combination
sapply(avg_forecast_combination_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})



avg_forecast_combination_table$model <- "Average"

individual_models_table <- rbind(individual_models_table,avg_forecast_combination_table)


#Esta función hace el time series cross-validation del regression forecast combination
regression_forecast_combination_table_ipc <- regression_forecast_combination_complete(individual_models_table,30)
weights <- regression_forecast_combination_table_ipc[[2]]
regression_forecast_combination_table_ipc <- regression_forecast_combination_table_ipc[[1]] 

sapply(regression_forecast_combination_table_ipc%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

regression_forecast_combination_table_ipc[regression_forecast_combination_table_ipc["step"]!="in_sample","train_test"] <- "test"
regression_forecast_combination_table_ipc[regression_forecast_combination_table_ipc["step"]=="in_sample","train_test"] <- "train"


regression_forecast_combination_table_ipc <- hits_and_sd(regression_forecast_combination_table_ipc)

regression_forecast_combination_table_ipc$model <- "Regression Forecast Combination"


sd_hit_single(regression_forecast_combination_table_ipc,"one")


opt_test_function_combination(regression_forecast_combination_table_ipc)



##En esta parte estimamos los modelos con la data sintética
#El usuario debe de tener en cuenta que se necesita una cantidad considerable de tiempo
#para generar los resultados con la data sintética
if (cross_validate_with_synth_data) {
#Función que genera la data sintética
synthetic_data <- synthetic_data_function_2(diff(cpi_series),integer_correction=0,n_sim_samples=1000,n_obs_per_block=47,level_model_data=cpi_series)


#Función que calibra y hace el time series cross-validation con 
#la  data sintética
tic()
sarima_model_synthetic_table_ipc <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                       sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(1,1,0),seas_order=c(0,2,0))
toc()


#Esta función pone en el formato correcto los resultados anteriores
tic()
model_synthetic_df_sarima_ipc <- data_frame_synthetic_conversion_function(cpi_sarima_table,
                                                                              sarima_model_synthetic_table_ipc,1000)
toc()
rm(sarima_model_synthetic_table_ipc)


#Parámetros de la distribución de los errores con la data sintética en nivel
sapply(model_synthetic_df_sarima_ipc%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_sarima_ipc,"one")


#Igual que más arriba para el ETS
tic()
ets_model_synthetic_table_ipc <- synthetic_model_table_function(
  final_synthetic_data =synthetic_data,
  ets_table_function,l_test_set=60,forecast_horizon=6)
toc()


tic()
model_synthetic_df_ets_ipc <- data_frame_synthetic_conversion_function(cpi_ets_table,
                                                                           ets_model_synthetic_table_ipc,1000)
toc()

rm(ets_model_synthetic_table_ipc)


sapply(model_synthetic_df_ets_ipc%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_ets_ipc,"one")



tic()
best_arima_model_synthetic_table_ipc <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                           sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(1,2,1),seas_order=c(0,0,0))
toc()



tic()
model_synthetic_df_best_arima_ipc <- data_frame_synthetic_conversion_function(best_arima_table,
                                                                                  best_arima_model_synthetic_table_ipc ,1000)
toc()


rm(best_arima_model_synthetic_table_ipc)
sapply(model_synthetic_df_best_arima_ipc%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_best_arima_ipc,"one")



tic()
best_ets_model_synthetic_table_ipc <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                         ets_table_function_fixed,l_test_set=60,forecast_horizon=6,alpha=0.6,beta=0.1,gamma=0.3)
toc()


tic()
model_synthetic_df_best_ets_ipc <- data_frame_synthetic_conversion_function(cpi_best_ets_table,
                                                                                best_ets_model_synthetic_table_ipc ,1000)
toc()
rm(best_ets_model_synthetic_table_ipc)


sapply(model_synthetic_df_best_ets_ipc%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_best_ets_ipc,"one")



model_synthetic_df_sarima_ipc$model <- "SARIMA"
model_synthetic_df_ets_ipc$model <- "ETS"
model_synthetic_df_best_arima_ipc$model <- "Best ARIMA"
model_synthetic_df_best_ets_ipc$model <- "Best ETS"

classic_models_ipc_synthetic <- rbind(model_synthetic_df_sarima_ipc,model_synthetic_df_ets_ipc,
                                      model_synthetic_df_best_arima_ipc,model_synthetic_df_best_ets_ipc)


#Función que hace el time series cross-validation del average forecast combination con la data sintética
tic()
avg_forecast_combination_table_synthetic_ipc <- avg_forecast_combination_function_synthetic(classic_models_ipc_synthetic)
toc()

sapply(avg_forecast_combination_table_synthetic_ipc%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(avg_forecast_combination_table_synthetic_ipc,"one")


#Función que calibra y hace el time series cross-validation 
#del regression forecast combination con la data sintética
tic()
regression_forecast_combination_synthetic_table_ipc <- regression_forecast_combination_complete_synthetic(classic_models_ipc_synthetic,
                                                                                                                     30)
toc()



weights <- regression_forecast_combination_synthetic_table_ipc[[2]]
regression_forecast_combination_synthetic_table_ipc <- regression_forecast_combination_synthetic_table_ipc[[1]]



regression_forecast_combination_synthetic_table_ipc <- hits_and_sd_synthetic(regression_forecast_combination_synthetic_table_ipc)

regression_forecast_combination_synthetic_table_ipc$model <- "Regression Forecast Combination"


sapply(regression_forecast_combination_synthetic_table_ipc%>%filter(step!="in_sample")%>%select(error)%>%drop_na(),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(regression_forecast_combination_synthetic_table_ipc,"one")

regression_forecast_combination_synthetic_table_ipc[regression_forecast_combination_synthetic_table_ipc["step"]!="in_sample","train_test"] <- "test"
regression_forecast_combination_synthetic_table_ipc[regression_forecast_combination_synthetic_table_ipc["step"]=="in_sample","train_test"] <- "train"


synthetic_models <- rbind(classic_models_ipc_synthetic,
                          avg_forecast_combination_table_synthetic_ipc,
                          regression_forecast_combination_synthetic_table_ipc)

}
#Plots of all models 
cpi_sarima_table$model <- "SARIMA"
cpi_ets_table$model <- "ETS"
best_arima_table$model <- "Best ARIMA"
cpi_best_ets_table$model <- "Best ETS"
avg_forecast_combination_table$model <- "Average Combination"
regression_forecast_combination_table$model <- "Regression Combination"

regression_forecast_combination_table_ipc <- regression_forecast_combination_table_ipc%>%select(-Average)

all_models_real_table <- rbind(cpi_sarima_table,cpi_ets_table,best_arima_table,cpi_best_ets_table,
                               avg_forecast_combination_table,regression_forecast_combination_table_ipc)


#Plots de las predicciones y valor realizado en nivel
step_plots <- list()
n_steps <- 6
for (i in 1:n_steps){
  step_plots[[i]] <- all_models_real_table%>%filter(step==paste("step",i%>%as.character(),sep = " "))%>%
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


#Plot de los errores de todos los modelos 
inflacion_new_plots <- list()
for (i in 1:sum(lengths(all_models_real_table["model"]%>%unique()))){
  inflacion_new_plots[[i]] <- all_models_real_table %>% filter((model == all_models_real_table["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=all_models_real_table["model"]%>%unique()%>%slice(i)%>%as.character()
    )#,histnorm = "probability"
}

for (i in 1:sum(lengths(all_models_real_table["model"]%>%unique()))){
  print(inflacion_new_plots[[i]])
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


#Esta parte del código convierte los datos a interanual
interanual<- all_models_real_table%>%group_by(model,interaction)

interanual<-interanual%>%arrange(model,interaction,date)

interanual<-interanual%>%
  mutate(realized_value_interannual=c(rep(NA,12),diff(log(realized_value),12)*100))

interanual<-interanual%>%
  mutate(realized_value_lag=lag_vec(realized_value,12))%>%
  mutate(predicted_interannual=log(predicted/realized_value_lag)*100)%>%
  mutate(error_interannual=realized_value_interannual-predicted_interannual)%>%
  ungroup()%>%as.data.frame()


#Plots de las predicciones en interanual
step_plots <- list()
n_steps <- 6
for (i in 1:n_steps){
  step_plots[[i]] <- interanual%>%filter(step==paste("step",i%>%as.character(),sep = " "))%>%
    group_by(model)%>%
    plot_ly(x = ~date,y=~predicted_interannual, type = 'scatter',mode="marker",
            color=~factor(model)) %>% 
    layout(title = paste("step",i%>%as.character(),sep = " "))%>%
    add_lines(x =~date
              ,y=~realized_value_interannual,name="Realized Value",color=I("red"))
  
}

#Plots ----
for (i in 1:n_steps){
  step_plots[[i]]%>%print()
}


interanual2<-interanual%>%filter(step!="in_sample")
tapply(interanual2$error_interannual,interanual2$model,function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Tests de optimalidad para los resultados en interanual
opt_test_function_single_interannual(interanual%>%filter(model=="Regression Forecast Combination"))

opt_test_function_single_interannual(interanual%>%filter(model=="Best ARIMA"))

opt_test_function_single_interannual(interanual%>%filter(model=="Average Combination"))

opt_test_function_single_interannual(interanual%>%filter(model=="Best ETS"))

opt_test_function_single_interannual(interanual%>%filter(model=="ETS"))

opt_test_function_single_interannual(interanual%>%filter(model=="SARIMA"))

#Plots de los errores para resultados en variación interanual
inflacion_interanual_plots <- list()
for (i in 1:sum(lengths(bla["model"]%>%unique()))){
  inflacion_interanual_plots[[i]] <- interanual %>% filter((model == bla["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error_interannual,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=bla["model"]%>%unique()%>%slice(i)%>%as.character()
    )#,histnorm = "probability"
}


for (i in 1:sum(lengths(bla["model"]%>%unique()))){
  print(inflacion_interanual_plots[[i]])
}


#Agregando las desviaciones estándar a los resultados interanuales
interanual <- sd_interannual(interanual)

#Agregando los hits de las bandas a los resultados interanuales
interanual <- hits_interannual(interanual)

#Caculando la proporción del hit de las bandas para una, dos y tres desviaciones estándar
interanual2<-interanual%>%filter(step!="in_sample")
tapply(interanual2$hit_1,list(interanual2$model,interanual2$step),mean)%>%t()*100
tapply(interanual2$hit_2,list(interanual2$model,interanual2$step),mean)%>%t()*100
tapply(interanual2$hit_3,list(interanual2$model,interanual2$step),mean)%>%t()*100


#Plot de todas las predicciones en interanual en una misma gráfica
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



#Resultados en interanual con la data sintética
interanual_synthetic<- synthetic_models%>%group_by(model,interaction)

interanual_synthetic<-interanual_synthetic%>%arrange(model,interaction,date)

interanual_synthetic<-interanual_synthetic%>%
  mutate(realized_value_interannual=c(rep(NA,12),diff(log(realized_value),12)*100))

interanual_synthetic<-interanual_synthetic%>%
  mutate(realized_value_lag=lag_vec(realized_value,12))%>%
  mutate(predicted_interannual=log(predicted/realized_value_lag)*100)%>%
  mutate(error_interannual=realized_value_interannual-predicted_interannual)%>%
  ungroup()%>%as.data.frame()

#Parámetros de la distribución de los errores con la data sintética para los resultados en variación interanual
interanual_synthetic2<-interanual_synthetic%>%filter(step!="in_sample")
tapply(interanual_synthetic2$error_interannual,interanual_synthetic2$model,function(x) {c(mean=mean(x,na.rm=T),sd=sd(x,na.rm=T),quantile(x,probs = seq(0, 1, 0.25),na.rm=T))})
