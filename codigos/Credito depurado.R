#Autor: Italo López
#Departamento de Estudios Económicos, Superintendencia de Bancos de la República Dominicana
#Fecha;: Julio 2023
#Título: Prediciendo series de Tiempo de Variables Económicas Dominicanas
#Variable: Crédito (nivel)

library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(plotly)
library(tibble)
library(pracma)
library(ROracle)
library(keyring)
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
source("auxiliares/funciones.R")
#Para prevenir notación científica
options(scipen = 999)
#Leyendo la data
credito <- read.csv("series/credito.csv")
credito <-credito%>%select(DEUDA_TOTAL)%>%ts(start = c(2007,1),frequency = 12)
credito <-credito%>%as.zoo()

#Plot de la serie en nivel
fig <- plot_ly(y=credito[,1],x=index(credito), type = 'scatter', mode = 'lines')%>%
  layout(title="Cartera de Crédito Privada MN")
fig

#Plot de la variación interanual
fig <- plot_ly(y=diff(log(credito[,1]),12)*100,x=index(diff(log(credito[,1]),12)), type = 'scatter', mode = 'lines')%>%
  layout(title="Cartera de Crédito Privada MN")
fig

#Training data
credito_training <- window(credito,end=tail(index(credito),60)[1])

nrow(credito_training)

#Tests de estacionaridad
adf.test(credito_training)

adf.test(diff(credito_training))


fig <- plot_ly(y=diff(credito_training[,1]),x=index(diff(credito_training)), type = 'scatter', mode = 'lines')%>%
  layout(title="Cartera de Crédito Privada MN")
fig

fig <- plot_ly(y=diff(credito_training[,1],12),x=index(diff(credito_training,12)), type = 'scatter', mode = 'lines')%>%
  layout(title="Cartera de Crédito Privada MN")
fig


#Tests de estacionaridad
adf.test(diff(credito_training,12))
pp.test(diff(credito_training,12))
kpss.test(diff(credito_training,12))

adf.test(diff(log(credito_training),12))
pp.test(diff(log(credito_training),12))
kpss.test(diff(log(credito_training),12))

adf.test(diff(credito_training,1))
pp.test(diff(credito_training,1))
kpss.test(diff(credito_training,1))

#ACF y PACF
acf(diff(credito_training[,1]))
pacf(diff(credito_training[,1]))


#Función que entrena un SARIMA y da output de la diagnosis
fit_arima_diag(credito_training[,1],reg_order=c(1,2,1),seas_order=c(0,0,0))#1,2,1

#Funcion que entrena el SARIMA y hace time series cross-validation y calcula lo de las bandas
credito_sarima_table <- sarima_table_function(model_data=credito,l_test_set=60,forecast_horizon=6,reg_order=c(1,2,1),seas_order=c(0,0,0))

#Tests de optimalidad
opt_tests_function_single(credito_sarima_table)

#Parámetros de la distribución de los errores para el SARIMA
sapply(credito_sarima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#Proporción de los hits de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(credito_sarima_table,"one")


#Funcion que entrena el ETS y hace time series cross-validation y calcula lo de las bandas
credito_ets_table <- ets_table_function(model_data=credito,l_test_set=60,forecast_horizon=6,"ANA")


sd_hit_single(credito_ets_table,"one")


opt_tests_function_single(credito_ets_table)

sapply(credito_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

#Función para identificar el orden  del Grid/Best ARIMA

arimas <- best_arima_identification(credito_training/1000,30,6)

#Orden del mejor ARIMA
arimas[[2]][[arimas[[1]]%>%which.min()]]

rm(arimas)
credito_best_arima_table <- sarima_table_function(model_data=credito,l_test_set=60,forecast_horizon=6,reg_order=c(0,2,4),seas_order=c(0,0,0))


sd_hit_single(credito_best_arima_table,"one")


opt_tests_function_single(credito_best_arima_table)

sapply(credito_best_arima_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Función para identificar el Grid/Best ETS
etss <- best_ets_identification(credito_training)

#Orden del mejor ETS
etss[[2]][[etss[[1]]%>%which.min()]]

rm(etss)
credito_best_ets_table <- ets_table_function_fixed(model_data=credito,l_test_set=60,forecast_horizon=6,alpha=0.8,beta=0.1,gamma=0.1)

sd_hit_single(credito_best_ets_table,"one")


opt_tests_function_single(credito_best_ets_table)

#Función para obtener los parámetros de la distribución de los errores
sapply(credito_best_ets_table%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


credito_sarima_table$model <- "SARIMA"
credito_ets_table$model <- "ETS"
credito_best_arima_table$model <- "Best ARIMA"
credito_best_ets_table$model <- "Best ETS"

classic_models_credito <- rbind(credito_sarima_table,credito_ets_table,credito_best_arima_table,credito_best_ets_table)


#Plot de los errores de los modelos individuales
classic_models_credito_plots <- list()
for (i in 1:sum(lengths(classic_models_credito["model"]%>%unique()))){
  classic_models_credito_plots[[i]] <- classic_models_credito %>% filter((model == classic_models_credito["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=classic_models_credito["model"]%>%unique()%>%slice(i)%>%as.character()
    )#,histnorm = "probability"
}

for (i in 1:sum(lengths(classic_models_credito["model"]%>%unique()))){
  print(classic_models_credito_plots[[i]])
}


#Plot de las predicciones de los modelos individuales en nivel
step_plots <- list()
n_steps <- 6
for (i in 1:n_steps){
  step_plots[[i]] <- classic_models_credito%>%filter(step==paste("step",i%>%as.character(),sep = " "))%>%
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

#Función que hace el time series cross-validation para el average forecast combination
avg_forecast_combination_table_credito <- avg_forecast_combination_function_real(classic_models_credito)

sd_hit_single(avg_forecast_combination_table_credito,"one")

avg_forecast_combination_table_credito$model <- "Average"

opt_tests_function_single(avg_forecast_combination_table_credito)

sapply(avg_forecast_combination_table_credito%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Hacemos el time series cross-validation del regression forecast combination
regression_forecast_combination_table_credito <- regression_forecast_combination_complete(classic_models_credito,30)
weights <- regression_forecast_combination_table_credito[[2]]
regression_forecast_combination_table_credito <- regression_forecast_combination_table_credito[[1]] 

sapply(regression_forecast_combination_table_credito%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

regression_forecast_combination_table_credito[regression_forecast_combination_table_credito["step"]!="in_sample","train_test"] <- "test"
regression_forecast_combination_table_credito[regression_forecast_combination_table_credito["step"]=="in_sample","train_test"] <- "train"


regression_forecast_combination_table_credito <- hits_and_sd(regression_forecast_combination_table_credito)

regression_forecast_combination_table_credito$model <- "Regression Forecast Combination"

sd_hit_single(regression_forecast_combination_table_credito,"one")


opt_test_function_combination(regression_forecast_combination_table_credito)

#Data frame con todos los modelos
all_models_real_credito <- rbind(classic_models_credito,avg_forecast_combination_table_credito,
                                 regression_forecast_combination_table_credito)


#Plot de los errores de todos los modelos
all_models_real_credito_plots <- list()
for (i in 1:sum(lengths(all_models_real_credito["model"]%>%unique()))){
  all_models_real_credito_plots[[i]] <- all_models_real_credito %>% filter((model == all_models_real_credito["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=all_models_real_credito["model"]%>%unique()%>%slice(i)%>%as.character()
    )#,histnorm = "probability"
}


for (i in 1:sum(lengths(all_models_real_credito["model"]%>%unique()))){
  print(all_models_real_credito_plots[[i]])
}

if (cross_validate_with_synth_data) {
#Ahora con la data sintética
diff(credito)%>%nrow()
diff(credito)%>%nrow()%>%divisors()

#Función que genera la data sintética
synthetic_data <- synthetic_data_function_2(diff(credito),integer_correction=0,n_sim_samples=1000,n_obs_per_block=13,level_model_data=credito)


#Función que calibra y hace el time series cross-validation con 
#la  data sintética
tic()
sarima_model_synthetic_table_credito <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                       sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(1,2,1),seas_order=c(0,0,0))
toc()

#Esta función pone en el formato correcto los resultados anteriores
tic()
model_synthetic_df_sarima_credito <- data_frame_synthetic_conversion_function(credito_sarima_table,
                                                                              sarima_model_synthetic_table_credito,1000)
toc()
rm(sarima_model_synthetic_table_credito)
#Parámetros de la distribución de los errores con la data sintética en nivel
sapply(model_synthetic_df_sarima_credito%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_sarima_credito,"one")


#Igual que más arriba para el ETS
tic()
ets_model_synthetic_table_credito <- synthetic_model_table_function(
  final_synthetic_data =synthetic_data,
  ets_table_function,l_test_set=60,forecast_horizon=6)
toc()


tic()
model_synthetic_df_ets_credito <- data_frame_synthetic_conversion_function(credito_ets_table,
                                                                           ets_model_synthetic_table_credito,1000)
toc()

rm(ets_model_synthetic_table_credito)

sapply(model_synthetic_df_ets_credito%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_ets_credito,"one")


tic()
best_arima_model_synthetic_table_credito <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                           sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(0,2,4),seas_order=c(0,0,0))
toc()


tic()
model_synthetic_df_best_arima_credito <- data_frame_synthetic_conversion_function(credito_best_arima_table,
                                                                                  best_arima_model_synthetic_table_credito ,1000)
toc()

rm(best_arima_model_synthetic_table_credito)
sapply(model_synthetic_df_best_arima_credito%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_best_arima_credito,"one")


tic()
best_ets_model_synthetic_table_credito <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                         ets_table_function_fixed,l_test_set=60,forecast_horizon=6,alpha=0.8,beta=0.1,gamma=0.1)
toc()


tic()
model_synthetic_df_best_ets_credito <- data_frame_synthetic_conversion_function(credito_best_ets_table,
                                                                                best_ets_model_synthetic_table_credito ,1000)
toc()
rm(best_ets_model_synthetic_table_credito)


#Parámetros de la distribución de los errores para Grid/Best ETS con la data sintética
sapply(model_synthetic_df_best_ets_credito%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_best_ets_credito,"one")

model_synthetic_df_sarima_credito$model <- "SARIMA"
model_synthetic_df_ets_credito$model <- "ETS"
model_synthetic_df_best_arima_credito$model <- "Best ARIMA"
model_synthetic_df_best_ets_credito$model <- "Best ETS"

classic_models_credito_synthetic <- rbind(model_synthetic_df_sarima_credito,model_synthetic_df_ets_credito,
                                          model_synthetic_df_best_arima_credito,model_synthetic_df_best_ets_credito)


#Time series cross-validation del average forecast combination con la data sintética
tic()
avg_forecast_combination_table_synthetic_credito <- avg_forecast_combination_function_synthetic(classic_models_credito_synthetic)
toc()

sapply(avg_forecast_combination_table_synthetic_credito%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(avg_forecast_combination_table_synthetic_credito,"one")


#Time series cross-validation del regression forecast combination con la data sintética
tic()
regression_forecast_combination_synthetic_table_credito <- regression_forecast_combination_complete_synthetic(classic_models_credito_synthetic,
                                                                                                              30)
toc()



weights <- regression_forecast_combination_synthetic_table_credito[[2]]
regression_forecast_combination_synthetic_table_credito <- regression_forecast_combination_synthetic_table_credito[[1]]

#Agregando la desviación estándar y el hit de las bandas para regression forecast combination con la data sintética
regression_forecast_combination_synthetic_table_credito <- hits_and_sd_synthetic(regression_forecast_combination_synthetic_table_credito)

regression_forecast_combination_synthetic_table_credito$model <- "Regression Forecast Combination"

#Parámetros de la distribución de los errores para regression forecast combination con la data sintética
sapply(regression_forecast_combination_synthetic_table_credito%>%filter(step!="in_sample")%>%select(error)%>%drop_na(),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(regression_forecast_combination_synthetic_table_credito,"one")

regression_forecast_combination_synthetic_table_credito[regression_forecast_combination_synthetic_table_credito["step"]!="in_sample","train_test"] <- "test"
regression_forecast_combination_synthetic_table_credito[regression_forecast_combination_synthetic_table_credito["step"]=="in_sample","train_test"] <- "train"



synthetic_models <- rbind(classic_models_credito_synthetic,
                          avg_forecast_combination_table_synthetic_credito,
                          regression_forecast_combination_synthetic_table_credito)

}
#Plots de las predicciones con todos los modelos
step_plots <- list()
n_steps <- 6
for (i in 1:n_steps){
  step_plots[[i]] <- all_models_real_credito%>%filter(step==paste("step",i%>%as.character(),sep = " "))%>%
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

#Plots de las predicciones en una misma gráfica
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


#ACF del hit a horizonte de un mes para las bandas de dos desviaciones estándar
avg_forecast_combination_table_credito%>%filter(step=="step 1")%>%
  select(hit_two)%>%filter(hit_two!="NA")%>%acf()

#Test de Kolmogorov-Smirnoff
ks.test(avg_forecast_combination_table_credito%>%filter(step=="step 1")%>%
          select(hit_two)%>%filter(hit_two!="NA"),c(rep(1,950),rep(0,50)))


#En esta parte del código pasamos los resultados de nivel a interanual
interanual<- all_models_real_credito%>%group_by(model,interaction)

interanual<-interanual%>%arrange(model,interaction,date)

interanual<-interanual%>%
  mutate(realized_value_interannual=c(rep(NA,12),diff(log(realized_value),12)*100))

interanual<-interanual%>%
  mutate(realized_value_lag=lag_vec(realized_value,12))%>%
  mutate(predicted_interannual=log(predicted/realized_value_lag)*100)%>%
  mutate(error_interannual=realized_value_interannual-predicted_interannual)%>%
  ungroup()%>%as.data.frame()

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

for (i in 1:n_steps){
  step_plots[[i]]%>%print()
}


interanual2<-interanual%>%filter(step!="in_sample")
tapply(interanual2$error_interannual,interanual2$model,function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


opt_test_function_single_interannual(interanual%>%filter(model=="Regression Forecast Combination"))

opt_test_function_single_interannual(interanual%>%filter(model=="Best ARIMA"))

opt_test_function_single_interannual(interanual%>%filter(model=="Average"))

opt_test_function_single_interannual(interanual%>%filter(model=="Best ETS"))

opt_test_function_single_interannual(interanual%>%filter(model=="ETS"))

opt_test_function_single_interannual(interanual%>%filter(model=="SARIMA"))


#Plot de los errores para los resultados en variación interanual
credito_interanual_plots <- list()
for (i in 1:sum(lengths(bla["model"]%>%unique()))){
  credito_interanual_plots[[i]] <- interanual %>% filter((model == bla["model"]%>%unique()%>%slice(i)%>%as.character())&(interaction!=0))%>%
    group_by(step) %>%
    do(p=plot_ly(., x = ~error_interannual,name =~step, type = "histogram")) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
    layout(title=bla["model"]%>%unique()%>%slice(i)%>%as.character()
    )#,histnorm = "probability"
}


for (i in 1:sum(lengths(bla["model"]%>%unique()))){
  print(credito_interanual_plots[[i]])
}




interanual <- sd_interannual(interanual)


interanual <- hits_interannual(interanual)

interanual2<-interanual%>%filter(step!="in_sample")
tapply(interanual2$hit_1,list(interanual2$model,interanual2$step),mean)%>%t()*100
tapply(interanual2$hit_2,list(interanual2$model,interanual2$step),mean)%>%t()*100
tapply(interanual2$hit_3,list(interanual2$model,interanual2$step),mean)%>%t()*100


#Plot de las predicciones en interanual en una misma gráfica
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


#Esta parte del código convierte los resultados a variación interanual con la data sintética
interanual_synthetic<- synthetic_models%>%group_by(model,interaction)

interanual_synthetic<-interanual_synthetic%>%arrange(model,interaction,date)

interanual_synthetic<-interanual_synthetic%>%
  mutate(realized_value_interannual=c(rep(NA,12),diff(log(realized_value),12)*100))

interanual_synthetic<-interanual_synthetic%>%
  mutate(realized_value_lag=lag_vec(realized_value,12))%>%
  mutate(predicted_interannual=log(predicted/realized_value_lag)*100)%>%
  mutate(error_interannual=realized_value_interannual-predicted_interannual)%>%
  ungroup()%>%as.data.frame()

interanual_synthetic2<-interanual_synthetic%>%filter(step!="in_sample")
tapply(interanual_synthetic2$error_interannual,interanual_synthetic2$model,function(x) {c(mean=mean(x,na.rm=T),sd=sd(x,na.rm=T),quantile(x,probs = seq(0, 1, 0.25),na.rm=T))})
