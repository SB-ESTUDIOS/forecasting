#Autor: Italo López
#Departamento de Estudios Económicos, Superintendencia de Bancos de la República Dominicana
#Fecha;: Julio 2023
#Título: Prediciendo series de Tiempo de Variables Económicas Dominicanas
#Variable: IMAE

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
library(xlsx)

# Esta variable controla si se ejecuta la parte del código que realiza el proceso con la data sintética
cross_validate_with_synth_data = FALSE

#Cargando funciones
source("auxiliares/funciones.R")
#Para prevenir notación científica
options(scipen = 999)

#Cargando la data
imae_series <- read.xlsx("series/imae.xlsx",sheetIndex = 1,header = F)

imae_series <- ts(imae_series,start = c(2007,1),frequency = 12)

#Plot de la serie completa
fig <- plot_ly(y=imae_series,x=index(imae_series), type = 'scatter', mode = 'lines')%>%
  layout(title="IMAE")
fig

#Training set
imae_training <- window(imae_series,end=tail(index(imae_series),60)[1])

#Plot del training set
fig <- plot_ly(y=imae_training,x=index(imae_training), type = 'scatter', mode = 'lines')%>%
  layout(title="IMAE")
fig
nrow(imae_training)

#Plot del training set en diferencia
fig <- plot_ly(y=diff(imae_training),x=index(imae_series), type = 'scatter', mode = 'lines')%>%
  layout(title="IMAE")
fig

#Tests de estacionaridad de la diferencia estacional
adf.test(diff(imae_training,12))
pp.test(diff(imae_training,12))
kpss.test(diff(imae_training,12))

#Tests de estacionaridad de la diferencia regular
adf.test(diff(imae_training))
pp.test(diff(imae_training))
kpss.test(diff(imae_training))

#ACF y PACF de la diferencia estacional
acf(diff(imae_training,12))
pacf(diff(imae_training,12))

#ACF y PACF de la diferencia regular
acf(diff(imae_training,1))
pacf(diff(imae_training,1))

#Calibración y diagnosis Box-Jenkins SARIMA
fit_arima_diag(imae_training,reg_order=c(3,0,0),seas_order=c(0,1,0))


imae_sarima_table <- sarima_table_function(model_data=imae_series,l_test_set=60,forecast_horizon=6,reg_order=c(3,0,0),seas_order=c(0,1,0))


#Esta función hace los tests de optimalidad
opt_tests_function_single(imae_sarima_table)

#Proporción del hit de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(imae_sarima_table,"one")

#Parámetros de la distribución de los errores de predicción
sapply(imae_sarima_table%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


#Ahora Double Naive
fit_arima_diag(imae_training,c(0,1,0),c(0,1,0))

imae_double_naive_table <- sarima_table_function(model_data=imae_series,l_test_set=60,forecast_horizon=6,reg_order=c(0,1,0),seas_order=c(0,1,0))


#Esta función hace los tests de optimalidad
opt_tests_function_single(imae_double_naive_table)

#Proporción del hit de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(imae_double_naive_table,"one")

#Parámetros de la distribución de los errores de predicción
sapply(imae_double_naive_table%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})



imae_direct_forecast_regression_table <- direct_forecast_regression_table_function(model_data=imae_series,l_test_set=60,forecast_horizon=6)


#Esta función hace los tests de optimalidad
opt_tests_function_single(imae_direct_forecast_regression_table)

#Proporción del hit de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(imae_direct_forecast_regression_table,"one")

#Parámetros de la distribución de los errores de predicción
sapply(imae_direct_forecast_regression_table%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})



#Identificación del mejor ARIMA
arimas <- best_arima_identification(imae_training)


arimas[[2]][[arimas[[1]]%>%which.min()]]

#Time series cross-validation del Grid/Best ARIMA
best_arima_table <- sarima_table_function(model_data=imae_series,l_test_set=60,forecast_horizon=6,reg_order=c(0,2,3),seas_order=c(0,0,0))


#Esta función hace los tests de optimalidad
opt_tests_function_single(best_arima_table)

#Proporción del hit de las bandas a una, dos y tres desviaciones estándar
sd_hit_single(imae_direct_forecast_regression_table,"one")

#Parámetros de la distribución de los errores de predicción
sapply(best_arima_table%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})


imae_sarima_table$model <- "SARIMA"
imae_double_naive_table$model <- "ETS"
imae_direct_forecast_regression_table$model <- "Best ARIMA"
best_arima_table$model <- "Best ETS"


individual_models <- rbind(imae_sarima_table,imae_double_naive_table,
                           imae_direct_forecast_regression_table,
                           best_arima_table)


#TIme series cross-validation del average forecast combination
avg_forecast_combination_table <- avg_forecast_combination_function_real(individual_models)

sd_hit_single(avg_forecast_combination_table,"one")

opt_tests_function_single(avg_forecast_combination_table)

#Parámetros de la distribución de los errores de predicción del average forecast combination
sapply(avg_forecast_combination_table%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

imae_sarima_table$model <- "SARIMA"
imae_double_naive_table$model <- "Double Naive"
imae_direct_forecast_regression_table$model <- "Direct Forecast Regression"
best_arima_table$model <- "Best ARIMA"


avg_forecast_combination_table$model <- "Average"


##En esta parte estimamos los modelos con la data sintética
#El usuario debe de tener en cuenta que se necesita una cantidad considerable de tiempo
#para generar los resultados con la data sintética
if (cross_validate_with_synth_data) {
#Ahora con la data sintética
diff(imae_series)%>%nrow()
diff(imae_series)%>%nrow()%>%divisors()


#Función que genera la data sintética
synthetic_data <- synthetic_data_function(diff(imae_series),1,1000,16,imae_series)


#Función que calibra y hace el time series cross-validation con 
#la  data sintética
tic()
sarima_model_synthetic_table_imae <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                    sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(1,1,0),seas_order=c(0,2,0))
toc()



#Esta función pone en el formato correcto los resultados anteriores
tic()
model_synthetic_df_sarima_imae <- data_frame_synthetic_conversion_function(imae_sarima_table,
                                                                          sarima_model_synthetic_table_imae,1000)
toc()
rm(sarima_model_synthetic_table_imae)

sapply(model_synthetic_df_sarima_imae%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_sarima_imae,"one")


tic()
double_naive_model_synthetic_table_imae <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                    sarima_table_function,l_test_set=60,forecast_horizon=6,reg_order=c(0,1,0),seas_order=c(0,1,0))
toc()



#Esta función pone en el formato correcto los resultados anteriores
tic()
model_synthetic_df_double_naive_imae <- data_frame_synthetic_conversion_function(imae_double_naive_table,
                                                                                 double_naive_model_synthetic_table_imae,1000)
toc()
rm(double_naive_model_synthetic_table_imae)

sapply(model_synthetic_df_double_naive_imae%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_double_naive_imae,"one")



tic()
direct_forecast_regression_synthetic_table_imae <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                                  direct_forecast_regression_table_function,l_test_set=60,forecast_horizon=6)
toc()



tic()
model_synthetic_df_direct_forecast_regression_imae <- data_frame_synthetic_conversion_function(imae_direct_forecast_regression_table,
                                                                                               direct_forecast_regression_synthetic_table_imae ,1000)
toc()


rm(direct_forecast_regression_synthetic_table_imae)
sapply(model_synthetic_df_direct_forecast_regression_imae%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_direct_forecast_regression_imae,"one")



tic()
best_arima_model_synthetic_table_imae <- synthetic_model_table_function(final_synthetic_data =synthetic_data,
                                                                     ets_table_function_fixed,l_test_set=60,forecast_horizon=6,,reg_order=c(0,2,3),seas_order=c(0,0,0))
toc()


tic()
model_synthetic_df_best_arima_imae <- data_frame_synthetic_conversion_function(best_arima_table,
                                                                               best_arima_model_synthetic_table_imae ,1000)
toc()
rm(best_arima_model_synthetic_table_imae)


sapply(model_synthetic_df_best_arima_imae%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(model_synthetic_df_best_arima_imae,"one")



model_synthetic_df_sarima_imae$model <- "SARIMA"
model_synthetic_df_double_naive_imae$model <- "ETS"
model_synthetic_df_direct_forecast_regression_imae$model <- "Best ARIMA"
model_synthetic_df_best_arima_imae$model <- "Best ETS"

classic_models_imae_synthetic <- rbind(model_synthetic_df_sarima_imae,model_synthetic_df_double_naive_imae,
                                      model_synthetic_df_direct_forecast_regression_imae,model_synthetic_df_best_arima_imae)


#Función que hace el time series cross-validation del average forecast combination con la data sintética
tic()
avg_forecast_combination_table_synthetic_imae <- avg_forecast_combination_function_synthetic(classic_models_imae_synthetic)
toc()

sapply(avg_forecast_combination_table_synthetic_ipc%>%filter(step!="in_sample")%>%select(error),function(x) {c(mean=mean(x),sd=sd(x),quantile(x,probs = seq(0, 1, 0.25)))})

sd_hit_single(avg_forecast_combination_table_synthetic_imae,"one")



model_synthetic_df_sarima_imae$model <- "SARIMA"
model_synthetic_df_double_naive_imae$model <- "Double Naive"
model_synthetic_df_direct_forecast_regression_imae$model <- "Direct Forecast Regression"
model_synthetic_df_best_arima_imae$model <- "Best ARIMA"

}

all_models_real <- rbind(imae_sarima_table,imae_double_naive_table,
                         imae_direct_forecast_regression_table,
                         best_arima_table,avg_forecast_combination_table)


#Plots de las predicciones y valor realizado 
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


#ACF del hit de las bandas de dos desviaciones estándar a un mes
best_arima_model_table%>%filter(step=="step 1")%>%select(hit_two)%>%filter(hit_two!="NA")%>%acf()

#Test de Kolmogorov-Smirnoff con H0 que el hit de las bandas de dos desviaciones estándar se distribuye Bernoulli 95%
ks.test(sarima_model_table%>%filter(step=="step 1")%>%
          select(hit_two)%>%filter(hit_two!="NA"),c(rep(1,950),rep(0,50)))


#Plot de las ppredicciones en una misma página
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

