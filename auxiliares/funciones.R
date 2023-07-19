#Autor: Italo López
#Departamento de Estudios Económicos, Superintendencia de Bancos de la República Dominicana
#Fecha;: Julio 2023
#Título: Prediciendo series de Tiempo de Variables Económicas Dominicanas


#Esta función realiza la calibración y diagnosis del SARIMA
#Los argumentos son:
#model_data: la serie de tiempo
#reg_order: el orden regular del SARIMA
#seas_order: el orden estacional del SARIMA
fit_arima_diag <- function(model_data,reg_order,seas_order){
  #Calibración del modelo (Llamada a función Arima requiere el paquete Forecast)
  fit_arima <- Arima(model_data,order=reg_order,seasonal = list(order=seas_order,period=12),include.mean = T)
  #Summary del modelo con los parámetros estimados y su significancia
  print(summary(fit_arima))
  #Fitted values
  print(fitted(fit_arima))
  #Plot de los residuos
  print(plot(residuals(fit_arima)))
  #Media del error
  print(mean(residuals(fit_arima)))
  #ACF del error
  acf(residuals(fit_arima))
  #PACF del error
  pacf(residuals(fit_arima))
  #Test de que los errores son white noise
  Box.test(residuals(fit_arima)) 
}


#Esta función realiza varias tareas:
# 1) el time series cross-validation del modelo SARIMA ajustado a una serie de tiempo
# 2) calcula las desvaciones estándar, las bandas y los hits
# Los argumentos son:
#model_data: la serie de tiempo
#l_test_set: el largo del test set
#forecast_horizon: el horizonte de predicción
#reg_order: orden regular del SARIMA
#seas_order: orden estacional del SARIMA
sarima_table_function <- function(model_data,l_test_set,forecast_horizon,reg_order,seas_order){
  #definiendo algunas variables internas
  in_sample <- list()
  l_test_set <- l_test_set 
  forecast_horizon <- forecast_horizon
  forecasts_df <- matrix(ncol=forecast_horizon,nrow = l_test_set)
  forecasts_df <- data.frame(forecasts_df)
  forecast_errors_df <- matrix(ncol=forecast_horizon,nrow = l_test_set)
  forecast_errors_df <- data.frame(forecasts_df)
  
  colnames(forecasts_df)<- c("step 1","step 2","step 3","step 4","step 5","step 6")
  #Aquí hacemos las calibraciones y la generación de los forecasts del time series cross-validation
  for (i in 1:l_test_set){
    data <- window(model_data,end=tail(index(model_data),l_test_set+forecast_horizon)[i])
    fit_model <- Arima(data,order=reg_order,seasonal = list(order=seas_order,period=12),include.mean = T,method="ML")
    forecasts_df[i,] <- forecast(fit_model,h=forecast_horizon)$mean
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
    forecast_errors_df[i,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[i,]
    in_sample[[i]] <-cbind(fitted(fit_model),residuals(fit_model),rep(i,length(fitted(fit_model))))
  }
  #En esta parte armamos la parte in-sample del dataframe
  in_sample_data.frame <- cbind(index(in_sample[[1]]),in_sample[[1]])
  for (i in 2:l_test_set){
    in_sample_data.frame <-rbind(in_sample_data.frame,cbind(index(in_sample[[i]]),in_sample[[i]]))
  }
  in_sample_data.frame <-in_sample_data.frame%>%as.data.frame()
  in_sample_data.frame$realized_value <- in_sample_data.frame[,2]+in_sample_data.frame[,3]
  colnames(in_sample_data.frame)[2]<- 'predicted'
  colnames(in_sample_data.frame)[3] <- 'error'
  colnames(in_sample_data.frame)[1] <- 'date'
  in_sample_data.frame$step <- "in_sample"
  in_sample_data.frame$standard_deviation <- NA
  in_sample_data.frame$train_test <- "train"
  colnames(in_sample_data.frame)[4] <- 'interaction'
  
  
  #En esta parte comenzamos a armar la parte de out-of-sample del data frame
  forecasts_for_tables <- forecasts_df[1,]%>%t()%>%as.data.frame()%>%rownames_to_column()
  start_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+1]
  end_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+forecast_horizon]
  realized_value <- window(model_data,start=start_series,end=end_series)
  forecasts_for_tables$date <- realized_value%>%index()
  forecasts_for_tables$realized_value <- realized_value%>%as.numeric()
  start_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+1] 
  end_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+forecast_horizon]
  forecast_errors_df[1,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[1,]
  forecasts_for_tables$error <- forecast_errors_df[1,]%>%as.numeric()
  names(forecasts_for_tables)[names(forecasts_for_tables) == '1'] <- 'predicted'
  forecasts_for_tables$interaction <-1
  
  #En esta parte terminamos con el out-of-sample
  for (i in 2:l_test_set){
    forecasts_for_tables_bla <- forecasts_df[i,]%>%t()%>%as.data.frame()%>%rownames_to_column()
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
    realized_value <- window(model_data,start=start_series,end=end_series)
    forecasts_for_tables_bla$date <- realized_value%>%index()
    forecasts_for_tables_bla$realized_value <- realized_value%>%as.numeric()
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
    forecast_errors_df[i,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[i,]
    forecasts_for_tables_bla$error <- forecast_errors_df[i,]%>%as.numeric()
    forecasts_for_tables_bla$interaction <- i
    names(forecasts_for_tables_bla)[names(forecasts_for_tables_bla) == i%>%as.character()] <- 'predicted'
    forecasts_for_tables <- rbind(forecasts_for_tables,forecasts_for_tables_bla)
  }
  #Calculamos las desviaciones estándar
  tapply(forecasts_for_tables$error, forecasts_for_tables$rowname, sd)
  standard_deviation <- tapply(forecasts_for_tables$error, forecasts_for_tables$rowname, sd)
  number_of_steps <- 6
  forecasts_for_tables$standard_deviation <- rep(standard_deviation,nrow(forecasts_for_tables)/number_of_steps)
  forecasts_for_tables$train_test <- "test"
  names(forecasts_for_tables)[names(forecasts_for_tables) == 'rowname'] <- 'step'
  
  #Juntamos in-sample y out-of-sample
  model_table <- rbind(in_sample_data.frame,forecasts_for_tables)
  
  #Armamos lo de las bandas
  model_table$lower_bound_one <-model_table$predicted-model_table$standard_deviation
  model_table$upper_bound_one <-model_table$predicted+model_table$standard_deviation
  model_table$lower_bound_two <-model_table$predicted-2*model_table$standard_deviation
  model_table$upper_bound_two <-model_table$predicted+2*model_table$standard_deviation
  model_table$lower_bound_three <-model_table$predicted-3*model_table$standard_deviation
  model_table$upper_bound_three <-model_table$predicted+3*model_table$standard_deviation
  
  #En esta parte hacemos la parte de los hits
  source("auxiliares/sd_hit_single.R")
  temporary <- sd_hit_single(model_table,"one")
  model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(model_table,"two")
  model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(model_table,"three")
  model_table <- temporary[[1]]

  model_table <-model_table%>%as.data.frame()
  model_table
  return(model_table)
  
}
  
#Esta función hace los tests de optimalidad
opt_tests_function_single <- function(data){
    filter_mask<-data["train_test"]=="test"
    filter_mask_2<-data["step"]=="step 1"
    #Box-Pierce test
    box_test<-Box.test(data[filter_mask&filter_mask_2,"error"],lag = 3)
    #Test de sesgo
    fit1<-lm(data[filter_mask&filter_mask_2,"error"]~1)
    summary(fit1)
    plot(data[filter_mask,"error"])
    #Mincer-Zarnowitz test
    fit2<-lm(data[filter_mask&filter_mask_2,"error"]~data[filter_mask&filter_mask_2,"predicted"])
    summary(fit2)
    return(list(box_test,summary(fit1),summary(fit2)))
}
  
 #Esta función realiza:
#1) Time series cross-validation del Minimum AIC ETS
#2) Calcula las desviciones estándar, las bandas y los hits
#Los argumentos son:
#model_data: la serie de tiempo
#l_test_set: el largo del test set
#forecast_horizon: el horizonte de predicción
#model_ets:El tipo de ETS
ets_table_function <- function(model_data,l_test_set,forecast_horizon,model_ets){
  #Definimos variables
    in_sample <- list()
    l_test_set <- l_test_set 
    forecast_horizon <- forecast_horizon
    forecasts_df <- matrix(ncol=forecast_horizon,nrow = l_test_set)
    forecasts_df <- data.frame(forecasts_df)
    forecast_errors_df <- matrix(ncol=forecast_horizon,nrow = l_test_set)
    forecast_errors_df <- data.frame(forecasts_df)
    
    colnames(forecasts_df)<- c("step 1","step 2","step 3","step 4","step 5","step 6")
    #Esta parte realiza la calibración y generación de predicciones para cada interacción
    for (i in 1:l_test_set){
      data <- window(model_data,end=tail(index(model_data),l_test_set+forecast_horizon)[i])
      data <- data%>%as.ts()
      fit_model <- ets(data,additive.only = T)
      forecasts_df[i,] <- forecast(fit_model,h=forecast_horizon)$mean
      start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
      end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
      forecast_errors_df[i,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[i,]
      in_sample[[i]] <-cbind(fitted(fit_model),residuals(fit_model),rep(i,length(fitted(fit_model))))
    }
    #Esta parte arma la parte in-sample del data frame
    in_sample_data.frame <- cbind(index(in_sample[[1]]),in_sample[[1]])
    for (i in 2:l_test_set){
      in_sample_data.frame <-rbind(in_sample_data.frame,cbind(index(in_sample[[i]]),in_sample[[i]]))
    }
    in_sample_data.frame <-in_sample_data.frame%>%as.data.frame()
    in_sample_data.frame$realized_value <- in_sample_data.frame[,2]+in_sample_data.frame[,3]
    colnames(in_sample_data.frame)[2]<- 'predicted'
    colnames(in_sample_data.frame)[3] <- 'error'
    colnames(in_sample_data.frame)[1] <- 'date'
    in_sample_data.frame$step <- "in_sample"
    in_sample_data.frame$standard_deviation <- NA
    in_sample_data.frame$train_test <- "train"
    colnames(in_sample_data.frame)[4] <- 'interaction'
    
    #Esta parte arma la parte out-of-sample del data frame
    forecasts_for_tables <- forecasts_df[1,]%>%t()%>%as.data.frame()%>%rownames_to_column()
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+1]
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+forecast_horizon]
    realized_value <- window(model_data,start=start_series,end=end_series)
    forecasts_for_tables$date <- realized_value%>%index()
    forecasts_for_tables$realized_value <- realized_value%>%as.numeric()
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+1] 
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+forecast_horizon]
    forecast_errors_df[1,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[1,]
    forecasts_for_tables$error <- forecast_errors_df[1,]%>%as.numeric()
    names(forecasts_for_tables)[names(forecasts_for_tables) == '1'] <- 'predicted'
    forecasts_for_tables$interaction <-1
    
    #Esta parte continua con lo mismo
    for (i in 2:l_test_set){
      forecasts_for_tables_bla <- forecasts_df[i,]%>%t()%>%as.data.frame()%>%rownames_to_column()
      start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
      end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
      realized_value <- window(model_data,start=start_series,end=end_series)
      forecasts_for_tables_bla$date <- realized_value%>%index()
      forecasts_for_tables_bla$realized_value <- realized_value%>%as.numeric()
      start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
      end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
      forecast_errors_df[i,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[i,]
      forecasts_for_tables_bla$error <- forecast_errors_df[i,]%>%as.numeric()
      forecasts_for_tables_bla$interaction <- i
      names(forecasts_for_tables_bla)[names(forecasts_for_tables_bla) == i%>%as.character()] <- 'predicted'
      forecasts_for_tables <- rbind(forecasts_for_tables,forecasts_for_tables_bla)
    }
    #esta parte calcula las desviaciones estándar
    tapply(forecasts_for_tables$error, forecasts_for_tables$rowname, sd)
    standard_deviation <- tapply(forecasts_for_tables$error, forecasts_for_tables$rowname, sd)
    number_of_steps <- 6
    forecasts_for_tables$standard_deviation <- rep(standard_deviation,nrow(forecasts_for_tables)/number_of_steps)
    forecasts_for_tables$train_test <- "test"
    names(forecasts_for_tables)[names(forecasts_for_tables) == 'rowname'] <- 'step'
    
    model_table <- rbind(in_sample_data.frame,forecasts_for_tables)
    
    #Esta parte arma las bandas
    model_table$lower_bound_one <-model_table$predicted-model_table$standard_deviation
    model_table$upper_bound_one <-model_table$predicted+model_table$standard_deviation
    model_table$lower_bound_two <-model_table$predicted-2*model_table$standard_deviation
    model_table$upper_bound_two <-model_table$predicted+2*model_table$standard_deviation
    model_table$lower_bound_three <-model_table$predicted-3*model_table$standard_deviation
    model_table$upper_bound_three <-model_table$predicted+3*model_table$standard_deviation
    
    #Esta parte hace lo de los hits
    source("auxiliares/sd_hit_single.R")
    temporary <- sd_hit_single(model_table,"one")
    model_table <- temporary[[1]]
    
    temporary <- sd_hit_single(model_table,"two")
    model_table <- temporary[[1]]
    
    temporary <- sd_hit_single(model_table,"three")
    model_table <- temporary[[1]]

    model_table <-model_table%>%as.data.frame()
    
    return(model_table)
  }
  
  #Función que calcula la proporción de los hits a una, dos y tres desviaciones estándar
#Los argumentos son:
#models_table:el dataframe del modelo
#n_sd: número de desviaciones estándar (one, two or three)
  sd_hit_single <- function(models_table,n_sd){
    #Creamos columna nueva
    models_table[,paste("hit_",n_sd,sep = "")] <- NA
    #En este chunk filtramos por out-of-sample y bandas
    filter_mask <- models_table[,"step"]!="in_sample"
    filter_2 <- models_table[,paste("lower_bound_",n_sd,sep = "")]%>%as.data.frame()<=models_table[,"realized_value"]%>%as.data.frame()
    filter_2 <-filter_2 & models_table[,"realized_value"]%>%as.data.frame()<=models_table[,paste("upper_bound_",n_sd,sep = "")]%>%as.data.frame()
    filter_2[is.na(filter_2)]<- FALSE
    
    #Imputamos el hit
    models_table[(filter_mask)&(filter_2),paste("hit_",n_sd,sep = "")] <- 1
    filter_3 <- is.na(models_table[,paste("hit_",n_sd,sep = "")]) & filter_mask
    
    #Imputamos cuando no hay hit
    models_table[filter_3,paste("hit_",n_sd,sep = "")] <- 0
    
    #Aquí armamos la tabla con los resultados de los hit
    n_out_of_sample <- models_table%>%filter(step=="step 1")%>%nrow()
    models_table[filter_mask,paste("hit_",n_sd,sep = "")]%>%sum()/models_table[filter_mask,paste("hit_",n_sd,sep = "")]%>%nrow()
    proportion_hit_table <- matrix()
    proportion_hit_table <- proportion_hit_table%>%tibble()
    sum_hit_table<-models_table[filter_mask,]%>%group_by(step)%>%summarise(hit_proportion=sum(eval(parse(text=paste("hit_",n_sd,sep = "")))))%>%as.data.frame()#sum(hit)/length(hit))
    proportion_hit_table<-sum_hit_table[,"hit_proportion"]*100/n_out_of_sample
    proportion_hit_table<-cbind(sum_hit_table[,1:2],proportion_hit_table)
    proportion_hit_table<-proportion_hit_table%>%filter(step!="in-sample")
    return(list(models_table,proportion_hit_table))
  }


#Esta función calibra todos los modelos del grid para el Grid/Best ARIMA y calcula la Sum of Squared Errors (SSE) para cada modelo
#Los argumentos son:
#training_data: el training set
#arima_interactions: la cantidad de interacciones
#forecast_horizon: el horizonte de predicción
best_arima_identification <- function(training_data,arima_interactions,forecast_horizon){
  
  arima_interactions <- 30
  forecast_horizon <- 6
  #Data para calibrar
  arimas_data <- training_data%>%head(-(arima_interactions+forecast_horizon))
  
  #Calibramos todos los modelos en este chunk
  fit_arimas <- list()
  start <- list()
  start[[1]] <- c(0,0,0)
  fit_arimas[[1]] <- Arima(arimas_data,start[[1]],seasonal = list(order=c(0,0,0),period=12),include.mean = T)
  i<-0
  # arimas_data
  for (p in 0:5) {
    for(d in 0:2){
      for(q in 0:5){
        i<- i+1
        tryCatch({
          start[[i]] <- c(p,d,q)
          fit_arimas[[i]] <- Arima(arimas_data,order=c(p,d,q),seasonal = list(order=c(0,0,0),period=12),include.mean = T)
          names(fit_arimas[[i]]<-c(p,d,q))}, error=function(e){}
        )
      }
    }}
  
  n_models <- 6*6*3
  in_sample <- list()
  forecasts_df <- array(dim=c(arima_interactions,forecast_horizon,n_models))
  
  forecast_errors_df <- array(dim=c(arima_interactions,forecast_horizon,n_models))
  
  j<-0
  
  colnames(forecasts_df)<- c("step 1","step 2","step 3","step 4","step 5","step 6")
  #Aquí hacemos el time series cross-validation
  for (p in 0:5) {
    for(d in 0:2){
      for(q in 0:5){
        j<- j+1
        tryCatch({
          for (i in 1:arima_interactions){
            data <- window(training_data,end=tail(index(training_data),arima_interactions+forecast_horizon)[i])
            fit_model <- Arima(data,order=c(p,d,q),seasonal = list(order=c(0,0,0),period=12),include.mean = T)
            forecasts_df[i,,j] <- forecast(fit_model,h=6)$mean
            start_series <- tail(index(training_data),arima_interactions+forecast_horizon)[i+1]
            end_series <- tail(index(training_data),arima_interactions+forecast_horizon)[i+forecast_horizon]
            forecast_errors_df[i,,j] <- window(training_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[i,,j]
            
          }}, error=function(e){}
        )
      }}}
  
  #Calculamos el SSE
  rmse_vector <- vector(length = n_models)
  rmse_vector <- apply(forecast_errors_df,3,function(x){x^2%>%sum()})
  return(list(rmse_vector,start))}


#Función análoga a best_arima_identification pero para el ETS
best_ets_identification <- function(training_data,ets_interactions,forecast_horizon){
  
  ets_interactions <- 30
  forecast_horizon <- 6
  ets_data <- training_data%>%head(-(ets_interactions+forecast_horizon))
  
  fit_ets <- list()
  start <- list()
  start[[1]] <- c(0,0,0)
  fit_ets[[1]] <- ets(ets_data,start[[1]],model = "AAA")#
  i<-0
  
  for (alpha in seq(0, 1, 0.1)) {
    for(beta in seq(0, 1, 0.1)){
      for(gamma in seq(0, 1, 0.1)){
        i<- i+1
        tryCatch({
          start[[i]] <- c(alpha,beta,gamma)
          fit_ets[[i]] <- ets(ets_data,alpha=alpha,beta=beta,gamma=gamma,model = "AAA")#
          names(fit_ets[[i]]<-c(alpha,beta,gamma))}, error=function(e){}
        )
      }
    }}
  
  n_models <- seq(0, 1, 0.1)%>%length()*seq(0, 1, 0.1)%>%length()*seq(0, 1, 0.1)%>%length()
  in_sample <- list()
  forecasts_df <- array(dim=c(ets_interactions,forecast_horizon,n_models))
  
  forecast_errors_df <- array(dim=c(ets_interactions,forecast_horizon,n_models))
  
  j<-0
  
  colnames(forecasts_df)<- c("step 1","step 2","step 3","step 4","step 5","step 6")
  for (alpha in seq(0, 1, 0.1)) {
    for(beta in seq(0, 1, 0.1)){
      for(gamma in seq(0, 1, 0.1)){
        j<- j+1
        tryCatch({
          for (i in 1:ets_interactions){
            data <- window(training_data,end=tail(index(training_data),ets_interactions+forecast_horizon)[i])
            fit_model <- ets(data,alpha=alpha,beta=beta,gamma=gamma,model = "AAA")#
            forecasts_df[i,,j] <- forecast(fit_model,h=6)$mean
            start_series <- tail(index(training_data),ets_interactions+forecast_horizon)[i+1]
            end_series <- tail(index(training_data),ets_interactions+forecast_horizon)[i+forecast_horizon]
            forecast_errors_df[i,,j] <- window(training_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[i,,j]
            
          }}, error=function(e){}
        )
      }}}
  #Calculamos el SSE
  rmse_vector <- vector(length = n_models)
  rmse_vector <- apply(forecast_errors_df,3,function(x){x^2%>%sum()})
  return(list(rmse_vector,start))}


#Función análoga a ets_table_function, pero fijando los parámetros del ETS
#Esta función realiza:
#1) Time series cross-validation del ETS fijando los parámetros
#2) Calcula las desviaciones estándar, las bandas y los hits
#los argumentos son:
#model_data: la serie de tiempo
#l_test_set: largo del test set
#forecast_horizon: horizonte de predicción
#beta: parámetro beta
#alpha: parámetro alpha
#gamma: parámetro gamma
ets_table_function_fixed <- function(model_data,l_test_set,forecast_horizon,beta=beta,alpha=alpha,gamma=gamma){
  #definimos variables
  in_sample <- list()
  l_test_set <- l_test_set 
  forecast_horizon <- forecast_horizon
  forecasts_df <- matrix(ncol=forecast_horizon,nrow = l_test_set)
  forecasts_df <- data.frame(forecasts_df)
  forecast_errors_df <- matrix(ncol=forecast_horizon,nrow = l_test_set)
  forecast_errors_df <- data.frame(forecasts_df)
  
  colnames(forecasts_df)<- c("step 1","step 2","step 3","step 4","step 5","step 6")
  #Calculamos las estimaciones y la generación de predicciones para cada interacción
  for (i in 1:l_test_set){
    data <- window(model_data,end=tail(index(model_data),l_test_set+forecast_horizon)[i])
    fit_model <- ets(data,beta=beta,alpha=alpha,gamma=gamma,additive.only = T)
    forecasts_df[i,] <- forecast(fit_model,h=forecast_horizon)$mean
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
    forecast_errors_df[i,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[i,]
    in_sample[[i]] <-cbind(fitted(fit_model),residuals(fit_model),rep(i,length(fitted(fit_model))))
  }
  #armamos la parte in-sample del dataframe
  in_sample_data.frame <- cbind(index(in_sample[[1]]),in_sample[[1]])
  for (i in 2:l_test_set){
    in_sample_data.frame <-rbind(in_sample_data.frame,cbind(index(in_sample[[i]]),in_sample[[i]]))
  }
  in_sample_data.frame <-in_sample_data.frame%>%as.data.frame()
  in_sample_data.frame$realized_value <- in_sample_data.frame[,2]+in_sample_data.frame[,3]
  colnames(in_sample_data.frame)[2]<- 'predicted'
  colnames(in_sample_data.frame)[3] <- 'error'
  colnames(in_sample_data.frame)[1] <- 'date'
  in_sample_data.frame$step <- "in_sample"
  in_sample_data.frame$standard_deviation <- NA
  in_sample_data.frame$train_test <- "train"
  colnames(in_sample_data.frame)[4] <- 'interaction'
  
  #Armamos la parte out-of-sample del dataframe
  forecasts_for_tables <- forecasts_df[1,]%>%t()%>%as.data.frame()%>%rownames_to_column()
  start_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+1]
  end_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+forecast_horizon]
  realized_value <- window(model_data,start=start_series,end=end_series)
  forecasts_for_tables$date <- realized_value%>%index()
  forecasts_for_tables$realized_value <- realized_value%>%as.numeric()
  start_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+1] 
  end_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+forecast_horizon]
  forecast_errors_df[1,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[1,]
  forecasts_for_tables$error <- forecast_errors_df[1,]%>%as.numeric()
  names(forecasts_for_tables)[names(forecasts_for_tables) == '1'] <- 'predicted'
  forecasts_for_tables$interaction <-1
  
  #cotinuamos armando esa parte
  for (i in 2:l_test_set){
    forecasts_for_tables_bla <- forecasts_df[i,]%>%t()%>%as.data.frame()%>%rownames_to_column()
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
    realized_value <- window(model_data,start=start_series,end=end_series)
    forecasts_for_tables_bla$date <- realized_value%>%index()
    forecasts_for_tables_bla$realized_value <- realized_value%>%as.numeric()
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
    forecast_errors_df[i,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[i,]
    forecasts_for_tables_bla$error <- forecast_errors_df[i,]%>%as.numeric()
    forecasts_for_tables_bla$interaction <- i
    names(forecasts_for_tables_bla)[names(forecasts_for_tables_bla) == i%>%as.character()] <- 'predicted'
    forecasts_for_tables <- rbind(forecasts_for_tables,forecasts_for_tables_bla)
  }
  #calculamos las desviaciones estándar
  tapply(forecasts_for_tables$error, forecasts_for_tables$rowname, sd)
  standard_deviation <- tapply(forecasts_for_tables$error, forecasts_for_tables$rowname, sd)
  number_of_steps <- 6
  forecasts_for_tables$standard_deviation <- rep(standard_deviation,nrow(forecasts_for_tables)/number_of_steps)
  forecasts_for_tables$train_test <- "test"
  names(forecasts_for_tables)[names(forecasts_for_tables) == 'rowname'] <- 'step'
  
  model_table <- rbind(in_sample_data.frame,forecasts_for_tables)
  
  #Calculamos las bandas
  model_table$lower_bound_one <-model_table$predicted-model_table$standard_deviation
  model_table$upper_bound_one <-model_table$predicted+model_table$standard_deviation
  model_table$lower_bound_two <-model_table$predicted-2*model_table$standard_deviation
  model_table$upper_bound_two <-model_table$predicted+2*model_table$standard_deviation
  model_table$lower_bound_three <-model_table$predicted-3*model_table$standard_deviation
  model_table$upper_bound_three <-model_table$predicted+3*model_table$standard_deviation
  
  #Hacemos lo de los hits
  source("auxiliares/sd_hit_single.R")
  temporary <- sd_hit_single(model_table,"one")
  model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(model_table,"two")
  model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(model_table,"three")
  model_table <- temporary[[1]]
  
  
  model_table <-model_table%>%as.data.frame()
  
  return(model_table)
}

#Esta función hace el time series cross-validation del average forecast combination
#Los argumentos son:
#models_table: dataframe con las predicciones de todos los modelos individuales
avg_forecast_combination_function_real <- function(models_table){
  #Hacemos fit y generación de forecast
  avg_forecast_combination <-models_table%>%group_by(interaction,date)%>%summarize(predicted=mean(predicted),realized_value=mean(realized_value),date=date,train_test=train_test,step=step,interaction=interaction)
  avg_forecast_combination <- avg_forecast_combination%>%as.data.frame()
  avg_forecast_combination <- avg_forecast_combination%>%unique()
  
  #Calculamor errores
  avg_forecast_combination$error <- avg_forecast_combination$realized_value-avg_forecast_combination$predicted
  avg_forecast_combination$model <- "Average"
  tapply(avg_forecast_combination$error, avg_forecast_combination$step, sd)
  forecasts_part <- avg_forecast_combination%>%filter(step!="in_sample")
  #calculamos las desviaciones estándar
  standard_deviation <- tapply(forecasts_part$error, forecasts_part$step, sd)
  number_of_steps <- 6
  forecasts_part$standard_deviation <- rep(standard_deviation,nrow(forecasts_part)/number_of_steps)
  avg_forecast_combination$standard_deviation<-NA
  #La ponemos en el data frame
  avg_forecast_combination[avg_forecast_combination[,"step"]!="in_sample","standard_deviation"]<- forecasts_part$standard_deviation
  
  #Hacemos las bandas
  avg_forecast_combination$lower_bound_one <-avg_forecast_combination$predicted-avg_forecast_combination$standard_deviation
  avg_forecast_combination$upper_bound_one <-avg_forecast_combination$predicted+avg_forecast_combination$standard_deviation
  avg_forecast_combination$lower_bound_two <-avg_forecast_combination$predicted-2*avg_forecast_combination$standard_deviation
  avg_forecast_combination$upper_bound_two <-avg_forecast_combination$predicted+2*avg_forecast_combination$standard_deviation
  avg_forecast_combination$lower_bound_three <-avg_forecast_combination$predicted-3*avg_forecast_combination$standard_deviation
  avg_forecast_combination$upper_bound_three <-avg_forecast_combination$predicted+3*avg_forecast_combination$standard_deviation
  
  #Hacemos los hits
  temporary <- sd_hit_single(avg_forecast_combination,"one")
  avg_forecast_combination <- temporary[[1]]
  
  temporary <- sd_hit_single(avg_forecast_combination,"two")
  avg_forecast_combination <- temporary[[1]]
  
  temporary <- sd_hit_single(avg_forecast_combination,"three")
  avg_forecast_combination <- temporary[[1]]
  return(avg_forecast_combination)}

#Esta función hace una interaction del time series cross-validation del regression forecast combination
#Los argumentos son:
#models_table: dataframe con las predicciones de todos los modelos individuales
#n_comb_set: número de interacciones a utilizar para realizar las regresiones
regression_forecast_combination_function <- function(models_tables,n_comb_set){
  #Tomamos las columnas relevantes del input dataframe
  ols_f_data <- models_tables%>%filter(step!="in_sample",interaction<=n_comb_set,model!="Average")%>%select(predicted,model,realized_value,date,interaction)
  ols_f_data<-ols_f_data%>%spread(key = model, value = predicted)%>%as.data.frame()
  ols_f_data<-ols_f_data%>%arrange(interaction)
  #Creamos la dummy
  ols_f_data<-ols_f_data %>% mutate(
    
    dummy = case_when(
      
      date >= 2020.167 & date <= 2021.167
      ~ 1,
      
      TRUE ~ 0
    ))
  #Hacemos la regresión
  f_comb_model <- lm(realized_value~ `Best ARIMA`+`ETS`+`Best ETS`+`SARIMA`+dummy+`ETS`*dummy+`Best ETS`*dummy+
                       `Best ARIMA`*dummy+ `SARIMA`*dummy-1,data = ols_f_data)#+`Double Naive`
  summary(f_comb_model)
  #Tomamos los coeficientes
  f_comb_coef <- f_comb_model$coefficients
  
  #En este chunk generamos los forecast
  ols_f_oos_data_keep <- models_tables%>%filter(step!="in_sample",interaction==n_comb_set+1)%>%select(predicted,model,realized_value,date,interaction,step)
  ols_f_oos_data_keep <- ols_f_oos_data_keep%>%arrange(desc(interaction))
  ols_f_oos_data <- models_tables%>%filter(step!="in_sample",interaction==n_comb_set+1)%>%select(predicted,model,realized_value,date,interaction,step)
  ols_f_oos_data<-ols_f_oos_data%>%spread(key = model, value = predicted)%>%as.data.frame()
  ols_f_oos_data<-ols_f_oos_data%>%arrange(desc(interaction))
  ols_f_oos_data$predicted <- sweep(ols_f_oos_data%>%select(5:8), MARGIN=2, f_comb_coef[1:4], `*`)%>%
    apply(1,sum)
  ols_f_oos_data$error <- ols_f_oos_data$realized_value - ols_f_oos_data$predicted
  
  #En este chunk armamos el dataframe
  ols_f_oos_data <- subset( ols_f_oos_data, select = -c(`Best ARIMA`,`Best ETS`,`ETS`,`SARIMA`) )
  
  in_sample_data_keep <- models_tables%>%filter(step=="in_sample",interaction==n_comb_set+1)%>%select(predicted,model,realized_value,date,interaction,step)
  #in_sample_data_keep <- in_sample_data_data_keep%>%arrange(desc(interaction))
  in_sample_data <- models_tables%>%filter(step=="in_sample",interaction==n_comb_set+1)%>%select(predicted,model,realized_value,date,interaction,step)
  in_sample_data<-in_sample_data%>%spread(key = model, value = predicted)%>%as.data.frame()
  in_sample_data<-in_sample_data%>%arrange(date,interaction)
  in_sample_data$predicted <- sweep(in_sample_data%>%select(5:8), MARGIN=2, f_comb_coef[1:4], `*`)%>%
    apply(1,sum)
  in_sample_data$error <- in_sample_data$realized_value - in_sample_data$predicted
   
  in_sample_data <- subset( in_sample_data, select = -c(`Best ARIMA`,`Best ETS`,`ETS`,`SARIMA`) )
  reg_comb_table <- rbind(in_sample_data,ols_f_oos_data)
  return(list(reg_comb_table,f_comb_coef[1:4]))
}

#Esta función hace la regression forecast combination para todas las interactions
#Los argumentos son:
#models_table: dataframe con las predicciones de todos los modelos individuales
#n_comb_set: número de interacciones a utilizar para realizar las regresiones
regression_forecast_combination_complete <- function(models_tables,n_comb_set){
  source("auxiliares/regression_forecast_combination_function.R")
  regression_forecast_combination_table <-regression_forecast_combination_function(models_tables,n_comb_set)[[1]]
  weigths <-regression_forecast_combination_function(models_tables,n_comb_set)[[2]] 
  temporary <- list()
  for (i in 1:(n_comb_set-1)){
    temporary[[i]] <- regression_forecast_combination_function(models_tables,n_comb_set+i) 
    regression_forecast_combination_table <- rbind(regression_forecast_combination_table,temporary[[i]][[1]])
    weigths<- rbind(weigths,temporary[[i]][[2]])
  }
  return(list(regression_forecast_combination_table,weigths))}

#Esta función genera los hits de las bandas y las desviaciones estándar para el regression forecast combination
hits_and_sd <- function(model_table){
#Calculamos las desviaciones estándar
  model_table[model_table["step"]!="in_sample",]
  standard_deviation <- tapply(model_table[model_table["step"]!="in_sample","error"], model_table[model_table["step"]!="in_sample","step"], sd)
  number_of_steps <- 6
  model_table[model_table["step"]!="in_sample","standard_deviation"] <- rep(standard_deviation,nrow(model_table[model_table["step"]!="in_sample",])/number_of_steps)
  
  #Creamos las bandas
  model_table$lower_bound_one <-model_table$predicted-model_table$standard_deviation
  model_table$upper_bound_one <-model_table$predicted+model_table$standard_deviation
  model_table$lower_bound_two <-model_table$predicted-2*model_table$standard_deviation
  model_table$upper_bound_two <-model_table$predicted+2*model_table$standard_deviation
  model_table$lower_bound_three <-model_table$predicted-3*model_table$standard_deviation
  model_table$upper_bound_three <-model_table$predicted+3*model_table$standard_deviation
  
 
  #Creamos los hits
  source("auxiliares/sd_hit_single.R")
  temporary <- sd_hit_single(model_table,"one")
  model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(model_table,"two")
  model_table <- cbind(model_table,temporary[[1]][,ncol(temporary[[1]])])
  colnames(model_table)[ncol(model_table)] <- "hit_two"
  
  temporary <- sd_hit_single(model_table,"three")
  model_table <- cbind(model_table,temporary[[1]][,ncol(temporary[[1]])])
  colnames(model_table)[ncol(model_table)] <- "hit_three"
  
  
  return(model_table)}


#Esta función es análoga a opt_tests_function pero para el regression forecast combination
#Los argumentos son:
#data: dataframe del modelo
opt_test_function_combination <- function(data){
  
  filter_mask_2<-data["step"]=="step 1"
  box_test<-Box.test(data[filter_mask_2,"error"],lag = 3)
  fit1<-lm(data[filter_mask_2,"error"]~1)
  summary(fit1)
  plot(data[,"error"])
  fit2<-lm(data[filter_mask_2,"error"]~data[filter_mask_2,"predicted"])
  summary(fit2)
  return(list(box_test,summary(fit1),summary(fit2)))
}


#Esta función crea la data sintética
#Los argumentos son:
#model_data: serie de tiempo en diferencias
#integer_correction: número de observaciones iniciales al descartar en la simulación
#n_sim_samples: número de series sintéticas
#n_obs_per_block: número de observaciones por bloque
#level_model_data: data en nivel
synthetic_data_function <- function(model_data,integer_correction,n_sim_samples,n_obs_per_block,level_model_data){
  
  (model_data%>%nrow()-integer_correction)%>%divisors()
  #Número de observaciones por bloque
  n_obs_per_block <- n_obs_per_block
  #Número de bloques
  n_blocks <- (model_data%>%nrow()-integer_correction)/n_obs_per_block
  #definimos la serie de la cual samplearemos
  stationary_series_to_sample <- model_data%>%as.zoo()
  index(stationary_series_to_sample) <- index(model_data)
  stationary_series_to_sample <- model_data%>%tail(-1*(integer_correction))%>%as.zoo()
  index(stationary_series_to_sample) <- index(model_data%>%tail(-1*(integer_correction)))
  #Armamos los bloques
  blocks<-stationary_series_to_sample%>%matrix(ncol=n_blocks,nrow=n_obs_per_block)
  n_sim_samples <- n_sim_samples
  #Hacemos sampling con reemplazo
  blocks_sample <- sample(1:n_blocks,n_blocks*n_sim_samples,replace = T)%>%matrix(nrow=n_sim_samples)
  #Este chunk obtiene la data estacionaria simulada
  synthetic_data<-blocks[,blocks_sample[1,]]%>%as.vector()%>%as.zoo()
  for(i in 2:n_sim_samples){
    bla <- blocks[,blocks_sample[i,]]%>%as.vector()%>%as.zoo()
    synthetic_data<-cbind(synthetic_data,bla)
    colnames(synthetic_data)[i] <- i
  }
  colnames(synthetic_data)[1] <- 1
  synthetic_data<-synthetic_data%>%as.zoo()
  index(synthetic_data) <- index(stationary_series_to_sample)
  #Este chunk obtiene la serie en nivel
  final_synthetic_data <- apply(synthetic_data,2,function(x){cumsum(x)+level_model_data[(integer_correction+1)]%>%as.numeric()})
  final_synthetic_data <- apply(final_synthetic_data,2,function(x){
    ts(c(level_model_data[1:(integer_correction+1)]%>%as.numeric(),x%>%as.numeric()), start=start(level_model_data[1:(integer_correction+1)]), frequency=frequency(level_model_data[1:(integer_correction+1)]))%>%as.zoo()})

  return(final_synthetic_data)}

#Esta función hace el time series cross-validation con la data sintética para los diferentes modelos
#Ponemos .... porque allí pondremos la función que corresponde a cada modelo
#Los argumentos son:
#final_synthetic_data: la data sintética
#real_table_function: función del modelo
#...: argumentos de la función anterior
synthetic_model_table_function <- function(final_synthetic_data,real_table_function,...){
  model_synthetic_list <-list()
  for (i in 1:ncol(final_synthetic_data)){tryCatch({
    bla <- real_table_function(final_synthetic_data[,i],...)}, error=function(e){}
  )
    model_synthetic_list[[i]] <- bla%>%as.data.frame()
  }
  
  return(model_synthetic_list)
}

#Esta función hace el formateo del ouput de la función anterior
#Los argumentos son:
#original_table: tabla del modelo con la data real
#synthetic_list: output de la función synthetic_model_table_function
#n_sim_samples: número de series sintéticas
data_frame_synthetic_conversion_function <- function(
    original_table,
    synthetic_list,
    n_sim_samples)
{
  model_synthetic_df_model <- data.frame(nrow=nrow(original_table)*n_sim_samples,ncol=(ncol(original_table)))
  model_synthetic_df_model <- cbind(synthetic_list[[1]],rep(i,nrow(synthetic_list[[1]])))
  colnames(model_synthetic_df_model)[ncol(model_synthetic_df_model)] <- "synthetic"
  for (i in 2:n_sim_samples){
    bla <- cbind(synthetic_list[[i]],rep(i,nrow(synthetic_list[[i]])))
    colnames(bla)[ncol(bla)] <- "synthetic"
    model_synthetic_df_model <- rbind(model_synthetic_df_model,bla)
  }
  return(model_synthetic_df_model)}

#Esta función hace lo análogo para el average forecast combination con la data sintética
#Los argumentos son:
#models_table: dataframe con los modelos individuales con la data sintética
avg_forecast_combination_function_synthetic <- function(models_table){
  #Calculamos las predicciones
  avg_forecast_combination <-models_table%>%group_by(interaction,date,synthetic)%>%summarize(predicted=mean(predicted),realized_value=mean(realized_value),date=date,train_test=train_test,step=step,interaction=interaction,synthetic=synthetic)
  avg_forecast_combination <- avg_forecast_combination%>%as.data.frame()
  avg_forecast_combination <- avg_forecast_combination%>%unique()
  
  #Calculamos error y desviaciones estándar
  avg_forecast_combination$error <- avg_forecast_combination$realized_value-avg_forecast_combination$predicted
  avg_forecast_combination$model <- "Average"
  tapply(avg_forecast_combination$error, avg_forecast_combination$step, sd)
  forecasts_part <- avg_forecast_combination%>%filter(step!="in_sample")
  standard_deviation <- tapply(forecasts_part$error, forecasts_part$step, sd)
  number_of_steps <- 6
  forecasts_part$standard_deviation <- rep(standard_deviation,nrow(forecasts_part)/number_of_steps)
  avg_forecast_combination$standard_deviation<-NA
  avg_forecast_combination[avg_forecast_combination[,"step"]!="in_sample","standard_deviation"]<- forecasts_part$standard_deviation
  
  #Calculamos bandas
  avg_forecast_combination$lower_bound_one <-avg_forecast_combination$predicted-avg_forecast_combination$standard_deviation
  avg_forecast_combination$upper_bound_one <-avg_forecast_combination$predicted+avg_forecast_combination$standard_deviation
  avg_forecast_combination$lower_bound_two <-avg_forecast_combination$predicted-2*avg_forecast_combination$standard_deviation
  avg_forecast_combination$upper_bound_two <-avg_forecast_combination$predicted+2*avg_forecast_combination$standard_deviation
  avg_forecast_combination$lower_bound_three <-avg_forecast_combination$predicted-3*avg_forecast_combination$standard_deviation
  avg_forecast_combination$upper_bound_three <-avg_forecast_combination$predicted+3*avg_forecast_combination$standard_deviation
  
  #Calculamos hits
  temporary <- sd_hit_single(avg_forecast_combination,"one")
  avg_forecast_combination <- temporary[[1]]
  
  temporary <- sd_hit_single(avg_forecast_combination,"two")
  avg_forecast_combination <- temporary[[1]]
  
  temporary <- sd_hit_single(avg_forecast_combination,"three")
  avg_forecast_combination <- temporary[[1]]
  return(avg_forecast_combination)}

#Esta función hace el regression forecast combination con la data sintética
#Los argumentos son
#models_table: dataframe con los modelos con la data sintética
#n_comb_set:número de interacciones con el que se comienza a hacer las regresiones
regression_forecast_combination_complete_synthetic <- function(models_tables,n_comb_set){
  source("auxiliares/regression_forecast_combination_synthetic.R")
  regression_forecast_combination_table <-regression_forecast_combination_synthetic(models_tables,n_comb_set)
  weigths <-regression_forecast_combination_table[[2]] 
  regression_forecast_combination_table <- regression_forecast_combination_table[[1]]
  temporary <- list()
  for (i in 1:(n_comb_set-1)){
    temporary[[i]] <- regression_forecast_combination_synthetic(models_tables,n_comb_set+i) 
    regression_forecast_combination_table <- rbind(regression_forecast_combination_table,temporary[[i]][[1]])
    weigths<- rbind(weigths,temporary[[i]][[2]])
  }
  return(list(regression_forecast_combination_table,weigths))}

#Esta función hace las bandas y los hits con la data sintética
#Los argumentos son
#model_table: dataframe del modelo
hits_and_sd_synthetic <- function(model_table){
  model_table <- model_table%>%group_by(synthetic)
  #Calculando las desviaciones estándar
  standard_deviation <- tapply(model_table$error, model_table$step, sd,na.rm=T)
  number_of_steps <- 6
  model_table$standard_deviation <- rep(standard_deviation,nrow(model_table)/number_of_steps)
  #Calculando las bandas
  model_table$lower_bound_one <-model_table$predicted-model_table$standard_deviation
  model_table$upper_bound_one <-model_table$predicted+model_table$standard_deviation
  model_table$lower_bound_two <-model_table$predicted-2*model_table$standard_deviation
  model_table$upper_bound_two <-model_table$predicted+2*model_table$standard_deviation
  model_table$lower_bound_three <-model_table$predicted-3*model_table$standard_deviation
  model_table$upper_bound_three <-model_table$predicted+3*model_table$standard_deviation
  #Calculando los hits
  source("auxiliares/sd_hit_single.R")
  temporary <- sd_hit_single(model_table,"one")
  model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(model_table,"two")
  model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(model_table,"three")
  model_table <- temporary[[1]]
  
  
  return(model_table)}

#Esta función hace lo mismo que synthetic_data_function cuando integer_correction es 0 cero
#Los argumentos son:
#model_data: la serie en diferencias
#integer_correction: cero 0
#n_sim_samples: número de series sintéticas
#número de observaciones por bloque
#level_model_data: data en nivel
synthetic_data_function_2 <- function(model_data,integer_correction,n_sim_samples,n_obs_per_block,level_model_data){
  
  (model_data%>%nrow()-integer_correction)%>%divisors()
  #Número de observaciones por bloque
  n_obs_per_block <- n_obs_per_block
  #número de bloques
  n_blocks <- (model_data%>%nrow()-integer_correction)/n_obs_per_block
  stationary_series_to_sample <- model_data%>%as.zoo()
  index(stationary_series_to_sample) <- index(model_data)
  #bloques
  blocks<-stationary_series_to_sample%>%matrix(ncol=n_blocks,nrow=n_obs_per_block)
  n_sim_samples <- n_sim_samples
  #bloques sampleados
  blocks_sample <- sample(1:n_blocks,n_blocks*n_sim_samples,replace = T)%>%matrix(nrow=n_sim_samples)
  #Armando la data sintética en diferencias
  synthetic_data<-blocks[,blocks_sample[1,]]%>%as.vector()%>%as.zoo()
  for(i in 2:n_sim_samples){
    bla <- blocks[,blocks_sample[i,]]%>%as.vector()%>%as.zoo()
    synthetic_data<-cbind(synthetic_data,bla)
    colnames(synthetic_data)[i] <- i
  }
  colnames(synthetic_data)[1] <- 1
  synthetic_data<-synthetic_data%>%as.zoo()
  index(synthetic_data) <- index(stationary_series_to_sample)
  #Armando la data sintética en nivel
  final_synthetic_data <- apply(synthetic_data,2,function(x){cumsum(x)+level_model_data[(integer_correction+1)]%>%as.numeric()})
  final_synthetic_data <- apply(final_synthetic_data,2,function(x){
    ts(c(level_model_data[1:(integer_correction+1)]%>%as.numeric(),x%>%as.numeric()), start=start(level_model_data[1:(integer_correction+1)]), frequency=frequency(level_model_data[1:(integer_correction+1)]))%>%as.zoo()})

  return(final_synthetic_data)}

#Lo análogo pero a opt_test_function_single los resultados en variación interanual
#Los argumentos son
#data: dataframe del modelo
opt_test_function_single_interannual <- function(data){
  #filtros
  filter_mask<-data["train_test"]=="test"
  filter_mask_2<-data["step"]=="step 1"
  #Box-Pierce test
  box_test<-Box.test(data[filter_mask&filter_mask_2,"error_interannual"],lag = 3)
  #Test de sesgo
  fit1<-lm(data[filter_mask&filter_mask_2,"error_interannual"]%>%unlist()~1)
  summary(fit1)
  plot(data[filter_mask,"error"])
  #Test de regresión Mincer-Zarnowitz
  fit2<-lm(data[filter_mask&filter_mask_2,"error_interannual"]~data[filter_mask&filter_mask_2,"predicted_interannual"])
  summary(fit2)
  return(list(box_test,summary(fit1),summary(fit2)))}

#Función que calcula las desviaciones estándar y las bandas para los resultados en interanual
#Los argumentos son
#models_table: dataframe de los modelos
#n_sd: número de desviaciones estándar (one, two o three)
sd_interannual <- function(models_table,n_sd){
  #Calculando las desviaciones estándar
  tapply(models_table$error_interannual,list(models_table$model,models_table$step) , sd,na.rm=T)
  standard_deviation_interannual <-   tapply(models_table$error_interannual,list(models_table$model,models_table$step) , sd,na.rm=T)
  number_of_steps <- 6
  models_table$standard_deviation_interannual <- NA
  n_models <- models_table%>%select(model)%>%unique()%>%nrow()
  #Armando el dataframe
  for (i in 1:n_models){
    models_table[models_table%>%select(model)==models_table%>%select(model)%>%unique()%>%slice(i)%>%as.character()&
                   models_table%>%select(step)!="in_sample","standard_deviation_interannual"] <- rep(standard_deviation_interannual[i,-1],nrow(models_table%>%filter(step!="in_sample",model==models_table%>%select(model)%>%unique()%>%slice(i)%>%as.character()))/number_of_steps)
  }
  #Calculando las bandas
  models_table$lower_bound_interannual_1 <-models_table$predicted_interannual-models_table$standard_deviation_interannual
  models_table$upper_bound_interannual_1 <-models_table$predicted_interannual+models_table$standard_deviation_interannual
  models_table$lower_bound_interannual_2 <-models_table$predicted_interannual-2*models_table$standard_deviation_interannual
  models_table$upper_bound_interannual_2 <-models_table$predicted_interannual+2*models_table$standard_deviation_interannual
  models_table$lower_bound_interannual_3 <-models_table$predicted_interannual-3*models_table$standard_deviation_interannual
  models_table$upper_bound_interannual_3 <-models_table$predicted_interannual+3*models_table$standard_deviation_interannual
  
  return(models_table)
}

#Función que calcula los hits para los resultados en variación interanual
hits_interannual <- function(models_table){
  #Calculando los hits para una desviación estándar
  lower_bound_hit <- models_table[models_table%>%select(step)!="in_sample","lower_bound_interannual_1"]<=models_table[models_table%>%select(step)!="in_sample","realized_value_interannual"]
  upper_bound_hit <-models_table[models_table%>%select(step)!="in_sample","realized_value_interannual"]<=models_table[models_table%>%select(step)!="in_sample","upper_bound_interannual_1"]
  hit <- lower_bound_hit&upper_bound_hit
  hit <-hit%>%as.numeric()
  models_table[models_table%>%select(step)!="in_sample","hit_1"] <- hit
  #Calculando los hits para dos desviaciones estándar
  lower_bound_hit <- models_table[models_table%>%select(step)!="in_sample","lower_bound_interannual_2"]<=models_table[models_table%>%select(step)!="in_sample","realized_value_interannual"]
  upper_bound_hit <-models_table[models_table%>%select(step)!="in_sample","realized_value_interannual"]<=models_table[models_table%>%select(step)!="in_sample","upper_bound_interannual_2"]
  hit <- lower_bound_hit&upper_bound_hit
  hit <-hit%>%as.numeric()
  models_table[models_table%>%select(step)!="in_sample","hit_2"] <- hit
  #Calculando los hits para tres desviaciones estándar
  lower_bound_hit <- models_table[models_table%>%select(step)!="in_sample","lower_bound_interannual_3"]<=models_table[models_table%>%select(step)!="in_sample","realized_value_interannual"]
  upper_bound_hit <-models_table[models_table%>%select(step)!="in_sample","realized_value_interannual"]<=models_table[models_table%>%select(step)!="in_sample","upper_bound_interannual_3"]
  hit <- lower_bound_hit&upper_bound_hit
  hit <-hit%>%as.numeric()
  models_table[models_table%>%select(step)!="in_sample","hit_3"] <- hit
  return(models_table)}

#Esta función, con el método de estimación "CSS", realiza:
#1) Time series cross-validation del SARIMA
#2) Calcula las desviaciones estándar, las bandas y los hits
#Los argumentos son
#model_data: series de tiempo
#l_test_set: largo del test set
#forecast_horizon: horizonte de predicción:
#reg_order: orden regular del SARIMA
#seas_order: orden estacional del SARIMA
sarima_table_function2 <- function(model_data,l_test_set,forecast_horizon,reg_order,seas_order){
  #Definimos variables
  in_sample <- list()
  l_test_set <- l_test_set 
  forecast_horizon <- forecast_horizon
  forecasts_df <- matrix(ncol=forecast_horizon,nrow = l_test_set)
  forecasts_df <- data.frame(forecasts_df)
  forecast_errors_df <- matrix(ncol=forecast_horizon,nrow = l_test_set)
  forecast_errors_df <- data.frame(forecasts_df)
  
  colnames(forecasts_df)<- c("step 1","step 2","step 3","step 4","step 5","step 6")
  #Hacemos los calculos del time series cross-validation de estimación y generación de predicciones para cada interacción
  for (i in 1:l_test_set){
    data <- window(model_data,end=tail(index(model_data),l_test_set+forecast_horizon)[i])
    fit_model <- Arima(data,order=reg_order,seasonal = list(order=seas_order,period=12),include.mean = T,method="CSS")
    forecasts_df[i,] <- forecast(fit_model,h=forecast_horizon)$mean
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
    forecast_errors_df[i,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[i,]
    in_sample[[i]] <-cbind(fitted(fit_model),residuals(fit_model),rep(i,length(fitted(fit_model))))
  }
  #Armando la parte in-sample del dataframe
  in_sample_data.frame <- cbind(index(in_sample[[1]]),in_sample[[1]])
  for (i in 2:l_test_set){
    in_sample_data.frame <-rbind(in_sample_data.frame,cbind(index(in_sample[[i]]),in_sample[[i]]))
  }
  in_sample_data.frame <-in_sample_data.frame%>%as.data.frame()
  in_sample_data.frame$realized_value <- in_sample_data.frame[,2]+in_sample_data.frame[,3]
  colnames(in_sample_data.frame)[2]<- 'predicted'
  colnames(in_sample_data.frame)[3] <- 'error'
  colnames(in_sample_data.frame)[1] <- 'date'
  in_sample_data.frame$step <- "in_sample"
  in_sample_data.frame$standard_deviation <- NA
  in_sample_data.frame$train_test <- "train"
  colnames(in_sample_data.frame)[4] <- 'interaction'
  
  #Armando la parte out-of-sample del dataframe
  forecasts_for_tables <- forecasts_df[1,]%>%t()%>%as.data.frame()%>%rownames_to_column()
  start_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+1]
  end_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+forecast_horizon]
  realized_value <- window(model_data,start=start_series,end=end_series)
  forecasts_for_tables$date <- realized_value%>%index()
  forecasts_for_tables$realized_value <- realized_value%>%as.numeric()
  start_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+1] 
  end_series <- tail(index(model_data),l_test_set+forecast_horizon)[1+forecast_horizon]
  forecast_errors_df[1,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[1,]
  forecasts_for_tables$error <- forecast_errors_df[1,]%>%as.numeric()
  names(forecasts_for_tables)[names(forecasts_for_tables) == '1'] <- 'predicted'
  forecasts_for_tables$interaction <-1
  
  #Terminando de armar la parte out-of-sample del dataframe
  for (i in 2:l_test_set){
    forecasts_for_tables_bla <- forecasts_df[i,]%>%t()%>%as.data.frame()%>%rownames_to_column()
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
    realized_value <- window(model_data,start=start_series,end=end_series)
    forecasts_for_tables_bla$date <- realized_value%>%index()
    forecasts_for_tables_bla$realized_value <- realized_value%>%as.numeric()
    start_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+1]
    end_series <- tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon]
    forecast_errors_df[i,] <- window(model_data,start=start_series,end=end_series)%>%as.numeric()-forecasts_df[i,]
    forecasts_for_tables_bla$error <- forecast_errors_df[i,]%>%as.numeric()
    forecasts_for_tables_bla$interaction <- i
    names(forecasts_for_tables_bla)[names(forecasts_for_tables_bla) == i%>%as.character()] <- 'predicted'
    forecasts_for_tables <- rbind(forecasts_for_tables,forecasts_for_tables_bla)
  }
  #Calculando las desviaciones estándar
  tapply(forecasts_for_tables$error, forecasts_for_tables$rowname, sd)
  standard_deviation <- tapply(forecasts_for_tables$error, forecasts_for_tables$rowname, sd)
  number_of_steps <- 6
  forecasts_for_tables$standard_deviation <- rep(standard_deviation,nrow(forecasts_for_tables)/number_of_steps)
  forecasts_for_tables$train_test <- "test"
  names(forecasts_for_tables)[names(forecasts_for_tables) == 'rowname'] <- 'step'
  
  model_table <- rbind(in_sample_data.frame,forecasts_for_tables)
  
  #Armando las bandas
  model_table$lower_bound_one <-model_table$predicted-model_table$standard_deviation
  model_table$upper_bound_one <-model_table$predicted+model_table$standard_deviation
  model_table$lower_bound_two <-model_table$predicted-2*model_table$standard_deviation
  model_table$upper_bound_two <-model_table$predicted+2*model_table$standard_deviation
  model_table$lower_bound_three <-model_table$predicted-3*model_table$standard_deviation
  model_table$upper_bound_three <-model_table$predicted+3*model_table$standard_deviation
  
  #Calculando los hits
  source("auxiliares/sd_hit_single.R")
  temporary <- sd_hit_single(model_table,"one")
  model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(model_table,"two")
  model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(model_table,"three")
  model_table <- temporary[[1]]
  
  
  model_table <-model_table%>%as.data.frame()
  model_table
  return(model_table)
}


#Esta función realiza:
#1) Time series cross-validation del Direct Forecast Regression
#2) Cálculo de las desviaciones estándar, las bandas y los hits
#Los argumentos son:
#model_data: la serie de tiempo
#l_test_set: largo del test set
#forecast_horizon: horizonte de predicción
direct_forecast_regression_table_function <- function(model_data,l_test_set,forecast_horizon){
  #Definimos variables
  in_sample <- list()
  forecasts_df <- matrix(ncol=6,nrow = 60)
  forecasts_df <- data.frame(forecasts_df)
  forecast_errors_df <- matrix(ncol=6,nrow = 60)
  forecast_errors_df <- data.frame(forecasts_df)
  
  colnames(forecasts_df)<- c("step 1","step 2","step 3","step 4","step 5","step 6")
  #Estimación del modelo y generación de predicciones para cada interacción
  for(j in 1:forecast_horizon){
    for(i in 1:l_test_set){
      data <- window(model_data,end=tail(index(model_data),l_test_set+forecast_horizon)[i])
      x<-data%>%as.tibble()%>%slice(-nrow(data):(-nrow(data)+j-1))%>%as.ts()%>%as.numeric()
      y<-data%>%lag_vec(-j)%>%na.remove()%>%as.ts()%>%as.numeric()
      fit_model <- lm(y~x)
      newdata <- data.frame(x=y[length(y)])
      
      input <- c(1,newdata)
      some_vector<-fit_model$coefficients*input%>%unlist()%>%as.numeric()
      forecasts_df[i,j] <- some_vector%>%sum()
      forecast_errors_df[i,j] <- window(model_data,start=tail(index(model_data),l_test_set+forecast_horizon)[i+j],end=tail(index(model_data),l_test_set+forecast_horizon)[i+j])%>%as.numeric()-forecasts_df[i,j]
      in_sample[[i]] <-cbind(fitted(fit_model),residuals(fit_model),index(data)[-1:(-j)],as.numeric(data[-1:(-j)]),rep(i,length(fitted(fit_model))))
    }}
  #Armando la parte in-sample del dataframe
  in_sample_data.frame <- cbind(index(in_sample[[1]]),in_sample[[1]])
  for (i in 2:l_test_set){
    in_sample_data.frame <-rbind(in_sample_data.frame,cbind(index(in_sample[[i]]),in_sample[[i]]))
  }
  in_sample_data.frame <-in_sample_data.frame%>%as.data.frame()
  
  colnames(in_sample_data.frame)[2] <- 'predicted'
  colnames(in_sample_data.frame)[3] <- 'error'
  colnames(in_sample_data.frame)[4] <- 'date'
  colnames(in_sample_data.frame)[5] <- 'realized_value'
  colnames(in_sample_data.frame)[6] <- 'interaction'
  in_sample_data.frame <- subset(in_sample_data.frame, select = -1 )
  in_sample_data.frame$step <- "in_sample"
  in_sample_data.frame$standard_deviation <- NA
  in_sample_data.frame$train_test <- "train"
  
  #Armando la parte out-of-sample del dataframe
  forecasts_for_tables <- forecasts_df[1,]%>%t()%>%as.data.frame()%>%rownames_to_column()
  realized_value <- window(model_data,start=tail(index(model_data),l_test_set+forecast_horizon)[1+1],end=tail(index(model_data),l_test_set+forecast_horizon)[1+forecast_horizon])
  forecasts_for_tables$date <- realized_value%>%index()
  forecasts_for_tables$realized_value <- realized_value%>%as.numeric()
  forecast_errors_df[1,] <- window(model_data,start=tail(index(model_data),l_test_set+forecast_horizon)[1+1],end=tail(index(model_data),l_test_set+forecast_horizon)[1+forecast_horizon])%>%as.numeric()-forecasts_df[1,]
  forecasts_for_tables$error <- forecast_errors_df[1,]%>%as.numeric()
  names(forecasts_for_tables)[names(forecasts_for_tables) == '1'] <- 'predicted'
  forecasts_for_tables$interaction <-1
  
  #Siguiendo armando la parte out-of-sample del dataframe
  for (i in 2:l_test_set){
    forecasts_for_tables_bla <- forecasts_df[i,]%>%t()%>%as.data.frame()%>%rownames_to_column()
    realized_value <- window(model_data,start=tail(index(model_data),l_test_set+forecast_horizon)[i+1],end=tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon])
    forecasts_for_tables_bla$date <- realized_value%>%index()
    forecasts_for_tables_bla$realized_value <- realized_value%>%as.numeric()
    forecast_errors_df[i,] <- window(model_data,start=tail(index(model_data),l_test_set+forecast_horizon)[i+1],end=tail(index(model_data),l_test_set+forecast_horizon)[i+forecast_horizon])%>%as.numeric()-forecasts_df[i,]
    forecasts_for_tables_bla$error <- forecast_errors_df[i,]%>%as.numeric()
    forecasts_for_tables_bla$interaction <- i
    names(forecasts_for_tables_bla)[names(forecasts_for_tables_bla) == i%>%as.character()] <- 'predicted'
    forecasts_for_tables <- rbind(forecasts_for_tables,forecasts_for_tables_bla)
  }
  #Calculando las desviaciones estándar
  tapply(forecasts_for_tables$error, forecasts_for_tables$rowname, sd)#It seems there is something wrong with the MonteCarlo model code
  standard_deviation <- tapply(forecasts_for_tables$error, forecasts_for_tables$rowname, sd)
  number_of_steps <- 6
  forecasts_for_tables$standard_deviation <- rep(standard_deviation,nrow(forecasts_for_tables)/number_of_steps)
  forecasts_for_tables$train_test <- "test"
  names(forecasts_for_tables)[names(forecasts_for_tables) == 'rowname'] <- 'step'
  
  colnames(in_sample_data.frame)[1] <- "predicted"
  
  direct_forecast_regression_model_table <- rbind(in_sample_data.frame,forecasts_for_tables)
  
  #Calculando las bandas
  direct_forecast_regression_model_table$lower_bound_one <-direct_forecast_regression_model_table$predicted-direct_forecast_regression_model_table$standard_deviation
  direct_forecast_regression_model_table$upper_bound_one <-direct_forecast_regression_model_table$predicted+direct_forecast_regression_model_table$standard_deviation
  direct_forecast_regression_model_table$lower_bound_two <-direct_forecast_regression_model_table$predicted-2*direct_forecast_regression_model_table$standard_deviation
  direct_forecast_regression_model_table$upper_bound_two <-direct_forecast_regression_model_table$predicted+2*direct_forecast_regression_model_table$standard_deviation
  direct_forecast_regression_model_table$lower_bound_three <-direct_forecast_regression_model_table$predicted-3*direct_forecast_regression_model_table$standard_deviation
  direct_forecast_regression_model_table$upper_bound_three <-direct_forecast_regression_model_table$predicted+3*direct_forecast_regression_model_table$standard_deviation
  
  direct_forecast_regression_model_table$hit_one <- NA
  filter_mask <- direct_forecast_regression_model_table[,"step"]!="in_sample"
  filter_2 <- direct_forecast_regression_model_table[,"lower_bound_one"]%>%as.data.frame()<=direct_forecast_regression_model_table[,"realized_value"]%>%as.data.frame()
  filter_2 <-filter_2 & direct_forecast_regression_model_table[,"realized_value"]%>%as.data.frame()<=direct_forecast_regression_model_table[,"upper_bound_one"]%>%as.data.frame()
  filter_2[is.na(filter_2)]<- FALSE
  
  direct_forecast_regression_model_table[(filter_mask)&(filter_2),"hit_one"] <- 1
  filter_3 <- is.na(direct_forecast_regression_model_table[,"hit_one"]) & filter_mask
  
  direct_forecast_regression_model_table[filter_3,"hit_one"] <- 0
  
  
  n_out_of_sample <- 60
  direct_forecast_regression_model_table[filter_mask,"hit_one"]%>%sum()/direct_forecast_regression_model_table[filter_mask,"hit_one"]%>%nrow()
  proportion_hit_table <- matrix(nrow=162,ncol=2)
  proportion_hit_table <- proportion_hit_table%>%tibble()
  sum_hit_table<-direct_forecast_regression_model_table[filter_mask,]%>%group_by(step)%>%summarise(hit_proportion=sum(hit_one))%>%as.data.frame()#sum(hit)/length(hit))
  proportion_hit_table<-sum_hit_table[,"hit_proportion"]*100/n_out_of_sample
  proportion_hit_table<-cbind(sum_hit_table[,1:2],proportion_hit_table)
  print.data.frame(proportion_hit_table,digits=3)
  #Calculando los hits
  temporary <- sd_hit_single(direct_forecast_regression_model_table,"one")
  direct_forecast_regression_model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(direct_forecast_regression_model_table,"two")
  direct_forecast_regression_model_table <- temporary[[1]]
  
  temporary <- sd_hit_single(direct_forecast_regression_model_table,"three")
  direct_forecast_regression_model_table <- temporary[[1]]
  return(direct_forecast_regression_model_table)
}