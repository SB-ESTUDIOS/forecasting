regression_forecast_combination_function <- function(models_tables,n_comb_set){
  ols_f_data <- models_tables%>%filter(step!="in_sample",interaction<=n_comb_set,model!="Average")%>%select(predicted,model,realized_value,date,interaction)
  ols_f_data<-ols_f_data%>%spread(key = model, value = predicted)%>%as.data.frame()
  ols_f_data<-ols_f_data%>%arrange(interaction)
  # bla <- direct_forecast_regression_model_table%>%arrange(interaction)
  # bla <-bla%>%filter(step!="in_sample")
  # ols_f_data<-cbind(ols_f_data,bla$predicted)
  # colnames(ols_f_data)[ncol(ols_f_data)] <- "Direct Forecast"
  # ols_f_data <- ols_f_data[,-5]
  ols_f_data<-ols_f_data %>% mutate(
    
    dummy = case_when(
      
      date >= 2020.167 & date <= 2021.167
      ~ 1,
      
      TRUE ~ 0
    ))
  f_comb_model <- lm(realized_value~ `Best ARIMA`+`ETS`+`Best ETS`+`SARIMA`+dummy+`ETS`*dummy+`Best ETS`*dummy+
                       `Best ARIMA`*dummy+ `SARIMA`*dummy-1,data = ols_f_data)#+`Double Naive`
  summary(f_comb_model)
  f_comb_coef <- f_comb_model$coefficients
  
  ols_f_oos_data_keep <- models_tables%>%filter(step!="in_sample",interaction==n_comb_set+1)%>%select(predicted,model,realized_value,date,interaction,step)
  ols_f_oos_data_keep <- ols_f_oos_data_keep%>%arrange(desc(interaction))
  ols_f_oos_data <- models_tables%>%filter(step!="in_sample",interaction==n_comb_set+1)%>%select(predicted,model,realized_value,date,interaction,step)
  ols_f_oos_data<-ols_f_oos_data%>%spread(key = model, value = predicted)%>%as.data.frame()
  ols_f_oos_data<-ols_f_oos_data%>%arrange(desc(interaction))
  ols_f_oos_data$predicted <- sweep(ols_f_oos_data%>%select(5:8), MARGIN=2, f_comb_coef[1:4], `*`)%>%
    apply(1,sum)
  ols_f_oos_data$error <- ols_f_oos_data$realized_value - ols_f_oos_data$predicted

  # complete_data <- rbind(models_tables,) step!="in_sample",
  ols_f_oos_data <- subset( ols_f_oos_data, select = -c(`Best ARIMA`,`Best ETS`,`ETS`,`SARIMA`) )
  
  in_sample_data_keep <- models_tables%>%filter(step=="in_sample",interaction==n_comb_set+1)%>%select(predicted,model,realized_value,date,interaction,step)
  #in_sample_data_keep <- in_sample_data_data_keep%>%arrange(desc(interaction))
  in_sample_data <- models_tables%>%filter(step=="in_sample",interaction==n_comb_set+1)%>%select(predicted,model,realized_value,date,interaction,step)
  in_sample_data<-in_sample_data%>%spread(key = model, value = predicted)%>%as.data.frame()
  in_sample_data<-in_sample_data%>%arrange(date,interaction)
  in_sample_data$predicted <- sweep(in_sample_data%>%select(5:8), MARGIN=2, f_comb_coef[1:4], `*`)%>%
    apply(1,sum)
  in_sample_data$error <- in_sample_data$realized_value - in_sample_data$predicted
  # 
  in_sample_data <- subset( in_sample_data, select = -c(`Best ARIMA`,`Best ETS`,`ETS`,`SARIMA`) )
  reg_comb_table <- rbind(in_sample_data,ols_f_oos_data)
  return(list(reg_comb_table,f_comb_coef[1:4]))
}
