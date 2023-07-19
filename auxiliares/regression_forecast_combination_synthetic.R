regression_forecast_combination_synthetic <- function(models_tables,n_comb_set){
  ols_f_data <- models_tables%>%filter(step!="in_sample",interaction<=n_comb_set,synthetic<=100)%>%select(predicted,model,realized_value,date,interaction,synthetic)
  ols_f_data<-ols_f_data%>%spread(key = model, value = predicted)%>%as.data.frame()#,model!="Average"
  ols_f_data<-ols_f_data%>%arrange(interaction,synthetic)
  # bla <- direct_forecast_regression_model_table%>%arrange(interaction)
  # bla <-bla%>%filter(step!="in_sample")
  # ols_f_data<-cbind(ols_f_data,bla$predicted)
  # colnames(ols_f_data)[ncol(ols_f_data)] <- "Direct Forecast"
  # ols_f_data <- ols_f_data[,-5]
  ols_f_data<-ols_f_data %>% mutate(
    
    dummy = case_when(
      
      date >= 2019.167 & date <= 2020.000
      ~ 1,
      
      TRUE ~ 0
    ))
  f_comb_model <- lm(realized_value~ `Best ARIMA`+`Best ETS`+dummy+`Best ARIMA`*dummy+`Best ETS`*dummy-1+`ETS`+`SARIMA`+`ETS`*dummy+ `SARIMA`*dummy,data = ols_f_data)#
 # 
  summary(f_comb_model)
  f_comb_coef <- f_comb_model$coefficients
                                                                                     
  ols_f_oos_data_keep <- models_tables%>%filter(step!="in_sample",interaction==n_comb_set+1)%>%filter(synthetic!=123,synthetic!=1000)%>%select(predicted,model,realized_value,date,interaction,step,synthetic)
  ols_f_oos_data_keep <- ols_f_oos_data_keep%>%arrange(desc(interaction))             
  ols_f_oos_data <- models_tables%>%filter(step!="in_sample",interaction==n_comb_set+1)%>%filter(synthetic!=123,synthetic!=1000)%>%select(predicted,model,realized_value,date,interaction,step,synthetic)
  ols_f_oos_data<-ols_f_oos_data%>%spread(key = model, value = predicted)%>%as.data.frame()
  ols_f_oos_data<-ols_f_oos_data%>%arrange(desc(interaction))
  ols_f_oos_data$predicted <- sweep(ols_f_oos_data%>%select(6:9), MARGIN=2, f_comb_coef[c(1,2,4,5)], `*`)%>%
    apply(1,sum)
  ols_f_oos_data$error <- ols_f_oos_data$realized_value - ols_f_oos_data$predicted
  ####
  # tapply(ols_f_oos_data$error, ols_f_oos_data$step, sd)
  # standard_deviation <- tapply(ols_f_oos_data$error, ols_f_oos_data$step, sd)
  # number_of_steps <- 6
  # ols_f_oos_data$standard_deviation <- rep(standard_deviation,nrow(ols_f_oos_data)/number_of_steps)
  # forecasts_for_tables$train_test <- "test"
  # ols_f_oos_data$lower_bound_one <-ols_f_oos_data$predicted-ols_f_oos_data$standard_deviation
  # ols_f_oos_data$upper_bound_one <-ols_f_oos_data$predicted+ols_f_oos_data$standard_deviation
  # ols_f_oos_data$lower_bound_two <-ols_f_oos_data$predicted-2*ols_f_oos_data$standard_deviation
  # ols_f_oos_data$upper_bound_two <-ols_f_oos_data$predicted+2*ols_f_oos_data$standard_deviation
  # ols_f_oos_data$lower_bound_three <-ols_f_oos_data$predicted-3*ols_f_oos_data$standard_deviation
  # ols_f_oos_data$upper_bound_three <-ols_f_oos_data$predicted+3*ols_f_oos_data$standard_deviation
  # 
  # source("sd_hit_single.R")
  # temporary <- sd_hit_single(ols_f_oos_data,"one")
  # ols_f_oos_data <- temporary[[1]]
  # 
  # temporary <- sd_hit_single(ols_f_oos_data,"two")
  # ols_f_oos_data <- temporary[[1]]
  # 
  # temporary <- sd_hit_single(ols_f_oos_data,"three")
  # ols_f_oos_data <- temporary[[1]]
  # #Let's do calculations for error table
  # error_table_sarima<-sapply(sarima_model_table%>%select(error),function(x) {c(mean(x),sd(x),quantile(x, probs = seq(0, 1, 0.25)))})
  # ols_f_oos_data <-ols_f_oos_data%>%as.data.frame()
  # ols_f_oos_data
  ####
  ols_f_oos_data <- subset( ols_f_oos_data, select = -c(`Best ARIMA`,`Best ETS`,`SARIMA`,`ETS`) )
  return(list(ols_f_oos_data,f_comb_coef[c(1,2,4,5)]))
}
