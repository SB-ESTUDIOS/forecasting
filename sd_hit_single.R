sd_hit_single <- function(models_table,n_sd){
  models_table[,paste("hit_",n_sd,sep = "")] <- NA
  filter_mask <- models_table[,"step"]!="in_sample"
  filter_2 <- models_table[,paste("lower_bound_",n_sd,sep = "")]%>%as.data.frame()<=models_table[,"realized_value"]%>%as.data.frame()
  filter_2 <-filter_2 & models_table[,"realized_value"]%>%as.data.frame()<=models_table[,paste("upper_bound_",n_sd,sep = "")]%>%as.data.frame()
  filter_2[is.na(filter_2)]<- FALSE
  
  models_table[(filter_mask)&(filter_2),paste("hit_",n_sd,sep = "")] <- 1
  filter_3 <- is.na(models_table[,paste("hit_",n_sd,sep = "")]) & filter_mask
  
  models_table[filter_3,paste("hit_",n_sd,sep = "")] <- 0
  
  #table with hit proportions
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