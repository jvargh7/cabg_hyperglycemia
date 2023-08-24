
# One variable --> unique_id_var

## Derived from cgmanalysis:: cgmvariables function
agp_indicators <- function(cgm_dataset,id_vars = character()){

  
  agp_df <- cgm_dataset %>% 
    group_by_at(vars(one_of(id_vars))) %>% 
    mutate(interval = abs(pracma::Mode(as.numeric(base::diff(timestamp))))*60,
           totaltime = as.numeric(difftime(max(timestamp),min(timestamp),units="secs"))) %>% 
    ungroup() %>% 
    group_by_at(vars(one_of(c(id_vars)))) %>% 
    dplyr::summarize(date_cgm_placement = min(timestamp),
                     date_cgm_last = max(timestamp),
                     total_sensor_readings = sum(which(!is.na(sensorglucose))),
                     percent_na = 100*sum(is.na(sensorglucose))/n(),
                     percent_cgm_wear = 100*sum(!is.na(sensorglucose))/mean(totaltime/interval),
                     num_days_good_data = n()/(86400/mean(interval)),
                     average_sensor = mean(sensorglucose,na.rm=TRUE),
                     estimated_a1c = (46.7 + mean(sensorglucose,na.rm=TRUE))/(28.7),
                     gmi = (3.31 + (0.02392*mean(sensorglucose,na.rm=TRUE))),
                     min_sensor = min(sensorglucose,na.rm = TRUE),
                     q1_sensor = quantile(sensorglucose,probs=0.25,na.rm=TRUE),
                     median_sensor = quantile(sensorglucose,probs=0.5,na.rm=TRUE),
                     q3_sensor = quantile(sensorglucose,probs=0.75,na.rm=TRUE),
                     max_sensor = min(sensorglucose,na.rm = TRUE),
                     standard_deviation = sd(sensorglucose,na.rm=TRUE),
                     cv = sd(sensorglucose,na.rm=TRUE)/mean(sensorglucose,na.rm=TRUE),
                     percent_time_70_140 = 100*mean(sensorglucose %in% c(70:140),na.rm=TRUE),
                     percent_time_lt_54 = 100*mean(sensorglucose < 54,na.rm=TRUE),
                     percent_time_54_69 = 100*mean(sensorglucose %in% c(54:69),na.rm=TRUE),
                     percent_time_70_180 = 100*mean(sensorglucose %in% c(70:180),na.rm=TRUE),
                     percent_time_181_250 = 100*mean(sensorglucose %in% c(181:250),na.rm=TRUE),
                     percent_time_gt_250 = 100*mean(sensorglucose > 250,na.rm=TRUE)
                     
                     
    )
  
  return(agp_df)
                      
  
  
}