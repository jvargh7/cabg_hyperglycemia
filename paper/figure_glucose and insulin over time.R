require(lubridate)
or_to_icu_glucose <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/or_to_icu_glucose.RDS")) %>% 
  mutate(timestamp = paste0(date_surgery," ",time_or) %>% ymd_hms(.)) %>% ungroup()
or_to_icu_ivinsulin <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/or_to_icu_ivinsulin.RDS"))  %>% 
  mutate(timestamp = paste0(date_surgery," ",time_idrstart) %>% ymd_hms(.))  %>% ungroup()
or_to_icu_bolus <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/or_to_icu_bolus.RDS")) %>% 
  mutate(timestamp = paste0(date_surgery," ",time_bolus) %>% ymd_hms(.))  %>% ungroup()



icu48h_glucose <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/icu48h_glucose.RDS"))  %>% 
  mutate(timestamp = paste0(date_calendar," ",time_icu) %>% ymd_hms(.)) %>% ungroup()
icu48h_ivinsulin <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/icu48h_ivinsulin.RDS"))  %>% 
  mutate(timestamp = paste0(date_calendar," ",time_idrstart) %>% ymd_hms(.)) %>% ungroup()


icu48h_subqinsulin <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/icu48h_subqinsulin.RDS")) %>% 
  mutate(timestamp = paste0(date_icuadmission," ",time_sq) %>% ymd_hms(.))
  
icu48h_poctmeals <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/icu48h_poctmeals.RDS")) %>% 
  mutate(timestamp = paste0(date2," ",time) %>% ymd_hms(.))



unique_records <- unique(c(or_to_icu_glucose$record_id,or_to_icu_ivinsulin$record_id,or_to_icu_bolus$record_id,
                        icu48h_glucose$record_id,icu48h_ivinsulin$record_id,icu48h_subqinsulin$record_id,icu48h_poctmeals$record_id))


fig_df <- bind_rows(
  or_to_icu_glucose %>% 
    dplyr::select(timestamp,record_id,glucose_or) %>% 
    rename(value = glucose_or) %>% 
    mutate(variable = "Glucose",
           location = "OR",
           type = "POCT"),
  or_to_icu_ivinsulin %>% 
    dplyr::select(timestamp,record_id,insulin_iv) %>% 
    rename(value = insulin_iv) %>% 
    mutate(variable = "Insulin",
           location = "OR",
           type = "IV"),
  or_to_icu_bolus %>% 
    dplyr::select(timestamp,record_id,insulin_bolus) %>% 
    rename(value = insulin_bolus) %>% 
    mutate(variable = "Insulin",
           location = "OR",
           type = "Bolus"),
  
  
  icu48h_glucose %>% 
    dplyr::select(timestamp,record_id,glucose_icu) %>% 
    rename(value = glucose_icu) %>% 
    mutate(variable = "Glucose",
           location = "ICU",
           type = "POCT"),
  icu48h_ivinsulin %>% 
    dplyr::select(timestamp,record_id,insulin_iv) %>% 
    rename(value = insulin_iv) %>% 
    mutate(variable = "Insulin",
           location = "ICU",
           type = "IV"),
  
  icu48h_subqinsulin %>% 
    dplyr::select(timestamp,record_id,insulin_sq) %>% 
    rename(value = insulin_sq) %>% 
    mutate(variable = "Insulin",
           location = "ICU",
           type = "Subcutaneous"),
  
  icu48h_poctmeals %>% 
    dplyr::select(timestamp,record_id,glucose) %>% 
    rename(value = glucose) %>% 
    mutate(variable = "Glucose",
           location = "ICU",
           type = "Meal"),
  
  icu48h_poctmeals %>% 
    dplyr::select(timestamp,record_id,insulinsupp) %>% 
    rename(value = insulinsupp) %>% 
    dplyr::filter(!is.na(value)) %>% 
    mutate(variable = "Insulin",
           location = "ICU",
           type = "Meal")
    
  
  
) %>% 
  arrange(timestamp,record_id)


pdf(paste0(path_sh_folder,"/Glucose and Insulin Data/figures/figure_glucose and insulin over time.pdf"),width=12,height=8)

for(u_r in unique_records){
  
  u_r_df <- fig_df %>% dplyr::filter(record_id == u_r) %>% mutate(timestamp = as_datetime(timestamp))
  if(nrow(u_r_df)>0){
    fig = u_r_df %>% 
      ggplot(data=,
           aes(x=timestamp,y=value,col=variable,shape=type))+ 
      geom_point() +
      geom_path(aes(linetype=location)) +
      theme_bw() +
      ylab("") +
      xlab("Timestamp") +
      ggtitle(paste0("Patient: ",u_r)) +
      scale_color_manual(name="",values=c("Glucose"="red","Insulin"="darkblue")) +
      scale_shape_manual(name="",values=c("POCT"=1,"IV"=2,"Bolus"=3,"Subcutaneous"=4,"Meal"=5)) +
      scale_linetype_manual(name="",values=c("ICU" = 2,"OR"=1)) +
      scale_y_continuous(limits=c(0,250),breaks=seq(0,250,by=50)) +
      scale_x_datetime(date_labels = "%d-%b (%H:%M)")
    
    fig %>% 
      print(.)
  }
  

}


dev.off()




