source("analysis/sha_figure df harmonized.R")


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


# Combined CGM and Glucose Insulin Plot --------

unique_records_combined <- timestamp_ranges$record_id
unique_files_combined <- timestamp_ranges$file
library(ggpubr)
source("C:/code/external/functions/cgm/plot_agp.R")
dt_surgery <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dt_surgery.RDS"))


pdf(paste0(path_sh_folder,"/Glucose and Insulin Data/figures/figure_cgm and glucose insulin over time.pdf"),width=12,height=8)

for(i in 1:nrow(timestamp_ranges)){
  
  u_r_df <- fig_df %>% 
    dplyr::filter(record_id == unique_records_combined[i]) %>% 
    mutate(timestamp = as_datetime(timestamp))
  
  surgery_timestamps = dt_surgery %>% 
    dplyr::filter(record_id == unique_records_combined[i])
  
  if(nrow(surgery_timestamps) == 0){
    surgery_timestamps$surgery_start_time = NA
    surgery_timestamps$surgery_end_time = NA
    
  }
  
  inputdirectory = paste0(path_sh_folder,"/Glucose and Insulin Data/raw/CGM/")
  
  cgm_df = read_csv(paste0(inputdirectory,unique_files_combined[i])) %>% 
    mutate(sensorglucose = case_when(sensorglucose == "Low" ~ NA_real_,
                                     TRUE ~ as.numeric(sensorglucose)) )
  
  if(!is.POSIXct(cgm_df$timestamp) & ! is.Date(cgm_df$timestamp)){
    cgm_df = cgm_df %>% 
      mutate(timestamp = mdy_hms(timestamp))
  }
  
  min_timestamp = min(c(cgm_df$timestamp,u_r_df$timestamp,surgery_timestamps$surgery_start_time),na.rm = TRUE)
  max_timestamp = max(c(cgm_df$timestamp,u_r_df$timestamp,surgery_timestamps$surgery_end_time),na.rm=TRUE)
  
  
  
  if(nrow(u_r_df)>0){
    figA = u_r_df %>% 
      ggplot(data=.,
             aes(x=timestamp,y=value,col=variable,shape=type))+ 
      geom_point() +
      geom_path(aes(linetype=location)) +
      theme_bw() +
      ylab("") +
      xlab("Timestamp") +
      ggtitle(paste0("Patient: ",unique_records_combined[i])) +
      scale_color_manual(name="",values=c("Glucose"="red","Insulin"="darkblue")) +
      scale_shape_manual(name="",values=c("POCT"=1,"IV"=2,"Bolus"=3,"Subcutaneous"=4,"Meal"=5)) +
      scale_linetype_manual(name="",values=c("ICU" = 2,"OR"=1)) +
      scale_y_continuous(limits=c(0,300),breaks=seq(0,300,by=50)) +
      scale_x_datetime(date_labels = "%d-%b (%H:%M)",limits = c(min_timestamp,max_timestamp)) +
      geom_hline(yintercept = c(54,250),
                 color="orange") +
      geom_hline(yintercept = c(70,180),
                 color="darkgreen") +
      geom_vline(xintercept = c(surgery_timestamps$surgery_start_time),col="black",linetype=2) +
      annotate("text",y=300,x=surgery_timestamps$surgery_start_time,hjust = 0,label = "Start of Surgery") +
      geom_vline(xintercept = c(surgery_timestamps$surgery_end_time),col="black",linetype=2) +
      annotate("text",y=280,x=surgery_timestamps$surgery_end_time,hjust = 0,label = "End of Surgery") +
      theme(legend.position = "bottom")
    
    # CGM Plot -----------
    
    figB = cgm_df  %>% 
      plot_agp(.,title=str_replace(unique_files_combined[i],"\\.csv",""),y_label = "Sensor Glucose (mg/dL)",
               x_limits = c(min_timestamp,max_timestamp)) +
      geom_vline(xintercept = c(surgery_timestamps$surgery_start_time),col="black",linetype=2) +
      annotate("text",y=300,x=surgery_timestamps$surgery_start_time,hjust = 0,label = "Start of Surgery") +
      geom_vline(xintercept = c(surgery_timestamps$surgery_end_time),col="black",linetype=2) +
      annotate("text",y=280,x=surgery_timestamps$surgery_end_time,hjust = 0,label = "End of Surgery") 
    
    ggarrange(figA,figB,
              nrow=2,ncol=1) %>% 
      print(.)
    
    
  }
  
  
}


dev.off()






