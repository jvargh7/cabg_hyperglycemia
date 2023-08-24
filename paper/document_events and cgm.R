rm(list=ls());gc();source(".Rprofile")

# From chpre03_cgm harmonization.R -
cgm_long <- readRDS(paste0(path_metacabg_paper,"/working/data/cgm harmonization.RDS"))
bg_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/data/glucose_longitudinal.RDS"))
insulinbolus_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/data/insulinbolus_longitudinal.RDS"))
insulindrip_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/data/insulindrip_longitudinal.RDS"))

corrected_key_dates <- read_csv(paste0(path_metacabg_paper,"/working/data/corrected key observation dates.csv"))
surgery_cs <- readRDS(paste0(path_metacabg_paper,"/working/data/surgery_cs.RDS")) %>% 
  dplyr::select(record_id,surgery_start_time,surgery_end_time)

# unique_records <- surgery_cs$record_id # Do this for all records
unique_records <- unique(cgm_long$record_id)

# MCM013: LGA 8/2/2023 added; MCM013 - sensor did not passed from the warming up period. 
unique_records <- unique_records[!unique_records %in% c("MCM013")]

pdf(paste0(path_metacabg_paper,"/figures/events and cgm_",Sys.Date(),".pdf"),width=12,height=8)

for (u_r in unique_records){
  print(u_r)
  cgm_df <- cgm_long %>% 
    dplyr::filter(record_id == u_r)
  
  n_cgms = unique(cgm_df$cgm_id) %>% length()
  
  
  surgery_timestamps = surgery_cs %>% 
    dplyr::filter(!is.na(surgery_start_time)) %>% 
    dplyr::filter(record_id == u_r)
  

  insulinbolus_df = insulinbolus_longitudinal %>% 
    mutate(type = "Insulin Bolus",
           item = "Insulin") %>% 
    dplyr::filter(record_id == u_r)
  
  insulindrip_df = insulindrip_longitudinal %>% 
    dplyr::select(record_id,timestamp_start,units_value) %>% 
    mutate(type = "Insulin Drip",
           item = "Insulin") %>% 
    rename(value = units_value,
           timestamp = timestamp_start) %>% 
    dplyr::filter(record_id == u_r) 
  
  
  bg_df = bg_longitudinal %>% 
    dplyr::select(record_id,timestamp,value) %>% 
    mutate(type = "POCT Glucose",
           item = "Glucose") %>% 
    dplyr::filter(record_id == u_r) 
  
  p = ggplot()
  
  poct_df = bind_rows(insulinbolus_df,
                      insulindrip_df,
                      bg_df)
  
  p = p + geom_point(data = poct_df,
                     aes(x=timestamp,y=value,color=item,shape=type)) +
    ylab("") +
    xlab("Timestamp") +
    ggtitle(paste0("Patient: ",u_r))  +
    scale_shape_manual(name = "",values=c("POCT Glucose" = 1,"Insulin Drip" = 2, "Insulin Bolus" = 3)) +
    theme_bw()  +
    theme(legend.position = "bottom")
  
  color_values = c("red","blue")
  names(color_values) = c("Glucose","Insulin")
  
  timestamp_ranges = c(min(poct_df$timestamp), max(poct_df$timestamp))
  
  if(n_cgms > 0){
    
    names_cgms = unique(cgm_df$cgm_id)
    
    color_values = c("red","blue",grey.colors(n_cgms,rev = TRUE,start=0.25,end=0.75))
    names(color_values) = c("Glucose","Insulin",names_cgms)
    # color_values = c("Glucose","Insulin",names_cgms)
    # names(color_values) = c("red","blue",rainbow(n_cgms))
    
    timestamp_ranges = c(min(c(poct_df$timestamp,cgm_df$timestamp)), max(c(poct_df$timestamp,cgm_df$timestamp)))

    p = p + 
      # geom_path(data=cgm_df,aes(group=cgm_id,col=cgm_id,x=timestamp,y=sensorglucose)) 
      geom_point(data=cgm_df,aes(group=cgm_id,col=cgm_id,x=timestamp,y=sensorglucose)) 
    
  }
  
  p = p +
    scale_x_datetime(limits=timestamp_ranges,date_labels = "%d-%b (%H:%M)") +
    scale_color_manual(name="",values=color_values) +
    scale_y_continuous(limits = c(0,300),breaks=seq(0,300,by=50)) +
    geom_hline(yintercept=c(70,140),col="darkgreen",linetype=2) 
  
  if(nrow(surgery_timestamps)>0){
    p = p +
      geom_vline(xintercept = c(surgery_timestamps$surgery_start_time),col="black",linetype=2) +
      annotate("text",y=300,x=surgery_timestamps$surgery_start_time,hjust = 0,label = "Start of Surgery") +
      geom_vline(xintercept = c(surgery_timestamps$surgery_end_time),col="black",linetype=2) +
      annotate("text",y=280,x=surgery_timestamps$surgery_end_time,hjust = 0,label = "End of Surgery") 
    
  }
  
  p = p +
    theme(legend.position = "bottom")
  
  p %>% 
    print()
}

dev.off()
