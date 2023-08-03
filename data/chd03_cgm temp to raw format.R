rm(list=ls());gc();source(".Rprofile")


# Since some individuals had multiple devices ---------
files_list <- list.files(paste0(path_sh_folder,"/raw/CGM/TEMP"))

for (f in files_list){
  print(f);
  df = read_csv(paste0(path_sh_folder,"/raw/CGM/TEMP/",f)) %>% 
    rename_at(vars(contains("Timestamp")),~"timestamp") %>% 
    rename_at(vars(contains("Glucose Value")),~"sensorglucose") %>% 
    rename_at(vars(contains("Glucose mg")),~"sensorglucose") %>% 
    rename_at(vars(matches("EGV")), ~"sensorglucose") %>% 
    rename_at(vars(contains("Transmitter ID")),~"transmitter_id") 
  

  if(f == "MCM032.csv"){
    
    df = df %>% 
      mutate(timestamp = case_when(transmitter_id == "25C7KL" ~ mdy_hm(timestamp),
                                   TRUE ~ ymd_hms(timestamp)))
    
    
  }
  
  if("transmitter_id" %in% colnames(df)){
    df <- df %>% 
      dplyr::filter(!is.na(transmitter_id)) %>% 
      # Manual fix for MCM054
      dplyr::mutate(transmitter_id = case_when(transmitter_id == "3.14e+53" ~ "314L51",
                                               TRUE ~ as.character(transmitter_id)))
      
      
    unique_devices = na.omit(unique(df$transmitter_id))
    for(u in unique_devices){
      
      df %>% 
        dplyr::filter(transmitter_id == u) %>% 
        dplyr::select(timestamp,sensorglucose) %>% 
        write_csv(.,paste0(path_sh_folder,"/raw/CGM/",str_replace(f,"\\.csv",paste0("_",as.character(u),".csv"))))
      
    }} else{
    df %>%
      write_csv(.,paste0(path_sh_folder,"/raw/CGM/",f))
  }
}
