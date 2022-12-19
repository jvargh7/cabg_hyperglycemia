

# Since some individuals had multiple devices ---------
files_list <- list.files(paste0(path_sh_folder,"/Glucose and Insulin Data/raw/CGM/TEMP"))

for (f in files_list){
  print(f);
  df = read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/raw/CGM/TEMP/",f)) %>% 
    rename_at(vars(contains("Transmitter")),~"transmitter_id")
  
  names(df)[1:2] <- c("timestamp","sensorglucose")
  
  
  if("transmitter_id" %in% colnames(df)){
    unique_devices = unique(df$transmitter_id)
    for(u in unique_devices){
      
      df %>% 
        dplyr::filter(transmitter_id == u) %>% 
        write_csv(.,paste0(path_sh_folder,"/Glucose and Insulin Data/raw/CGM/",str_replace(f,"\\.csv",paste0("_",u,".csv"))))
      
    }
  } else{
    df %>%
      write_csv(.,paste0(path_sh_folder,"/Glucose and Insulin Data/raw/CGM/",f))
  }
  
  
  
  
}
