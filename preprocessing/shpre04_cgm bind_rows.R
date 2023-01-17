inputdirectory = paste0(path_sh_folder,"/Glucose and Insulin Data/raw/CGM/")
files = list.files(inputdirectory)
files = files[regexpr("\\.csv",files)>0]

cgm_summary <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_summary.RDS"))

cgm_long = map_dfr(files,
                   function(f){
                     df = read_csv(paste0(inputdirectory,f)) %>% 
                       mutate(sensorglucose = case_when(sensorglucose == "Low" ~ NA_real_,
                                                        TRUE ~ as.numeric(sensorglucose)),
                              file = f,
                              subject_id = str_replace(f,"\\.csv","")) %>% 
                       dplyr::select(-one_of("transmitter_id"));
                     
                     if(is.character(df$timestamp)){
                       df$timestamp = mdy_hms(df$timestamp)
                     };
                     
                     return(df)
                  })

# Filter to only devices with at least 80% CGM wear
cgm_long %>% 
  dplyr::filter(subject_id %in% cgm_summary[as.numeric(cgm_summary$percent_cgm_wear)>80,]$subject_id) %>%
saveRDS(.,paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_long.RDS"))
