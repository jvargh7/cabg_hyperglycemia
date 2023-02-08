library(lubridate)
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
                     
                     if(f == "MCM009.csv"){
                       # Date of surgery: 2019-08-20 08:32:00
                       diff_date = min(df$timestamp) - ymd_hms("2019-08-20 08:32:00")
                       df = df %>% 
                         mutate(timestamp = timestamp - diff_date)
                       
                     }
                     
                     return(df)
                  })

length(unique(str_replace(cgm_summary$subject_id,"_[0-9A-Z]+","")))

# Filter to only devices with at least 80% CGM wear
cgm_long_80pct = cgm_long %>% 
  dplyr::filter(subject_id %in% cgm_summary[as.numeric(cgm_summary$percent_cgm_wear)>80,]$subject_id)
length(unique(str_replace(cgm_long_80pct$subject_id,"_[0-9A-Z]+","")))

saveRDS(cgm_long_80pct,paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_long.RDS"))
