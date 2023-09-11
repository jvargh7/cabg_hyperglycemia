rm(list=ls());gc();source(".Rprofile")

# May need to run this script + document_events and cgm.R a couple of times to get an idea of whether CGMs are valid

inputdirectory = paste0(path_sh_folder,"/raw/CGM/")
files = list.files(inputdirectory)
files = files[regexpr("\\.csv",files)>0]

corrected_key_dates <- read_csv(paste0(path_metacabg_paper,"/working/data/corrected key observation dates.csv"))
cgm_insertion_dates <- read_csv(paste0(path_metacabg_paper,"/working/data/cgm insertion dates.csv"))

metacabg_cgm <- readxl::read_excel("data/CABG Hyperglycemia Variable List.xlsx",sheet="METACABG CGM")

# FZT: MCM043- is a screen failure. They never had CABG as their condition was successfully treated with PCI. CGMs were consequently removed.
files = files[!files %in% c("MCM043_31P53G.csv")]

cgm_long = map_dfr(files,
                   function(f){
                     print(f)
                     df = read_csv(paste0(inputdirectory,f)) %>% 
                       dplyr::select(timestamp,sensorglucose) %>% 
                       # Does not remove values that are below 27 mg/dL or where glucose changes by +/- 20%
                       mutate(sensorglucose = case_when(sensorglucose == "Low" ~ NA_real_,
                                                        # Low glucose cutoff comes from .Rprofile
                                                        as.numeric(sensorglucose) < low_glucose_cutoff ~ NA_real_,
                                                        TRUE ~ as.numeric(sensorglucose)),
                              file = f,
                              record_id = str_replace(f,"(_[A-Z0-9]+)*\\.csv",""),
                              cgm_id = str_replace(f,"\\.csv","")) %>% 
                       dplyr::select(-one_of("transmitter_id")) %>% 
                       mutate(cgm_type = case_when(cgm_id %in% metacabg_cgm$cgm1_id ~ "CGM 1",
                                                   cgm_id %in% metacabg_cgm$cgm2_id ~ "CGM 2",
                                                   TRUE ~ NA_character_)) %>% 
                       left_join(metacabg_cgm %>% 
                                   dplyr::select(cgm1_id,cgm1_status),
                                 by = c("cgm_id" = "cgm1_id"));
                     
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




saveRDS(cgm_long,paste0(path_metacabg_paper,"/working/data/cgm harmonization.RDS"))
write_csv(cgm_long,paste0(path_metacabg_paper,"/working/data/cgm harmonization.csv"))
# cgm_long <- readRDS(paste0(path_metacabg_paper,"/working/data/cgm harmonization.RDS"))
