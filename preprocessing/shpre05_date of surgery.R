
dt_variables <- readxl::read_excel("data/Stress Hyperglycemia Variable List.xlsx",sheet="dt_surgery")


dt_surgery <- read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/raw/METABOCABG-SurgeryStartAndStopT_DATA_LABELS_2023-02-14.csv"))  %>% 
  rename_with(~ dt_variables$new_var[which(dt_variables$variable == .x)], 
              .cols = dt_variables$variable) %>% 
  mutate_at(vars(contains("time")),~lubridate::mdy_hm(.)) %>%
  mutate(surgery_start_time = case_when(record_id == "MCM045" ~ mdy_hm("11/17/2020 15:12"),
                                        TRUE ~ surgery_start_time),
         surgery_end_time = case_when(record_id == "MCM045" ~ mdy_hm("11/17/2020 19:15"),
                                        TRUE ~ surgery_end_time)) %>% 
  bind_rows(data.frame(record_id = "MCM019",
                       surgery_start_time = mdy_hm("01/30/2020 16:18"),
                       surgery_end_time = mdy_hm("01/30/2020 19:05")
                       ) %>% 
              mutate(duration_surgery = 60*as.numeric((surgery_end_time - surgery_start_time))))
  
  
  
saveRDS(dt_surgery,paste0(path_sh_folder,"/Glucose and Insulin Data/working/dt_surgery.RDS"))
