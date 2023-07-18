sh_variables <- readxl::read_excel("data/Stress Hyperglycemia Variable List.xlsx",sheet="or_to_icu")


or_to_icu <- read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/raw/METABOCABG insulin requirements OR to ICU arrival_12.15.2022.csv")) %>% 
  rename_with(~ sh_variables$new_var[which(sh_variables$variable == .x)], 
              .cols = sh_variables$variable)  %>% 
  mutate(date_surgery = lubridate::mdy(date_surgery))




or_to_icu_glucose <- or_to_icu %>% 
  dplyr::select(record_id,date_surgery,
                starts_with("time_or"),starts_with("glucose_or")) %>% 
  pivot_longer(cols=-one_of("record_id","date_surgery"),names_to=c(".value","Var"),names_sep="[0-9]+$") %>% 
  dplyr::select(-Var) %>% 
  group_by(record_id,date_surgery) %>% 
  mutate(timepoint = 1:n()) %>% 
  dplyr::filter(!is.na(time_or))


or_to_icu_ivinsulin <- or_to_icu %>% 
  dplyr::select(record_id,date_surgery,
                starts_with("time_idr"),starts_with("insulin_idr"),starts_with("insulin_iv"))  %>% 
  pivot_longer(cols=-one_of("record_id","date_surgery"),names_to=c(".value","Var"),names_sep="[0-9]+$") %>% 
  dplyr::select(-Var) %>% 
  group_by(record_id,date_surgery) %>% 
  mutate(timepoint = 1:n()) %>% 
  dplyr::filter(!is.na(insulin_idr))


or_to_icu_bolus <- or_to_icu %>% 
  dplyr::select(record_id,date_surgery,contains("bolus"))  %>% 
  pivot_longer(cols=-one_of("record_id","date_surgery","total_insulin_bolus"),names_to=c(".value","Var"),names_sep="[0-9]+$") %>% 
  dplyr::select(-Var) %>% 
  dplyr::filter(!is.na(time_bolus))


saveRDS(or_to_icu,paste0(path_sh_folder,"/Glucose and Insulin Data/working/or_to_icu.RDS"))
saveRDS(or_to_icu_glucose,paste0(path_sh_folder,"/Glucose and Insulin Data/working/or_to_icu_glucose.RDS"))
saveRDS(or_to_icu_ivinsulin,paste0(path_sh_folder,"/Glucose and Insulin Data/working/or_to_icu_ivinsulin.RDS"))
saveRDS(or_to_icu_bolus,paste0(path_sh_folder,"/Glucose and Insulin Data/working/or_to_icu_bolus.RDS"))
