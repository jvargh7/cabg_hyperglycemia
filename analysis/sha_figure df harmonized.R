require(lubridate)
or_to_icu_glucose <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/or_to_icu_glucose.RDS")) %>% 
  mutate(timestamp = paste0(date_surgery," ",time_or) %>% ymd_hms(.)) %>% ungroup()
or_to_icu_ivinsulin <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/or_to_icu_ivinsulin.RDS"))  %>% 
  mutate(timestamp = paste0(date_surgery," ",time_idrstart) %>% ymd_hms(.))  %>% ungroup()
or_to_icu_bolus <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/or_to_icu_bolus.RDS")) %>% 
  mutate(timestamp = paste0(date_surgery," ",time_bolus) %>% ymd_hms(.))  %>% ungroup()

timestamp_ranges = read_csv("data/cgm timestamp ranges.csv") %>% 
  mutate(record_id = str_replace(file,"(_|\\.).*","")) # %>% 
  # group_by(record_id) %>% 
  # dplyr::filter(n == min(n))

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