corrected_key_dates <- read_csv(paste0(path_metacabg_paper,"/working/corrected key observation dates.csv"))



add_key_dates <- function(df){
  
  df %>% 
    left_join(corrected_key_dates,
              by=c("record_id","event_name")) %>% 
    # Grouping to identify next day
    group_by(record_id,event_name) %>% 
    # Borrowed from aha abstract/shaha01_glucose during and after surgery.R ---------
    mutate(# Checking if serial observations are of lower time --> indicates next day starts
    is_lower_time = case_when(time < dplyr::lag(time,1) ~ 1,
                              TRUE ~ 0)) %>% 
    mutate(added_days = cumsum(is_lower_time)) %>% 
    mutate(date_measurement =  date_event_name + days(added_days)) %>% 
    ungroup()  %>% 
    dplyr::select(-is_lower_time,-added_days,-date_event_name) %>% 
    return()
  
}




bg_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/metacabg_20230706.RDS")) %>% 
  dplyr::select(record_id,event_name,
                matches("(or_bg|cgmbg_bg|meal_bg)")) %>% 
  dplyr::select(-matches("(gt|lt|ge|le)")) %>% 
  pivot_longer(cols=-one_of(c("record_id","event_name")),
               # https://stackoverflow.com/questions/61940984/using-pivot-longer-with-multiple-paired-columns-in-the-wide-dataset
               names_to=c("domain",".value","index"),
               names_pattern="(.*_bg)_(.*)_(.*)"
               ) %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(time = as_hms(time)) %>% 
  add_key_dates(.) %>% 
  mutate(timestamp = lubridate::as_datetime(paste0(date_measurement," ",time)))

insulindrip_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/metacabg_20230706.RDS")) %>% 
  dplyr::select(record_id,event_name,
                matches("(or_idr|icu48_idr)")) %>% 
  dplyr::select(-matches("(gt|lt|ge|le)")) %>% 
  pivot_longer(cols=-one_of(c("record_id","event_name")),
               # https://stackoverflow.com/questions/61940984/using-pivot-longer-with-multiple-paired-columns-in-the-wide-dataset
               names_to=c("domain",".value","index"),
               names_pattern="(.*_idr)_(.*)_([0-9]+)"
  ) %>% 
  dplyr::filter(!is.na(rate_value)) %>% 
  mutate(across(one_of("time_start","time_stop"),~as_hms(.))) %>% 
  mutate(time = time_start) %>% 
  add_key_dates(.) %>% 
  # Redundant variable 
  dplyr::select(-time) %>% 
  mutate(date_stop = case_when(time_start > time_stop ~ date_measurement + days(1),
                                TRUE ~ date_measurement)) %>% 
  mutate(timestamp_start = lubridate::as_datetime(paste0(date_measurement," ",time_start)),
         timestamp_stop = lubridate::as_datetime(paste0(date_stop," ",time_stop)))

insulinbolus_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/metacabg_20230706.RDS")) %>% 
  dplyr::select(record_id,event_name,
                matches("(or_ibolus|icu48_isq)")) %>% 
  dplyr::select(-matches("(gt|lt|ge|le)")) %>% 
  pivot_longer(cols=-one_of(c("record_id","event_name")),
               # https://stackoverflow.com/questions/61940984/using-pivot-longer-with-multiple-paired-columns-in-the-wide-dataset
               names_to=c("domain",".value","index"),
               names_pattern="(or_ibolus|icu48_isq)_(.*)_([0-9]+)"
  ) %>% 
  dplyr::filter(!is.na(value))  %>% 
  mutate(time = as_hms(time)) %>% 
  add_key_dates(.) %>% 
  mutate(timestamp = lubridate::as_datetime(paste0(date_measurement," ",time)))


saveRDS(bg_longitudinal,paste0(path_metacabg_paper,"/working/bg_longitudinal.RDS"))
saveRDS(insulinbolus_longitudinal,paste0(path_metacabg_paper,"/working/insulinbolus_longitudinal.RDS"))
saveRDS(insulindrip_longitudinal,paste0(path_metacabg_paper,"/working/insulindrip_longitudinal.RDS"))


