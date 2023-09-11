corrected_key_dates <- read_csv(paste0(path_metacabg_paper,"/working/data/corrected key observation dates.csv"))

bg_longitudinal_saved <- readRDS(paste0(path_metacabg_paper,"/working/data/bg_longitudinal.RDS"))
insulinbolus_longitudinal_saved <- readRDS(paste0(path_metacabg_paper,"/working/data/insulinbolus_longitudinal.RDS"))
insulindrip_longitudinal_saved <- readRDS(paste0(path_metacabg_paper,"/working/data/insulindrip_longitudinal.RDS"))


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




bg_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS")) %>% 
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
  # FZT verified on 2023-07-26
  mutate(time = case_when(
                          record_id == "MCE001" & event_name == "post1" & domain == "meal_bg" & index == 3 ~ as_hms("18:03:00"),
                          record_id == "MCE001" & event_name == "post1" & domain == "meal_bg" & index == 4 ~ as_hms("22:16:00"),
                          record_id == "MCE001" & event_name == "post2" & domain == "meal_bg" & index == 1 ~ as_hms("06:49:00"),
                          record_id == "MCE001" & event_name == "post2" & domain == "meal_bg" & index == 2 ~ as_hms("11:29:00"),
                          record_id == "MCE001" & event_name == "post2" & domain == "meal_bg" & index == 3 ~ as_hms("17:41:00"),
                          record_id == "MCE001" & event_name == "post2" & domain == "meal_bg" & index == 4 ~ as_hms("21:35:00"),
                          
                          record_id == "MCE003" & event_name == "post2" & domain == "meal_bg" & index == 1 ~ as_hms("06:49:00"),
                          record_id == "MCE003" & event_name == "post2" & domain == "meal_bg" & index == 2 ~ as_hms("11:05:00"),
                          record_id == "MCE003" & event_name == "post2" & domain == "meal_bg" & index == 3 ~ as_hms("17:46:00"),
                          record_id == "MCE003" & event_name == "post2" & domain == "meal_bg" & index == 4 ~ as_hms("22:12:00"),
                          
                          record_id == "MCM001" & event_name == "post2" & domain == "meal_bg" & index == 2 ~ as_hms("13:09:00"),
                          record_id == "MCM001" & event_name == "post2" & domain == "meal_bg" & index == 3 ~ as_hms("16:48:00"),
                          record_id == "MCM001" & event_name == "post2" & domain == "meal_bg" & index == 4 ~ as_hms("21:06:00"),
                          
                          record_id == "MCM002" & event_name == "post2" & domain == "meal_bg" & index == 2 ~ as_hms("10:40:00"),
                          record_id == "MCM002" & event_name == "post2" & domain == "meal_bg" & index == 3 ~ as_hms("15:59:00"),
                          record_id == "MCM002" & event_name == "post2" & domain == "meal_bg" & index == 4 ~ as_hms("21:14:00"),
                          
                          
                          record_id == "MCM003" & event_name == "post2" & domain == "meal_bg" & index == 1 ~ as_hms("06:21:00"),
                          record_id == "MCM003" & event_name == "post2" & domain == "meal_bg" & index == 2 ~ as_hms("10:44:00"),
                          record_id == "MCM003" & event_name == "post2" & domain == "meal_bg" & index == 3 ~ as_hms("16:29:00"),
                          record_id == "MCM003" & event_name == "post2" & domain == "meal_bg" & index == 4 ~ as_hms("21:56:00"),
                          
                          record_id == "MCM009" & event_name == "post2" & domain == "meal_bg" & index == 2 ~ as_hms("11:46:00"),
                          record_id == "MCM009" & event_name == "post2" & domain == "meal_bg" & index == 3 ~ as_hms("16:22:00"),
                          record_id == "MCM009" & event_name == "post2" & domain == "meal_bg" & index == 4 ~ as_hms("21:33:00"),
                          
                          record_id == "MCM022" & event_name == "post2" & domain == "meal_bg" & index == 4 ~ as_hms("21:12:00"),
                          TRUE ~ time
                          )) %>% 
  
  
   
  add_key_dates(.) %>% 
  mutate(timestamp = lubridate::as_datetime(paste0(date_measurement," ",time)))



if(nrow(bg_longitudinal)>nrow(bg_longitudinal_saved)){
  
  
  (missing_timestamps <- anti_join(bg_longitudinal,bg_longitudinal_saved,by=c("record_id","event_name","domain","index")) %>% 
    dplyr::filter(!is.na(value),is.na(time)) %>% 
    dplyr::select(record_id,event_name,domain,index,time,value) ) %>% 
  writexl::write_xlsx(.,paste0(path_sh_folder,"/working/chpre02_QC Missing Times for POCT values_",Sys.Date(),".xlsx"))
  
}


insulindrip_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS")) %>% 
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

if(nrow(insulindrip_longitudinal)>nrow(insulindrip_longitudinal_saved)){
  
  
  (missing_timestamps <- anti_join(insulindrip_longitudinal,insulindrip_longitudinal_saved,by=c("record_id","event_name","domain","index")) %>% 
     dplyr::filter(!is.na(rate_value),is.na(time_start)|is.na(time_stop)) %>% 
     dplyr::select(record_id,event_name,domain,index,time_start,time_stop,rate_value) ) %>% 
    writexl::write_xlsx(.,paste0(path_sh_folder,"/working/chpre02_QC Missing Times for Insulin Drip values_",Sys.Date(),".xlsx"))
  
}

insulinbolus_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS")) %>% 
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

if(nrow(insulinbolus_longitudinal)>nrow(insulinbolus_longitudinal_saved)){
  
  
  (missing_timestamps <- anti_join(insulinbolus_longitudinal,insulinbolus_longitudinal_saved,by=c("record_id","event_name","domain","index")) %>% 
     dplyr::filter(!is.na(rate_value),is.na(time)) %>% 
     dplyr::select(record_id,event_name,domain,index,time,value) ) %>% 
    writexl::write_xlsx(.,paste0(path_sh_folder,"/working/chpre02_QC Missing Times for Insulin Bolus values_",Sys.Date(),".xlsx"))
  
}


labtests_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS")) %>% 
  dplyr::select(record_id,event_name, 
                creatinine, egfr, albumin,
                total_bilirubin, alt,
                alkaline_phosphatase, ast, wbc) %>% 
  left_join(corrected_key_dates,
            by=c("record_id","event_name")) %>% 
  left_join(corrected_key_dates %>% 
              dplyr::filter(event_name == "post1") %>% 
              dplyr::select(record_id,date_event_name) %>% 
              dplyr::rename(post1_date = date_event_name),
            by = "record_id") %>% 
  group_by(record_id) %>% 
  mutate(date_event_name = case_when(str_detect(event_name,"post") & is.na(date_event_name) ~ (post1_date + days(as.numeric(str_replace(event_name,"post","")))),
                                     TRUE ~ date_event_name)) %>% 
  ungroup() %>% 
  dplyr::select(-post1_date)




saveRDS(bg_longitudinal,paste0(path_metacabg_paper,"/working/data/bg_longitudinal.RDS"))
saveRDS(insulinbolus_longitudinal,paste0(path_metacabg_paper,"/working/data/insulinbolus_longitudinal.RDS"))
saveRDS(insulindrip_longitudinal,paste0(path_metacabg_paper,"/working/data/insulindrip_longitudinal.RDS"))
saveRDS(labtests_longitudinal,paste0(path_metacabg_paper,"/working/data/labtests_longitudinal.RDS"))

write_csv(bg_longitudinal,paste0(path_metacabg_paper,"/working/data/bg_longitudinal.csv"))
write_csv(insulinbolus_longitudinal,paste0(path_metacabg_paper,"/working/data/insulinbolus_longitudinal.csv"))
write_csv(insulindrip_longitudinal,paste0(path_metacabg_paper,"/working/data/insulindrip_longitudinal.csv"))
write_csv(labtests_longitudinal,paste0(path_metacabg_paper,"/working/data/labtests_longitudinal.csv"))




