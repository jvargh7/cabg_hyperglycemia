rm(list=ls());gc();source(".Rprofile")

key_dates <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS")) %>% 
  dplyr::select(record_id,event_name,
                surgery_date,date_of_cabg_bg_monitoring,
                date_surgery_or_drips,
                cgmbg_comparison_date,
                icu48_date_monitoring,
                date_2) %>% 
  pivot_longer(cols=-one_of("record_id","event_name"),
               names_to="date_type",
               values_to="date") %>% 
  dplyr::filter(!is.na(date)) 



pre_qc_dates <- key_dates %>% 
  group_by(record_id,event_name) %>% 
  summarize(min_date = min(date),
            max_date = max(date)) %>% 
  dplyr::filter(min_date != max_date)

corrected_key_dates <- key_dates %>% 
  mutate(date = case_when(# Surgery was on 2019-06-13, Post 1 should be "2019-06-14 --> FZT verified on 2023-07-26
    record_id == "MCE004" & event_name == "post1" & date_type == "cgmbg_comparison_date" ~ ymd("2019-06-14"),
    # Incorrectly entered as "2019-08-04 --> FZT verified on 2023-07-26
    record_id == "MCE006" & event_name == "surgery" & date_type == "date_surgery_or_drips" ~ ymd("2019-09-04"),
    # Incorrectly entered as "2020-04-22 twice? --> FZT verified on 2023-07-26
    record_id == "MCG001" & event_name == "surgery" & date_type == "surgery_date" ~ ymd("2019-04-22"),
    
    record_id == "MCG001" & event_name == "surgery" & date_type == "icu48_date_monitoring" ~ ymd("2019-04-22"),
    # Incorrectly entered as "2019-04-12" --> FZT verified on 2023-07-26
    record_id == "MCM001" & event_name == "post1" & date_type == "icu48_date_monitoring" ~ ymd("2019-03-12"),
    
    # Incorrectly entered as "2019-08-08" - could be actually when it was monitored --> FZT verified on 2023-07-26
    record_id == "MCM005" & event_name == "post2" & date_type == "icu48_date_monitoring" ~ ymd("2019-08-07"),
    
    # Incorrectly entered as "2019-08-15" since date of surgery was "2019-08-15" --> FZT verified on 2023-07-26
    record_id == "MCM008" & event_name == "post1" & date_type == "cgmbg_comparison_date" ~ ymd("2019-08-16"),
    record_id == "MCM008" & event_name == "post1" & date_type == "icu48_date_monitoring" ~ ymd("2019-08-16"),
    
    # Incorrectly entered as "2020-08-04" since date of surgery was "2020-08-04"
    record_id == "MCE010" & event_name == "post1" & date_type == "icu48_date_monitoring" ~ ymd("2020-08-05"),
    
    # Incorrectly entered as "2019-08-16" --> FZT verified on 2023-07-26
    record_id == "MCM008" & event_name == "post2" & date_type == "cgmbg_comparison_date" ~ ymd("2019-08-17"),
    
    # Incorrectly entered as "2020-01-30" --> FZT verified on 2023-07-26
    record_id == "MCM012" & event_name == "post2" & date_type == "cgmbg_comparison_date" ~ ymd("2019-10-30"),
    
    # Incorrectly entered as "2020-01-29" --> FZT verified on 2023-07-26
    record_id == "MCM018" & event_name == "post1" & date_type == "icu48_date_monitoring" ~ ymd("2020-01-31"),
    
    # Incorrectly entered as 2030-03-14 --> FZT verified on 2023-07-26
    record_id == "MCM026" & event_name == "post1" & date_type == "cgmbg_comparison_date" ~ ymd("2020-03-14"),
    
    # Incorrectly entered as 2020-07-14 --> FZT verified on 2023-07-26
    record_id == "MCM028" & event_name == "surgery" & date_type == "surgery_date" ~ ymd("2020-07-16"),
    
    # Incorrectly entered as "2020-11-02" --> FZT verified on 2023-07-26
    record_id == "MCM041" & event_name == "surgery" & date_type == "surgery_date" ~ ymd("2020-11-03"),
    
    # Incorrectly entered as "2020-12-11" --> FZT verified on 2023-07-26
    record_id == "MCM046" & event_name == "surgery" & date_type == "icu48_date_monitoring" ~ ymd("2020-12-01"),
    
    # Incorrectly entered as "2021-01-27" --> FZT verified on 2023-07-26
    record_id == "MCM051" & event_name == "surgery" & date_type == "icu48_date_monitoring" ~ ymd("2021-01-26"),
    TRUE ~ date
  ))  %>% 
  distinct(record_id,event_name,date) %>% 
  rename(date_event_name = date)

post_qc_dates <- corrected_key_dates %>% 
  group_by(record_id,event_name) %>% 
  summarize(min_date = min(date_event_name),
            max_date = max(date_event_name)) %>% 
  dplyr::filter(min_date != max_date)

write_csv(corrected_key_dates,paste0(path_metacabg_paper,"/working/data/corrected key observation dates.csv"))


cgm_dates <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS")) %>% 
  dplyr::filter(event_name == "screening") %>% 
  dplyr::select(record_id,blinded_group,type_of_participation,
                cgm1_insertion_date_time,
                cgm2_insertion_date_time) %>% 
  mutate(across(matches("date_time"),~as_date(.))) %>% 
  # 2023-07-31: Based on LGA's comment 'Incorrect data' 
  mutate(cgm1_insertion_date_time = case_when(record_id %in% c("MCE007","MCM029") ~ NA_Date_,
                                         TRUE ~ cgm1_insertion_date_time)) %>% 
  dplyr::filter(!is.na(cgm1_insertion_date_time) | !is.na(cgm2_insertion_date_time)) 


corrected_key_dates %>% 
  pivot_wider(names_from=event_name,values_from=date_event_name) %>% 
  left_join(cgm_dates,
            by="record_id") %>% 
  write_csv(.,paste0(path_metacabg_paper,"/working/data/cgm insertion dates.csv"))
