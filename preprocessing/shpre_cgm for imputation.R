
# From shpre04_cgm bind_rows.R ----------
# 39 CGMs become 38 after dropping one with <80% wear 
dt_surgery <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dt_surgery.RDS")) %>% 
  mutate(first24h_stop_time = surgery_end_time + hours(24),
         post72h_start_time = surgery_end_time + hours(72),
         post72h_end_time = surgery_end_time + hours(96)) 

cgm_long <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_long removing Low.RDS")) %>% 
  mutate(record_id = str_replace(subject_id,"\\_[A-Z0-9]+","")) %>% 
  group_by(subject_id) %>% 
  mutate(cgm_glucose = case_when(sensorglucose < 54*0.5 ~ NA_real_,
                                 sensorglucose < dplyr::lag(sensorglucose,1)*0.8 ~ NA_real_,
                                 sensorglucose > dplyr::lag(sensorglucose,1)*1.2 ~ NA_real_,
                                 TRUE ~ sensorglucose
  )) %>% 
  ungroup() 
# cgm_long$record_id[!cgm_long$record_id %in% dt_surgery$record_id] %>% unique()
# [1] "MCM026" x 1 "MCM046" x 2
cgm_for_imputation = cgm_long %>% 
  left_join(dt_surgery,
            by = "record_id") %>% 
  dplyr::filter(!is.na(surgery_start_time)) %>% 
  mutate(phase = case_when(timestamp < surgery_start_time ~ "pre_surgery",
                           timestamp <= surgery_end_time ~ "surgery",
                           timestamp < (surgery_end_time + hours(24)) ~ "post_24hours",
                           timestamp < (surgery_end_time + hours(72)) ~ "post_25to71hours",
                           timestamp >= (surgery_end_time + hours(72)) & timestamp <= (surgery_end_time + hours(96)) ~ "post_72to96hours",
                           TRUE ~ "post_remaining"))

unique_devices = unique(cgm_for_imputation$subject_id)
