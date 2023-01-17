source(".Rprofile")

lag_mins = 10

# From sha01_glucose before stress hyperglycemia.R-------
pre_insulin <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/pre_insulin.RDS"))
dt_surgery <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dt_surgery.RDS"))
all_insulin <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/all_insulin.RDS")) 

first_insulin <- all_insulin %>% 
  group_by(record_id) %>% 
  dplyr::filter(timestamp == min(timestamp)) 

total_insulin <- all_insulin %>% 
  group_by(record_id) %>% 
  summarize(total_insulin_units = sum(Insulin,na.rm=TRUE))

cgm_summary <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_summary.RDS"))
cgm_long <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_long.RDS")) %>% 
  mutate(record_id = str_replace(subject_id,"\\_[A-Z0-9]+",""))

# Pre-insulin glucose errors --------

glucose_diff = map_dfr(1:nrow(pre_insulin),
                   function(i){
                     df_x = pre_insulin[i,];
                     df_y = cgm_long %>% 
                       dplyr::filter(record_id == df_x$record_id) %>% 
                       dplyr::filter(timestamp > (df_x$timestamp+minutes(lag_mins))) %>% 
                       group_by(subject_id) %>% 
                       dplyr::filter(timestamp == min(timestamp)) %>% 
                       ungroup() %>% 
                       dplyr::select(timestamp,sensorglucose,subject_id) %>% 
                       rename(cgm_timestamp = timestamp,
                              cgm_glucose = sensorglucose)
                     
                  
                     
                     bind_cols(df_x %>% 
                                 dplyr::select(timestamp,record_id,Glucose) %>% 
                                 rename(poct_timestamp = timestamp,
                                        poct_glucose = Glucose),
                               df_y) %>% 
                       return(.)
                     
                     
                     
                     
                   })


glucose_diff %>% 
  dplyr::filter(abs(cgm_timestamp - poct_timestamp)<minutes(14)) %>%
  dplyr::mutate(pct_diff = abs(poct_glucose - cgm_glucose)*100/poct_glucose) %>% 
  # distinct(record_id) %>% nrow()
  write_csv(.,file="analysis/sha02_difference between poct and cgm.csv")


selected_devices = read_csv(file="analysis/sha02_difference between poct and cgm.csv") %>% 
  dplyr::filter(pct_diff < 30) %>% 
  distinct(subject_id,.keep_all=TRUE)

# Are there multiple devices for same record_id? 
selected_devices %>% 
  group_by(record_id) %>% 
  tally() %>% 
  dplyr::filter(n > 1)

cgm_selected <- cgm_long %>% 
  dplyr::filter(subject_id %in% selected_devices$subject_id) %>% 
  mutate(sensorglucose_corrected = case_when(sensorglucose < 54*0.5 ~ NA_real_,
                                   TRUE ~ sensorglucose)) %>% 
  left_join(
    first_insulin %>% 
      rename(first_ins_timestamp = timestamp) %>% 
      dplyr::select(record_id,first_ins_timestamp),
    by = "record_id"
  ) %>% 
  left_join(
    dt_surgery %>% 
      dplyr::select(record_id,contains("surgery")),
    by = "record_id"
  ) %>% 
  left_join(
    total_insulin,
    by = "record_id"
  ) %>% 
  mutate(phase = case_when(timestamp < surgery_start_time ~ "pre_surgery_obs",
                           timestamp < first_ins_timestamp ~ "surgery_preinsulin_obs",
                           timestamp < surgery_end_time ~ "surgery_postinsulin_obs",
                           TRUE ~ "post_surgery_obs"))

cgm_selected %>% 
  group_by(record_id,subject_id,total_insulin_units,phase) %>% 
  summarize(count_glucose = sum(!is.na(sensorglucose_corrected))) %>% 
  pivot_wider(names_from = phase,values_from=count_glucose) %>% 
  dplyr::select(record_id,subject_id,total_insulin_units,pre_surgery_obs, surgery_preinsulin_obs, surgery_postinsulin_obs, post_surgery_obs) %>% 
  write_csv(.,file="analysis/sha02_count of observations by phase for selected.csv")
