source(".Rprofile")
library(lubridate)

source("analysis/sha_figure df harmonized.R")

# Based on call with FJP on 17 Jan 2023 to select closest CGM glucose value after POCT for alignment
lag_mins = 0
max_mins = 4

# From sha01_glucose before stress hyperglycemia.R-------
# Values during surgery, i.e location == OR
pre_insulin <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/pre_insulin.RDS"))
dt_surgery <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dt_surgery.RDS"))
all_insulin <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/all_insulin.RDS"))
all_glucose <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/all_glucose.RDS"))

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

glucose_diff = map_dfr(1:nrow(all_glucose),
                   function(i){
                     df_x = all_glucose[i,];
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
                                 dplyr::select(timestamp,record_id,Glucose,location,type) %>% 
                                 rename(poct_timestamp = timestamp,
                                        poct_glucose = Glucose),
                               df_y) %>% 
                       return(.)
                     
                     
                     
                     
                   })


glucose_diff %>% 
  dplyr::filter(abs(cgm_timestamp - poct_timestamp)<minutes(max_mins)) %>%
  dplyr::mutate(pct_diff = abs(poct_glucose - cgm_glucose)*100/poct_glucose) %>% 
  # distinct(record_id) %>% nrow()
  write_csv(.,file="analysis/sha02_difference between poct and cgm.csv")

# Before 24-Jan meeting: At least 1 glucose measurement within 30% range
# selected_devices = read_csv(file="analysis/sha02_difference between poct and cgm.csv") %>% 
#   dplyr::filter(pct_diff < 30) %>% 
#   dplyr::filter(n() > 1) %>% 
#   distinct(subject_id,.keep_all=TRUE)
# # Are there multiple devices for same record_id? 
# selected_devices %>% 
#   group_by(record_id) %>% 
#   tally() %>% 
#   dplyr::filter(n > 1)

read_csv(file="analysis/sha02_difference between poct and cgm.csv") %>% 
  group_by(record_id,subject_id) %>% 
  summarize(proportion = mean(pct_diff < 30),
            n = n()) %>% 
  write_csv(.,file = "analysis/sha02_record_ids with counts of pct_diff lt 30 between poct and cgm.csv")
# %>% 
#   dplyr::filter(count > 0.5)
selected_devices = read_csv("analysis/sha02_record_ids with counts of pct_diff lt 30 between poct and cgm.csv") %>% 
  dplyr::filter(proportion >= 0.5) %>% 
  dplyr::select(record_id,subject_id) 



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
  mutate(phase = case_when(timestamp < surgery_start_time ~ "pre_surgery",
                           timestamp <= surgery_end_time ~ "surgery",
                           timestamp < (surgery_end_time + hours(24)) ~ "post_24hours",
                           timestamp < (surgery_end_time + hours(72)) ~ "post_25to71hours",
                           timestamp >= (surgery_end_time + hours(72)) & timestamp <= (surgery_end_time + hours(96)) ~ "post_72to96hours",
                           TRUE ~ "post_remaining"))
saveRDS(cgm_selected,paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_selected.RDS"))

cgm_selected %>% 
  group_by(record_id,subject_id,total_insulin_units,phase) %>% 
  summarize(count_glucose = sum(!is.na(sensorglucose_corrected))) %>% 
  pivot_wider(names_from = phase,values_from=count_glucose) %>% 
  dplyr::select(record_id,subject_id,total_insulin_units,pre_surgery, surgery, post_24hours, post_25to71hours,post_72to96hours,post_remaining) %>% 
  write_csv(.,file="analysis/sha02_count of observations by phase for selected.csv")
