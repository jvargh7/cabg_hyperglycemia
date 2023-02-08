source("analysis/sha_figure df harmonized.R")
dt_surgery <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dt_surgery.RDS")) %>% 
  mutate(first24h_stop_time = surgery_end_time + hours(24),
         post72h_start_time = surgery_end_time + hours(72),
         post72h_end_time = surgery_end_time + hours(96)) 

rm(icu48h_glucose,icu48h_ivinsulin,icu48h_poctmeals,icu48h_subqinsulin,
   or_to_icu_bolus,or_to_icu_glucose,or_to_icu_ivinsulin)

phase_insulin <- fig_df %>% 
  dplyr::filter(variable == "Insulin") %>% 
  left_join(dt_surgery,
            by = "record_id") %>% 
  mutate(phase = case_when(timestamp < surgery_start_time ~ "pre_surgery",
                           timestamp <= surgery_end_time ~ "surgery",
                           timestamp < (surgery_end_time + hours(24)) ~ "post_24hours",
                           timestamp < (surgery_end_time + hours(72)) ~ "post_25to71hours",
                           timestamp >= (surgery_end_time + hours(72)) & timestamp <= (surgery_end_time + hours(96)) ~ "post_72to96hours",
                           TRUE ~ "post_remaining")) %>% 
  group_by(record_id,phase) %>% 
  summarize(used_insulin = sum(value,na.rm=TRUE))

# rm(dt_surgery,fig_df)