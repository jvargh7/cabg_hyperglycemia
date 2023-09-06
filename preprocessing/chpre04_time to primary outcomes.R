rm(list=ls());gc();source(".Rprofile")

# This comes from ~/preprocessing/chpre01_metacabg cross sectional data.R
# Recommend running the below line if last run was long ago
source("preprocessing/chpre01_metacabg cross sectional data.R")
rm(post1,post2,screening,metacabg,surgery)
surgery_timestamps <- readRDS(paste0(path_metacabg_paper,"/working/data/surgery_cs.RDS")) %>% 
  dplyr::select(record_id,surgery_start_time,surgery_end_time)

# From ~/preprocessing/chpre02_metacabg longitudinal data.R
bg_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/data/bg_longitudinal.RDS"))

bg_outcome_period <- bg_longitudinal %>% 
  left_join(surgery_timestamps,
            by = "record_id") %>% 
  dplyr::filter(timestamp >= (surgery_end_time + hours(4)), timestamp <= (surgery_end_time + hours(24))) %>% 
  mutate(time_since_surgery_end = as.numeric(difftime(timestamp,surgery_end_time,units="mins")))

unique(bg_outcome_period$record_id) %>% length()

sh_outcome_period <- bg_outcome_period %>% 
  group_by(record_id) %>% 
  summarize(mean_bg = mean(value),
            n_bg = n(),
            n_bg_ge140 = sum(value>=140),
            mean_time_since_surgery_end_mins = mean(time_since_surgery_end)) %>% 
  mutate(stress_hyperglycemia = case_when(mean_bg >= 140 ~ 1,
                                          mean_bg < 140 ~ 0,
                                          TRUE ~ NA_real_))

table(sh_outcome_period$stress_hyperglycemia)

saveRDS(sh_outcome_period,paste0(path_metacabg_paper,"/working/data/stress hyperglycemia 4 to 24h.RDS"))
write_csv(sh_outcome_period,paste0(path_metacabg_paper,"/working/data/stress hyperglycemia 4 to 24h.csv"))
