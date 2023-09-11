rm(list=ls());gc();source(".Rprofile")

# Stress Hyperglycemia -------------
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
            n_bg_ge180 = sum(value>=180),
            mean_time_since_surgery_end_mins = mean(time_since_surgery_end)) %>% 
  mutate(stress_hyperglycemia = case_when(mean_bg >= 140 ~ 1,
                                          mean_bg < 140 ~ 0,
                                          TRUE ~ NA_real_),
         
         stress_hyperglycemia_def2 = case_when(n_bg_ge140 >= 2 ~ 1,
                                               n_bg_ge180 >= 1 ~ 1,
                                               TRUE ~ 0))

table(sh_outcome_period$stress_hyperglycemia)
table(sh_outcome_period$stress_hyperglycemia_def2)

saveRDS(sh_outcome_period,paste0(path_metacabg_paper,"/working/data/stress hyperglycemia 4 to 24h.RDS"))
write_csv(sh_outcome_period,paste0(path_metacabg_paper,"/working/data/stress hyperglycemia 4 to 24h.csv"))

# Atrial Fibrilliation: Anytime after surgery but before discharge -----------
afib <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS")) %>% 
  dplyr::select(record_id, cardiac_any, cardiac_cadnew, cardiac_cadprevious,
                cardiac_afib, cardiac_otherarrhythmia, cardiac_cardiomyopathy,
                cardiac_chf, cardiac_valvulopathy, cardiac_previousmi) %>% 
  mutate(across(contains("cardiac"),~case_when(. %in% c("Yes","Checked") ~ 1,
                                               . %in% c("No","Unchecked") ~ 0,
                                               TRUE ~ NA_real_))) %>% 
  dplyr::filter(!is.na(cardiac_any))

table(afib$cardiac_afib)

summary(afib)
saveRDS(afib,paste0(path_metacabg_paper,"/working/data/afib.RDS"))
write_csv(afib,paste0(path_metacabg_paper,"/working/data/afib.csv"))

# Acute Kidney Injury -------------

aki <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS")) %>% 
  dplyr::select(record_id, event_name, date_lab_tests, creatinine) %>% 
  dplyr::filter(!is.na(creatinine)) %>% 
  mutate(event_name = factor(event_name,levels=c("screening","surgery",paste0("post",1:19)))) %>% 
  group_by(record_id) %>% 
  mutate(baseline_creatinine = case_when(event_name == "screening" ~ creatinine,
                                         TRUE ~ NA_real_)) %>% 
  mutate(baseline_creatinine = zoo::na.locf(baseline_creatinine)) %>% 
  mutate(absolute_change_from_baseline = creatinine - baseline_creatinine,
         relative_change_from_baseline = creatinine/baseline_creatinine) %>% 
  mutate(aki_status = case_when(absolute_change_from_baseline >= 0.3 ~ 1,
                         relative_change_from_baseline >= 1.5 ~ 1,
                         TRUE ~ 0)) %>% 
  ungroup()



(fig_aki = aki %>% 
    mutate(aki_status = factor(aki_status, levels=c(0,1),labels=c("No","Yes"))) %>%  
    ggplot(data=.,aes(x=event_name,y=creatinine,group=record_id,col=aki_status)) +
  geom_point() +
  geom_path() +
  scale_color_manual(name = "AKI", values=c("No"="grey80","Yes"="red")) +
  theme_bw() +
  xlab("Event Name") +
  ylab("Creatinine") +
  theme(legend.position = "bottom")) %>% 
  ggsave(plot=.,filename=paste0(path_metacabg_paper,"/figures/Creatinine over time.png"),width=10,height=6)


unique_aki <- aki %>% 
  group_by(record_id) %>% 
  summarize(aki_status = max(aki_status)) %>% 
  ungroup()
table(unique_aki$aki_status)
