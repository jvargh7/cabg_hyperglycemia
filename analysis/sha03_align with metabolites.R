source(".Rprofile")
source("analysis/sha_figure df harmonized.R")
dt_surgery <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dt_surgery.RDS")) %>% 
  mutate(first24h_stop_time = surgery_end_time + hours(24),
         post72h_start_time = surgery_end_time + hours(72),
         post72h_end_time = surgery_end_time + hours(96)) 

# Sum up insulin used in each phase -------
source("analysis/sha_phase insulin.R")

# From sha02_cgm perioperative - after appropriate selections -----
cgm_selected <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_selected.RDS")) %>% 
  dplyr::select(record_id,subject_id,timestamp,phase,sensorglucose_corrected) %>% 
  group_by(record_id,subject_id,phase) %>% 
  summarize(n_cgm_observations = sum(!is.na(sensorglucose_corrected)),
         tir = mean(sensorglucose_corrected %in% c(70:180),na.rm=TRUE)*100,
         cv = sd(sensorglucose_corrected,na.rm=TRUE)/mean(sensorglucose_corrected,na.rm = TRUE)) %>% 
  dplyr::filter(n_cgm_observations > 30) %>% 
  dplyr::filter(tir >= 25)

table(cgm_selected$phase)

# Longitudinal dataset -------
longitudinal_df = full_join(phase_insulin,
                            cgm_selected,
                            by=c("record_id","phase")) %>% 
  dplyr::filter(!is.na(phase)) %>% 
  mutate(phase = factor(phase,levels=c("pre_surgery","surgery","post_24hours","post_25to71hours","post_72to96hours","post_remaining"),
                        labels=c("Pre Surgery","Surgery","Post 24 hours","Post 25-71 hours","Post 72-96 hours","Post >96 hours"),
                        ordered=TRUE)) %>% 
  arrange(record_id,phase) %>% 
  dplyr::select(record_id,subject_id,phase,everything()) %>% 
  mutate(used_insulin = case_when(is.na(used_insulin) ~ 0,
                                  TRUE ~ used_insulin)) %>% 
  dplyr::filter(!is.na(tir))  %>% 
  group_by(record_id) %>% 
  mutate(preceding_insulin = case_when(phase == "Pre Surgery" ~ 0,
                                     phase == "Surgery" ~ 0,
                                     TRUE ~ dplyr::lag(used_insulin,1)))


longitudinal_df %>% 
  dplyr::filter(phase %in% c("Pre Surgery","Surgery","Post 24 hours","Post 72-96 hours")) %>% 
  write_csv(.,paste0(path_sh_folder,"/Glucose and Insulin Data/working/longitudinal dataset for metabolomics.csv"))

# Cross-sectional dataset ----------

cs_df = full_join(phase_insulin,
                  cgm_selected,
                  by=c("record_id","phase")) %>% 
  dplyr::filter(!is.na(phase)) %>% 
  dplyr::filter(phase %in% c("pre_surgery","surgery","post_24hours")) %>% 
  dplyr::select(record_id,phase,used_insulin,tir,cv) %>% 
  pivot_longer(cols=c("used_insulin","tir","cv"),names_to="variable",values_to="value") %>% 
  pivot_wider(names_from=c("phase","variable"),values_from="value") %>% 
  dplyr::select(record_id,pre_surgery_tir,pre_surgery_cv,
                surgery_tir,surgery_cv,
                post_24hours_tir,post_24hours_cv) %>% 
  dplyr::filter(!is.na(surgery_cv)) %>% 
  mutate(cv_change_group = case_when(surgery_cv < post_24hours_cv ~ "CV Increased",
                                     surgery_cv >= post_24hours_cv ~ "CV Decreased",
                                     is.na(surgery_cv) ~ NA_character_),
         tir_change_group = case_when(surgery_tir < post_24hours_tir ~ "TIR Increased",
                                     surgery_tir >= post_24hours_tir ~ "TIR Decreased",
                                     is.na(surgery_tir) ~ NA_character_)
         )

write_csv(cs_df,paste0(path_sh_folder,"/Glucose and Insulin Data/working/crosssectional dataset for metabolomics.csv"))

# longitudinal df plot ---------

longitudinal2_df = longitudinal_df %>% 
  dplyr::mutate(phase = factor(phase,
                               levels=c("Pre Surgery","Surgery","Post 24 hours","Post 25-71 hours","Post 72-96 hours","Post >96 hours"),
                               labels = c("Pre Surgery","Surgery","Post \n24 hours","Post \n25-71 \nhours","Post \n72-96 \nhours","Post \n>96 \nhours")))

c("Pre Surgery","Surgery","Post \n24 hours","Post \n25-71 \nhours","Post \n72-96 \nhours","Post \n>96 \nhours")

figA = ggplot(data=longitudinal2_df,
              aes(x=phase,y=cv,group=record_id)) +
  geom_point() +
  geom_path() +
  xlab("") +
  ylab("Coefficient of variation") +
  theme_bw()
figB = ggplot(data=longitudinal2_df,
              aes(x=phase,y=tir,group=record_id)) +
  geom_point() +
  geom_path() +
  xlab("") +
  ylab("Time in Range (%)") +
  theme_bw()

figC = ggplot(data=longitudinal2_df,
              aes(x=phase,y=used_insulin,group=record_id)) +
  geom_point() +
  geom_path() +
  xlab("") +
  ylab("Total Insulin") +
  theme_bw()

library(ggpubr)
ggarrange(figA,figB,figC,
       nrow=1,
       ncol=3,labels = c("A","B","C")) %>% 
  ggsave(.,filename=paste0(path_sh_folder,"/Glucose and Insulin Data/figures/figure_phase specific cgm and insulin summary.png"),width=12,height=4)
