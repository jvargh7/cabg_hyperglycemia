source(".Rprofile")
source("analysis/sha_figure df harmonized.R")
dt_surgery <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dt_surgery.RDS")) %>% 
  mutate(first24h_stop_time = surgery_end_time + hours(24),
         post72h_start_time = surgery_end_time + hours(72),
         post72h_end_time = surgery_end_time + hours(96))

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

# From sha02_cgm perioperative - after appropriate selections -----
cgm_selected <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_selected.RDS")) %>% 
  dplyr::select(record_id,subject_id,timestamp,phase,sensorglucose_corrected) %>% 
  group_by(record_id,subject_id,phase) %>% 
  summarize(n_cgm_observations = sum(!is.na(sensorglucose_corrected)),
         tir = mean(sensorglucose_corrected %in% c(70:180),na.rm=TRUE)*100,
         cv = sd(sensorglucose_corrected,na.rm=TRUE)/mean(sensorglucose_corrected,na.rm = TRUE)) %>% 
  dplyr::filter(n_cgm_observations > 30)

table(cgm_selected$phase)

longitudinal_df = full_join(phase_insulin,
                            cgm_selected,
                            by=c("record_id","phase")) %>% 
  dplyr::filter(!is.na(phase)) %>% 
  mutate(phase = factor(phase,levels=c("pre_surgery","surgery","post_24hours","post_25to71hours","post_72to96hours","post_remaining"),
                        labels=c("Pre Surgery","Surgery","Post \n24 hours","Post \n25-71 \nhours","Post \n72-96 \nhours","Post \n>96 \nhours"),
                        ordered=TRUE)) %>% 
  arrange(record_id,phase)



write_csv(longitudinal_df,paste0(path_sh_folder,"/Glucose and Insulin Data/working/longitudinal dataset for metabolomics.csv"))

figA = ggplot(data=longitudinal_df,
              aes(x=phase,y=cv,group=record_id)) +
  geom_point() +
  geom_path() +
  xlab("") +
  ylab("Coefficient of variation") +
  theme_bw()
figB = ggplot(data=longitudinal_df,
              aes(x=phase,y=tir,group=record_id)) +
  geom_point() +
  geom_path() +
  xlab("") +
  ylab("Time in Range (%)") +
  theme_bw()

figC = ggplot(data=longitudinal_df,
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
