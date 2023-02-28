
D = m = 10
dataset_for_calibration <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dataset for calibration.RDS")) %>% 
  bind_rows(.) 

estimated_glycemic_variability = dataset_for_calibration %>% 
  mutate(pred_var = ((fit - lwr)/1.96)^2) %>% 
  dplyr::filter(fit > 0) %>% 
  group_by(timestamp,subject_id,record_id,phase) %>% 
  summarize(theta_D = mean(fit),
            B_D = var(fit),
            W_D = mean(pred_var)) %>% 
  ungroup() %>% 
  # C:/code/external/functions/imputation/adjusted_ci.R
  mutate(T_D = W_D + (1 + 1/D)*B_D,
         gamma_D = (1 + 1/D)*(B_D/T_D),
         # nu = (D-1)*((1+ (1/(D+1))*(W_D/B_D))^2),
         nu2 = (D-1)/(gamma_D)^2 # equivalent to mice's dfold; (D/(D+1)) and not (1/(D+1))
         # nu_improved = mice:::barnard.rubin(D,B_D,T_D,dfcom = dfcom) 
  ) %>% 
  mutate(L = theta_D + qt(p = 0.025,df = nu2)*((T_D)^((1/2))),
         U = theta_D + qt(p = 0.975,df = nu2)*((T_D)^((1/2))),
         sqrt_T_D = ((T_D)^((1/2)))) %>% 
  
  mutate(prob_70to140 = pnorm(140,theta_D,sqrt_T_D)) %>% 
  group_by(subject_id,record_id,phase) %>% 
  summarize(TIR = mean(prob_70to140,na.rm=TRUE),
            mean_cgm = mean(theta_D,na.rm=TRUE),
            sd_cgm = sqrt((1 + 1/D)*var(theta_D,na.rm=TRUE) + mean(B_D,na.rm=TRUE)),
            n = sum(!is.na(theta_D))) %>% 
  mutate(se_TIR = sqrt(TIR*(1-TIR)/n),
         CV_cgm = sd_cgm/mean_cgm)

length(unique(estimated_glycemic_variability$subject_id))
length(unique(estimated_glycemic_variability$record_id))

source("analysis/sha_phase insulin.R")

longitudinal_df = estimated_glycemic_variability %>% 
  group_by(record_id,phase) %>% 
  summarize(tir = max(TIR,na.rm=TRUE)*100,
            cv = min(CV_cgm,na.rm=TRUE)) %>% 
  full_join(phase_insulin,
            by=c("record_id","phase")) %>% 
  dplyr::filter(!is.na(phase)) %>% 
  mutate(phase = factor(phase,levels=c("pre_surgery","surgery","post_24hours","post_25to71hours","post_72to96hours","post_remaining"),
                        labels=c("Pre Surgery","Surgery","Post 24 hours","Post 25-71 hours","Post 72-96 hours","Post >96 hours"),
                        ordered=TRUE)) %>% 
  arrange(record_id,phase) %>% 
  dplyr::select(record_id,phase,everything()) %>% 
  mutate(used_insulin = case_when(is.na(used_insulin) ~ 0,
                                  TRUE ~ used_insulin)) %>% 
  dplyr::filter(!is.na(tir))  %>% 
  group_by(record_id) %>% 
  mutate(preceding_insulin = case_when(phase == "Pre Surgery" ~ 0,
                                       phase == "Surgery" ~ 0,
                                       TRUE ~ dplyr::lag(used_insulin,1)))

length(unique(longitudinal_df$record_id))

longitudinal_df %>% 
  dplyr::filter(phase %in% c("Pre Surgery","Surgery","Post 24 hours","Post 72-96 hours")) %>% 
  write_csv(.,paste0(path_sh_folder,"/Glucose and Insulin Data/working/longitudinal dataset for metabolomics after imputation.csv"))


cs_df = estimated_glycemic_variability %>% 
  group_by(record_id,phase) %>% 
  summarize(tir = max(TIR,na.rm=TRUE)*100,
            cv = min(CV_cgm,na.rm=TRUE)) %>% 
  full_join(phase_insulin,
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
  mutate(cv_group = case_when(post_24hours_cv > 0.1 ~ "CV > 0.1",
                              post_24hours_cv <= 0.1 ~ "CV <= 0.1",
                              TRUE ~ NA_character_),
         tir_group = case_when(post_24hours_tir < 70 ~ "TIR < 70%",
                               post_24hours_tir >= 70 ~ "TIR >= 70%",
                               TRUE ~ NA_character_)
  )

write_csv(cs_df,paste0(path_sh_folder,"/Glucose and Insulin Data/working/crosssectional dataset for metabolomics after imputation.csv"))


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
  theme_bw() +
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by=0.2))

figB = ggplot(data=longitudinal2_df,
              aes(x=phase,y=tir,group=record_id)) +
  geom_point() +
  geom_path() +
  xlab("") +
  ylab("Time in Range (%)") +
  theme_bw() +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20))

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
  ggsave(.,filename=paste0(path_sh_folder,"/Glucose and Insulin Data/figures/figure_phase specific cgm and insulin summary after imputation.png"),width=12,height=4)

# https://journals.sagepub.com/doi/pdf/10.1177/1932296814537039
