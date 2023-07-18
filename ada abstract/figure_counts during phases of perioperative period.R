fig_df <- read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/working/longitudinal dataset for metabolomics after imputation.csv")) %>% 
  dplyr::filter(record_id %in% c("MCE008","MCE009","MCM015",
                                 "MCM018","MCM021","MCM024",
                                 "MCM027","MCM030","MCM034",
                                 "MCM036","MCM037","MCM044",
                                 "MCM049","MCM050","MCM051",
                                 "MCM055","MCM056")) %>% 
  mutate(TIR_Class = case_when(tir >= 70 ~ "Normal",
                               TRUE ~ "Out of Range"),
         phase = fct_relevel(phase,c("Pre Surgery","Surgery","Post 24 hours","Post 72-96 hours"))) %>% 
  group_by(TIR_Class,phase) %>% 
  tally()


(fig_counts <- fig_df %>% 
  ggplot(data=.,aes(x=phase,fill=TIR_Class,y =n ,label=n)) +
  geom_col(position = position_dodge(width=0.9),width=0.8) +
  geom_text(aes(y=n+1),position = position_dodge(width=0.9)) +
  theme_bw() +
  xlab("") +
  ylab("Count") +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="",values=c("darkgreen","red"))) %>% 
  ggsave(.,filename= paste0(path_sh_paper,"/ada abstract/counts during phases of perioperative period by TIR.png"),width=8,height=6)
# fig_df <- read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/working/longitudinal dataset for metabolomics.csv"))
