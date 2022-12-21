# Plots ---------
aucs <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/aucs and mean sd.RDS"))


figA <- ggplot(data=aucs,aes(x=estimate,y=sd)) +
  geom_point() +
  xlab("Mean Glucose") +
  ylab("SD") +
  scale_x_continuous(limits=c(0,200),breaks=seq(0,200,by=50)) +
  theme_bw() +
  geom_vline(xintercept = 140,col="red",linetype=2)
figB <- ggplot(data=aucs,aes(x=estimate,y=cv)) +
  geom_point() +
  xlab("Mean Glucose") +
  ylab("CV (%)") +
  scale_x_continuous(limits=c(0,200),breaks=seq(0,200,by=50)) +
  scale_y_continuous(limits=c(0,31),breaks=seq(0,30,by=5)) +
  theme_bw()  +
  geom_vline(xintercept = 140,col="red",linetype=2)
figC <- ggplot(data=aucs,aes(x=estimate,y=auc)) +
  geom_point() +
  xlab("Mean Glucose") +
  ylab("AUC") +
  scale_x_continuous(limits=c(0,200),breaks=seq(0,200,by=50)) +
  scale_y_continuous(limits=c(0,750),breaks=seq(0,750,by=150)) +
  theme_bw()  +
  geom_vline(xintercept = 140,col="red",linetype=2)


library(ggpubr)
ggarrange(figA,figB,figC,
          nrow=1,ncol=3) %>% 
  ggsave(plot = .,filename = paste0(path_sh_folder,"/Glucose and Insulin Data/figures/sha01_mean glucose vs SD.png"),width=9,height=3)



figD <- ggplot(data=aucs,aes(x=estimate,y=max,label=n)) +
  geom_label() +
  xlab("Mean Glucose") +
  ylab("Max Glucose") +
  scale_x_continuous(limits=c(0,200),breaks=seq(0,200,by=50)) +
  scale_y_continuous(limits=c(0,300),breaks=seq(0,300,by=50)) +
  theme_bw()  +
  geom_hline(yintercept = 180,col="red",linetype=2) +

  geom_vline(xintercept = 140,col="darkblue",linetype=2)

figD


selected_patients <- aucs %>% 
  mutate(selection = case_when(max > 180 & estimate > 140 ~ "High",
                               max > 150 & estimate > 140 ~ "Possible High",
                               max <= 180 | estimate <= 140 ~ "Low",
                               TRUE ~ NA_character_)) %>% 
  rename(mean = estimate,
         npoints = n) %>% 
  dplyr::select(record_id,mean,max,npoints, selection)

write_csv(selected_patients,paste0(path_sh_folder,"/Glucose and Insulin Data/selected_patients.csv"))
