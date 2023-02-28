source(".Rprofile")
dt_surgery <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dt_surgery.RDS"))

source("analysis/sha_figure df harmonized.R")
rm(icu48h_glucose,icu48h_ivinsulin,icu48h_poctmeals,icu48h_subqinsulin,
   or_to_icu_bolus,or_to_icu_glucose,or_to_icu_ivinsulin)

dataset_for_calibration <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dataset for calibration.RDS"))

unique_subject_ids = dataset_for_calibration[[1]] %>% 
  distinct(record_id,subject_id)

pooled_dataset_for_calibration <- imap_dfr(dataset_for_calibration,
                                          function(df,name){
                                            df %>% 
                                              mutate(m = name)
                                            
                                          })


for(i in 1:nrow(unique_subject_ids)){
  
  s_i = unique_subject_ids$subject_id[i]
  r_i = unique_subject_ids$record_id[i]
  
  surg_df = dt_surgery %>% 
    dplyr::filter(record_id == r_i)
  
  poct_df = fig_df %>% 
    dplyr::filter(record_id == r_i,variable == "Glucose")
  
  df = pooled_dataset_for_calibration %>% 
    dplyr::filter(subject_id == s_i) %>%
    dplyr::select(timestamp,m,sensorglucose,cgm_glucose,fit,upr,lwr) %>% 
    pivot_longer(cols=c("sensorglucose","cgm_glucose","fit","upr","lwr"),names_to="type",values_to="value") %>% 
    group_by(timestamp,type) %>% 
    summarize(med = median(value),
              min = min(value),
              max = max(value)) %>% 
    ungroup() %>% 
    mutate(type = factor(type,levels=c("sensorglucose","cgm_glucose","fit","upr","lwr"),
                         labels = c("Actual","Missing Data Corrected",
                                    "Median POCT Corrected",
                                    "95%UCI POCT Corrected",
                                    "95%LCI POCT Corrected")))
  
  fig = ggplot(data=df,aes(x = timestamp,group=type,col=type,y = med,ymin=min,ymax=max))  +
    geom_path() +
    geom_errorbar() +
    scale_color_manual(name = "Series",values= c("black","red","grey60","grey85","grey90")) +
    theme_bw() + 
    scale_y_continuous(limits=c(0,300),breaks=seq(0,300,by=50)) +
    geom_vline(xintercept = c(surg_df$surgery_start_time),col="black",linetype=2) +
    annotate("text",y=300,x=surg_df$surgery_start_time,hjust = 0,label = "Start of Surgery") +
    geom_vline(xintercept = c(surg_df$surgery_end_time),col="black",linetype=2) +
    annotate("text",y=280,x=surg_df$surgery_end_time,hjust = 0,label = "End of Surgery") +
    ggtitle(paste0(r_i," || Device: ",s_i))  +
    geom_point(data=poct_df,aes(x=timestamp,y=value,ymin=value,ymax=value),col="darkgreen",shape = 2) +
    theme(legend.position = "bottom")
  
  ggsave(fig,filename=paste0(path_sh_folder,"/Glucose and Insulin Data/working/calibrated/",s_i,".png"),width=10,height=6)
  
  
}
