
source("analysis/sha_figure df harmonized.R")

insulin_counts <- fig_df %>% 
  dplyr::filter(location == "OR") %>% 
  dplyr::select(timestamp,record_id,variable,value) %>% 
  distinct(timestamp,record_id,variable,value,.keep_all=TRUE) %>%
  pivot_wider(names_from="variable",values_from = "value") %>% 
  group_by(record_id) %>% 
  mutate(next_insulin = case_when(!is.na(Insulin) ~ 1,
                                  TRUE ~ 0)) %>% 
  mutate(next_insulin = cumsum(next_insulin)) %>% 
  ungroup()


pre_insulin <- insulin_counts %>%
  dplyr::filter(next_insulin == 0) 


all_insulin <- insulin_counts %>% 
  dplyr::filter(!is.na(Insulin))

saveRDS(pre_insulin,paste0(path_sh_folder,"/Glucose and Insulin Data/working/pre_insulin.RDS"))
saveRDS(all_insulin,paste0(path_sh_folder,"/Glucose and Insulin Data/working/all_insulin.RDS"))



mean_glucose = pre_insulin %>% 
  group_by(record_id) %>% 
  summarize(estimate = mean(Glucose,na.rm=TRUE),
            sd = sd(Glucose,na.rm=TRUE),
            n = n(),
            max = max(Glucose,na.rm=TRUE)) %>% 
  mutate(cv = 100*sd/estimate)



require(pracma)
# AUC = trapz(strike,volatility)

unique_records <- pre_insulin %>% 
  group_by(record_id) %>% 
  dplyr::filter(n()>2) %>% 
  select(record_id) %>% 
  pull() %>% 
  unique(.)


aucs = map_dfr(unique_records,
          function(u_r){
            df = pre_insulin %>% 
              dplyr::filter(record_id == u_r);
            
            
            xaxis <- as.numeric(df$timestamp - min(df$timestamp))/3600
            
            auc <- pracma::trapz(xaxis, df$Glucose)
            
            data.frame(record_id = u_r,
                       auc = auc) %>%
              return(.)
            
          }) %>% 
  right_join(mean_glucose,
            by="record_id")

saveRDS(aucs,paste0(path_sh_folder,"/Glucose and Insulin Data/working/aucs and mean sd.RDS"))

