source(".Rprofile")
library(lubridate)
# library(fuzzyjoin)
# Based on call with FJP on 17 Jan 2023 to select closest CGM glucose value after POCT for alignment
lag_mins = 0
max_mins = 4
dt_surgery <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/dt_surgery.RDS"))

source("analysis/sha_figure df harmonized.R")
rm(icu48h_glucose,icu48h_ivinsulin,icu48h_poctmeals,icu48h_subqinsulin,
   or_to_icu_bolus,or_to_icu_glucose,or_to_icu_ivinsulin)

source("preprocessing/shpre_cgm for imputation.R")

# subject_id : record_id + device
# record_id: unique participant ID
imputed_subject_ids = list.files(paste0(path_sh_folder,"/Glucose and Insulin Data/working/amelia")) %>% 
  .[str_detect(.,".RDS")] %>% 
  str_replace(.,"\\.RDS","")
unimputed_subject_ids = unique_devices[!unique_devices %in% imputed_subject_ids]

excluded_subject_ids_from_calibration <- c(
                                           # Not a lot of variability and had to impute during surgery
                                           "MCM017","MCM027"
                                           
                                           )


m = 10
# https://cran.r-project.org/web/packages/Amelia/vignettes/using-amelia.html
imputed_dfs = map(1:m,
                  function(i){
                    
                    map_dfr(list.files(paste0(path_sh_folder,"/Glucose and Insulin Data/working/amelia"))%>% 
                              .[str_detect(.,".RDS")],
                            function(f){
                              
                              mi_df_i = readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/amelia/",f))$imputations[[i]]
                              return(mi_df_i)
                              
                            }) %>% 
                      bind_rows(cgm_for_imputation %>% 
                                  dplyr::filter(subject_id %in% unimputed_subject_ids)) %>%
                      dplyr::filter(!subject_id %in% excluded_subject_ids_from_calibration) %>% 
                      return(.)
                    
                  })

# fuzzyjoin
# https://community.rstudio.com/t/tidy-way-to-range-join-tables-on-an-interval-of-dates/7881/2
# https://stackoverflow.com/questions/61712185/r-fuzzy-left-join-with-time

# tidyverse
# https://stackoverflow.com/questions/62684774/matching-timestamps-in-two-dataframes-using-dplyr
library(lme4)
library(merTools)
library(splines)
dataset_for_calibration = map(1:m,
                              function(i){
                                
                                
                                merged_df = left_join(imputed_dfs[[i]], fig_df %>%
                                                        dplyr::filter(variable == "Glucose") %>% 
                                                        rename(poct_timestamp = timestamp),
                                                      by=c("record_id"="record_id")) %>% 
                                  dplyr::filter(timestamp >= poct_timestamp, 
                                                timestamp <= (poct_timestamp + minutes(max_mins))) %>% 
                                  rename(phase_imp = phase)
                                
                                pred_df <- imputed_dfs[[i]] %>% 
                                  dplyr::mutate(phase_imp = case_when(phase %in% merged_df$phase ~ phase,
                                                                   TRUE ~ "post_25to71hours"))
                                
                                lmm_fit = lmer(value ~ phase_imp*ns(cgm_glucose,4) + (cgm_glucose|subject_id),data = merged_df)
                                
                                PI <- predictInterval(merMod = lmm_fit, newdata = pred_df,
                                                      level = 0.95, n.sims = 1000,
                                                      stat = "median", type="linear.prediction",
                                                      include.resid.var = TRUE)
                                
                                bind_cols(pred_df,PI) %>% 
                                
                                return(.)
                                
                                
                              })
saveRDS(dataset_for_calibration,paste0(path_sh_folder,"/Glucose and Insulin Data/working/dataset for calibration.RDS"))





