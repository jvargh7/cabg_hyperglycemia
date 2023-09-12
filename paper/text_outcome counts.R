rm(list=ls());gc();source(".Rprofile")
screening <- readRDS(paste0(path_metacabg_paper,"/working/data/screening_cs.RDS"))

sh <- readRDS(paste0(path_metacabg_paper,"/working/data/stress hyperglycemia 4 to 24h.RDS")) %>% 
  left_join(screening %>% 
              dplyr::select(record_id, type_of_participation, blinded_group),
            by = "record_id") %>% 
  mutate(group = case_when(!is.na(blinded_group) ~ blinded_group,
                           type_of_participation == "Observation ONLY" ~ "Observation",
                           TRUE ~ "Missing"))
afib <- readRDS(paste0(path_metacabg_paper,"/working/data/afib.RDS")) %>% 
  left_join(screening %>% 
              dplyr::select(record_id, type_of_participation, blinded_group),
            by = "record_id") %>% 
  mutate(group = case_when(!is.na(blinded_group) ~ blinded_group,
                           type_of_participation == "Observation ONLY" ~ "Observation",
                           TRUE ~ "Missing"))
aki <- readRDS(paste0(path_metacabg_paper,"/working/data/aki.RDS")) %>% 
  group_by(record_id) %>% 
  summarize(aki_status = max(aki_status)) %>% 
  ungroup() %>% 
  left_join(screening %>% 
              dplyr::select(record_id, type_of_participation, blinded_group),
            by = "record_id") %>% 
  mutate(group = case_when(!is.na(blinded_group) ~ blinded_group,
                           type_of_participation == "Observation ONLY" ~ "Observation",
                           TRUE ~ "Missing"))


sh %>% 
  group_by(group) %>%
  summarize(n_pct = paste0(sum(stress_hyperglycemia)," (",
                        round(mean(stress_hyperglycemia)*100,1),"%)"))
  

afib %>% 
  group_by(group) %>%
  summarize(n_pct = paste0(sum(cardiac_arrhythmia_afib)," (",
                           round(mean(cardiac_arrhythmia_afib)*100,1),"%)"))

aki%>% 
  group_by(group) %>%
  summarize(n_pct = paste0(sum(aki_status)," (",
                           round(mean(aki_status)*100,1),"%)"))
