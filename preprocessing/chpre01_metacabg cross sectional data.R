rm(list=ls());gc();source(".Rprofile")

metacabg <- readRDS(paste0(path_metacabg_paper,"/working/raw/metacabg_20230706.RDS"))

screening <- metacabg %>% 
  dplyr::filter(event_name == "screening") %>% 
  dplyr::select(record_id,event_name,age_at_time_of_consent,sex,race,other_race,bmi_calculated,
                type_of_participation)


surgery_vars <- metacabg %>% 
  dplyr::filter(event_name == "surgery")  %>% 
  summarize_all(~sum(!is.na(.))) %>% 
  pivot_longer(everything(),names_to="var",values_to="n_nonna") %>% 
  dplyr::filter(n_nonna > 1) %>% 
  dplyr::select(var) %>% 
  pull()

post1_vars <- metacabg %>% 
  dplyr::filter(event_name == "post1")  %>% 
  summarize_all(~sum(!is.na(.))) %>% 
  pivot_longer(everything(),names_to="var",values_to="n_nonna") %>% 
  dplyr::filter(n_nonna > 1) %>% 
  dplyr::select(var) %>% 
  pull()

post2_vars <- metacabg %>% 
  dplyr::filter(event_name == "post2")  %>% 
  summarize_all(~sum(!is.na(.))) %>% 
  pivot_longer(everything(),names_to="var",values_to="n_nonna") %>% 
  dplyr::filter(n_nonna > 1) %>% 
  dplyr::select(var) %>% 
  pull()

surgery <- metacabg %>% 
  dplyr::filter(event_name == "surgery") %>% 
  dplyr::select(one_of(surgery_vars))

post1 <- metacabg %>% 
  dplyr::filter(event_name == "post1") %>% 
  dplyr::select(one_of(post1_vars))

post2 <- metacabg %>% 
  dplyr::filter(event_name == "post2") %>% 
  dplyr::select(one_of(post2_vars))

saveRDS(surgery,paste0(path_metacabg_paper,"/working/data/surgery_cs.RDS"))
write_csv(surgery,paste0(path_metacabg_paper,"/working/data/surgery_cs.csv"))


saveRDS(screening,paste0(path_metacabg_paper,"/working/data/screening.RDS"))
write_csv(screening,paste0(path_metacabg_paper,"/working/data/screening.csv"))
