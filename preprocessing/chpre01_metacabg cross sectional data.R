rm(list=ls());gc();source(".Rprofile")

metacabg <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS"))

screening <- metacabg %>% 
  dplyr::filter(event_name == "screening") %>% 
  dplyr::select(record_id,event_name,age_at_time_of_consent,sex,race,other_race,weight_kg, height_cm, bmi_calculated,
                type_of_participation, blinded_group, education, length_stay_prior_enrollment,total_length_stay_post_cabg,total_length_stay_post_admission,
                hypertension, hyperlipidemia, infectiousdisease, pulmonary, renaldisease,
                medication_acearb, medication_asaplavix, medication_betablocker,
                medication_statins, alcohol_any, alcohol_ndrinksperday,
                drugs_any, smoked_ever, hba1c, lipidpanel_last6mo,
                totalchol, tgl, hdl, ldl)
saveRDS(screening,paste0(path_metacabg_paper,"/working/data/screening_cs.RDS"))
write_csv(screening,paste0(path_metacabg_paper,"/working/data/screening_cs.csv"))


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
  dplyr::select(record_id,event_name,contains("surgery")) %>% 
  dplyr::select(-date_surgery_or_drips,-devices_changed_reason_surgery)

post1 <- metacabg %>% 
  dplyr::filter(event_name == "post1") %>% 
  dplyr::select(one_of(post1_vars))

post2 <- metacabg %>% 
  dplyr::filter(event_name == "post2") %>% 
  dplyr::select(one_of(post2_vars))

saveRDS(surgery,paste0(path_metacabg_paper,"/working/data/surgery_cs.RDS"))
write_csv(surgery,paste0(path_metacabg_paper,"/working/data/surgery_cs.csv"))


