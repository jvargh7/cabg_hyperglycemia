rm(list=ls());gc();source(".Rprofile")

screening <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS")) %>% 
  dplyr::filter(event_name == "screening", !record_id %in% excluded_metabocabg_record_ids) %>% 
  dplyr::select(record_id,event_name,age_at_time_of_consent,sex,race,other_race,weight_kg, height_cm, bmi_calculated,
                type_of_participation, blinded_group, education, length_stay_prior_enrollment,total_length_stay_post_cabg,total_length_stay_post_admission,
                contains("history"),
                hypertension, hyperlipidemia, infectiousdisease, pulmonary, renaldisease,
                medication_acearb, medication_asaplavix, medication_betablocker,
                medication_statins, alcohol_any, alcohol_ndrinksperday,
                drugs_any, smoked_ever, hba1c, lipidpanel_last6mo,
                totalchol, tgl, hdl, ldl) %>% 
  mutate(group = case_when(!is.na(blinded_group) ~ blinded_group,
                           type_of_participation == "Observation ONLY" ~ "Observation",
                           TRUE ~ "Missing"))
metacabg_cgm <- readxl::read_excel("data/CABG Hyperglycemia Variable List.xlsx",sheet="METACABG CGM") %>% 
  dplyr::filter(is.na(cgm1_status) | !cgm1_status %in% c("not usable","screen failure")) %>% 
  dplyr::filter(!is.na(cgm1_id) | !is.na(cgm2_id)) %>% 
  left_join(screening %>% 
              dplyr::select(record_id,blinded_group),
            by = "record_id") 

library(gtsummary)
# Unweighted -----------
(unweighted <- screening %>% 
   dplyr::filter(record_id %in% metacabg_cgm$record_id) %>% 
   mutate(bmi_category = case_when(bmi_calculated < 18.5 ~ "Underweight",
                                   bmi_calculated >= 18.5 & bmi_calculated < 25.0 ~ "Normal",
                                   bmi_calculated >= 25.0 & bmi_calculated < 30.0 ~ "Overweight",
                                   bmi_calculated >= 30.0 ~ "Obese",
                                   TRUE ~ NA_character_)) %>% 
   tbl_summary(by = group,
               include=c(age_at_time_of_consent,
                         sex,
                         race,
                         other_race,
                         weight_kg, 
                         height_cm, 
                         bmi_calculated,
                         bmi_category,
                         education, length_stay_prior_enrollment,total_length_stay_post_cabg,total_length_stay_post_admission,
                         history_cardiac_any,history_cardiac_afib,
                         hypertension, hyperlipidemia, infectiousdisease, pulmonary, renaldisease,
                         medication_acearb, medication_asaplavix, medication_betablocker,
                         medication_statins, alcohol_any, alcohol_ndrinksperday,
                         drugs_any, smoked_ever, hba1c, lipidpanel_last6mo,
                         totalchol, tgl, hdl, ldl
               ),
               missing = "ifany",
               missing_text = "Missing",
               type = list(
                 age_at_time_of_consent ~ "continuous",
                 sex ~ "categorical",
                 race ~ "categorical",
                 other_race ~ "categorical",
                 weight_kg ~ "continuous", 
                 height_cm ~ "continuous", 
                 bmi_calculated ~ "continuous",
                 bmi_category  ~ "categorical",
                 
                 education ~ "categorical", 
                 length_stay_prior_enrollment ~ "continuous2",
                 total_length_stay_post_cabg ~ "continuous2",
                 total_length_stay_post_admission ~ "continuous2",
                 history_cardiac_any ~ "categorical",
                 history_cardiac_afib ~ "categorical",
                 hypertension ~ "categorical",
                 hyperlipidemia ~ "categorical", infectiousdisease ~ "categorical", pulmonary ~ "categorical", renaldisease ~ "categorical",
                 medication_acearb ~ "categorical", medication_asaplavix ~ "categorical", medication_betablocker ~ "categorical",
                 medication_statins ~ "categorical", alcohol_any ~ "categorical", alcohol_ndrinksperday ~ "categorical",
                 drugs_any ~ "categorical", smoked_ever ~ "categorical", hba1c ~ "continuous", lipidpanel_last6mo ~ "categorical",
                 totalchol ~ "continuous", tgl ~ "continuous", hdl ~ "continuous", ldl ~ "continuous"
                 
                 
               ),
               digits = list(age_at_time_of_consent ~ c(1,1),
                             sex ~ c(1,1),
                             race ~ c(1,1),
                             other_race ~ c(1,1),
                             weight_kg ~ c(1,1), 
                             height_cm ~ c(1,1), 
                             bmi_calculated ~ c(1,1),
                             bmi_category  ~ c(1,1),
                             
                             education ~ c(1,1), 
                             length_stay_prior_enrollment ~ c(1,1,1,1,1),
                             total_length_stay_post_cabg ~ c(1,1,1,1,1),
                             total_length_stay_post_admission ~ c(1,1,1,1,1),
                             history_cardiac_any ~ c(1,1),
                             history_cardiac_afib ~ c(1,1),
                             hypertension ~ c(1,1),
                             hyperlipidemia ~ c(1,1), infectiousdisease ~ c(1,1), pulmonary ~ c(1,1), renaldisease ~ c(1,1),
                             medication_acearb ~ c(1,1), medication_asaplavix ~ c(1,1), medication_betablocker ~ c(1,1),
                             medication_statins ~ c(1,1), alcohol_any ~ c(1,1), alcohol_ndrinksperday ~ c(1,1,1,1,1),
                             drugs_any ~ c(1,1), smoked_ever ~ c(1,1), hba1c ~ c(1,1), lipidpanel_last6mo ~ c(1,1),
                             totalchol ~ c(1,1), tgl ~ c(1,1), hdl ~ c(1,1), ldl ~ c(1,1)
               ),
               statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))) %>% 
   add_n() %>% 
   add_overall()) %>%
  as_gt() %>%
  gt::gtsave(filename = "attd abstract/descriptive characteristics.docx")

