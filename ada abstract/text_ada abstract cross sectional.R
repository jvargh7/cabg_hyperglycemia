descriptives <- read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/raw/METABOCABG-Table1ForADAAbstract_DATA_LABELS_2023-03-08_0906.csv")) %>% 
  rename(record_id = "Record ID   Group: [screeningrandomiza_arm_1][bllinded_group]",
         age = "Age at time of consent",
         sex = "Sex",
         race = "Race",
         other_race = "Other Race",
         bmi = "BMI (calculated)",
         length_prior = "Length of hospital stay prior to enrollment",
         length_icu = "Total ICU stay post CABG",
         length_postcabg = "Total LOS after CABG",
         length_postadmission = "Total Hospital Length of Stay (Los) since hospital admission",
         never_smoked = "Never smoked",
         smoking = "Smoking?",
         alcohol = "Alcohol",
         hba1c = "HbA1c result",
         length_surgery = "Length of surgery")
cs_df = read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/working/crosssectional dataset for metabolomics after imputation.csv")) %>% 
  dplyr::filter(record_id %in% c("MCE008",
                                 'MCE009',
                                 "MCM015",
                                 "MCM018",
                                 "MCM021",
                                 "MCM024",
                                 "MCM027",
                                 "MCM030",
                                 "MCM034",
                                 "MCM036",
                                 "MCM037",
                                 "MCM044",
                                 "MCM049",
                                 "MCM050",
                                 "MCM051",
                                 "MCM055",
                                 "MCM056"))


text_df = cs_df %>% 
  left_join(descriptives,
            by = "record_id")


text_df %>% 
  dplyr::filter(!is.na(cv_group)) %>% 
  group_by(cv_group) %>%
  summarize(N = n(),
            prop_male = mean(sex == "Male",na.rm=TRUE),
            mean_age = mean(age,na.rm=TRUE),
            sd_age = sd(age,na.rm=TRUE),
            mean_bmi = mean(bmi,na.rm=TRUE),
            sd_bmi = sd(bmi,na.rm=TRUE),
            mean_hba1c = mean(hba1c,na.rm=TRUE),
            sd_hba1c = sd(hba1c,na.rm=TRUE))

text_df %>% 
  dplyr::filter(!is.na(tir_group)) %>% 
  group_by(tir_group) %>%
  summarize(N = n(),
            prop_male = mean(sex == "Male",na.rm=TRUE),
            mean_age = mean(age,na.rm=TRUE),
            sd_age = sd(age,na.rm=TRUE),
            mean_bmi = mean(bmi,na.rm=TRUE),
            sd_bmi = sd(bmi,na.rm=TRUE),
            mean_hba1c = mean(hba1c,na.rm=TRUE),
            sd_hba1c = sd(hba1c,na.rm=TRUE))
