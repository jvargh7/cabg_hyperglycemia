# From figure_mean glucose vs SD.R -------

selected_patients <- read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/selected_patients for aha abstract.csv")) %>% 
  mutate(group_sh = case_when(selection == "Low" ~ "Low",
                              TRUE ~ "High"))

aha_variables <- readxl::read_excel("data/Stress Hyperglycemia Variable List.xlsx",sheet="aha patient") %>% 
  dplyr::filter(!is.na(new_var))

patient_data <- readxl::read_excel(paste0(path_sh_folder,"/Glucose and Insulin Data/raw/AHA final clean Excel.xlsx")) %>% 
  dplyr::select(-one_of(c("...2","Specify (1):"))) %>% 
  rename_with(~ aha_variables$new_var[which(aha_variables$variable == .x)], 
              .cols = aha_variables$variable) %>% 
  dplyr::select(-contains("status")) %>% 
  
  mutate(d_white = case_when(race == "black" ~ "no",
                          race == "white" ~ "yes",
                          race == "Asian" ~ "no",
                          TRUE ~ NA_character_),
         d_n_vessels_gt2 = case_when(n_vessels %in% c(1,2) ~ "no",
                                     TRUE ~ "yes"),
         d_male = case_when(sex == "m" ~ "yes",
                            TRUE ~ "no"),
         
         d_ef = case_when(ef == "<25" ~ 25,
                          ef == ">55" ~ 55,
                          TRUE ~ as.numeric(ef)),
         
         d_pressor = case_when(pressor == "Yes" ~ "yes",
                               TRUE ~ "no"),
         
         d_ge2_pressor = case_when(ge2_pressor == "Use of 2 or more pressors/inotropes" ~ "yes",
                                   TRUE ~ "no"),
         
         d_curr_smoker = case_when(smoking == "Yes" ~ "yes",
                                   TRUE ~ "no"),
         
         d_cardiacarrest = case_when(is.na(cardiacarrest) ~ "No",
                                     TRUE ~ cardiacarrest),
         
         d_ef_lt50 = case_when(d_ef < 50 ~ "yes",
                               TRUE ~ "no")
         
         ) %>% 
  mutate_at(vars(one_of(c("afib","arr_pvc","arr_vtach","arr_arrest_vfib",
                          "arr_arrest_asystolepea","arr_other"))),function(x) case_when(x %in% c("checked","Checked") ~ "yes",
                                                                                        x %in% c("unchecked","Unchecked") ~ "no",
                                                                                        TRUE ~ NA_character_)) %>% 
  
  dplyr::select(record_id, hba1c,duration_surgery,
                apache_ii,bmi, d_ef, age,
                d_white, d_n_vessels_gt2, d_male, d_pressor, d_ge2_pressor, d_ef_lt50, d_curr_smoker,
                cardiac_arrhythmia, d_cardiacarrest, pulmonaryedema, heartfailure,
                hypertension, hyperlipidemia, alcohol, neversmoked,
                
                afib, arr_pvc,arr_vtach,arr_arrest_vfib,
                arr_arrest_asystolepea, arr_other)

table_df <- selected_patients %>% 
  left_join(patient_data,
            by = "record_id")


patient_data$record_id[!patient_data$record_id %in% selected_patients$record_id]


c_vars = c("mean","max","age","bmi","hba1c","duration_surgery","apache_ii","d_ef")
p_vars = c("d_white","d_n_vessels_gt2","d_male","d_pressor","d_ge2_pressor","d_ef_lt50",
           "d_curr_smoker","neversmoked","alcohol",
           "cardiac_arrhythmia","d_cardiacarrest",
           "pulmonaryedema","heartfailure","hypertension",
           "hyperlipidemia",
           
           "afib","arr_pvc","arr_vtach","arr_arrest_vfib",
           "arr_arrest_asystolepea","arr_other")

library(compareGroups)

cg_tab <- compareGroups(group_sh ~ mean + max + age + bmi +
                hba1c + apache_ii + d_male + 
                d_white + d_curr_smoker + neversmoked +
                alcohol + d_ef + d_ef_lt50 +
                # Surgical characteristics
                duration_surgery + d_n_vessels_gt2 +
                d_pressor + d_ge2_pressor + 
                cardiac_arrhythmia + d_cardiacarrest +
                pulmonaryedema + heartfailure + hypertension +
                hyperlipidemia +
                afib + arr_pvc + arr_vtach + arr_arrest_vfib + 
                arr_arrest_asystolepea + arr_other,data = table_df,
              method = c(1,1,2,1,
                         2,1,3,
                         3,3,3,
                         3,2,3,
                         2,3,
                         3,3,
                         3,3,
                         3,3,3,
                         3,
                         
                         3,3,3,3,
                         3,3),include.miss = TRUE,p.corrected = FALSE) 
cg_tab %>% 
  createTable(.,digits=1,show.all=TRUE,q.type = c(2,2),sd.type = 2,show.n = TRUE) %>% 
  export2xls(.,file="paper/table_aha abstract stress hyperglycemia without adjustment.xlsx")

p_unadj = map(cg_tab,
            function(c_t){
              c_t$p.overall
            }) %>% unlist() 
  
  
  p_adj = p.adjust(p_unadj,method="BH") %>% 
  as.data.frame() 

names(p_adj) = "p_adj"


p_adj$var_name = rownames(p_adj)
p_adj <- p_adj %>% 
  mutate(p_adj_round = round(p_adj,digits=3),
         p_unadj = p_unadj)

rownames(p_adj) = NULL


p_adj %>% 
  write.csv(.,file="paper/adjusted p values.csv")

# cg_tab %>% 
#   createTable(.,digits=1,show.all=TRUE,show.p.overall = TRUE,q.type = c(2,2),sd.type = 2,show.n = TRUE) %>% 
#   export2xls(.,file="paper/table_aha abstract stress hyperglycemia with adjustment.xlsx")

