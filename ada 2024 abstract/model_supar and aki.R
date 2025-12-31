aki <- readRDS(paste0(path_metacabg_paper,"/working/data/aki.RDS")) 
bg_post1 <- readRDS(paste0(path_metacabg_paper,"/working/data/bg_longitudinal.RDS")) %>% 
  dplyr::filter(event_name == "post1") %>% 
  group_by(record_id) %>% 
  summarize(mean_bg = mean(value),
            n_bg = n(),
            n_bg_ge140 = sum(value>=140),
            n_bg_ge180 = sum(value>=180)) %>% 
  mutate(stress_hyperglycemia = case_when(mean_bg >= 140 ~ 1,
                                          mean_bg < 140 ~ 0,
                                          TRUE ~ NA_real_),
         
         stress_hyperglycemia_def2 = case_when(n_bg_ge140 >= 2 ~ 1,
                                               n_bg_ge180 >= 1 ~ 1,
                                               TRUE ~ 0))

screening <- readRDS(paste0(path_metacabg_paper,"/working/data/screening_cs.RDS"))

advanced_biomarkers <- readRDS(paste0(path_metacabg_paper,"/working/data/advanced_biomarkers.RDS"))


aki_post1_records <- aki %>% 
  dplyr::filter(event_name == "post1", aki_status == 1) %>% 
  dplyr::select(record_id) %>% 
  pull()


excluded_records <- aki %>% 
  dplyr::filter(record_id %in% aki_post1_records) %>% 
  group_by(record_id) %>% 
  summarize(count_aki = sum(aki_status)) %>% 
  dplyr::filter(count_aki > 1)


analytic_dataset <- aki %>% 
  dplyr::filter(!record_id %in% excluded_records, !event_name %in% c("surgery","post1")) %>% 
  group_by(record_id) %>% 
  mutate(n = 1:n(),
         aki_count = cumsum(aki_status)) %>% 
  mutate(max_followup = case_when(aki_count == 1 & aki_status == 1 ~ 1,
                                  aki_count == 0 & n == max(n) ~ 1,
                                  TRUE ~ 0)) %>% 
  dplyr::filter(max_followup == 1) %>% 
  mutate(t = (str_replace(event_name,"post","") %>% as.numeric(.) - 1)) %>% 
  dplyr::select(record_id, t, aki_status) %>% 
  left_join(screening %>% 
              dplyr::select(record_id,age_at_time_of_consent,
                            sex, race,bmi_calculated),
            by = "record_id") %>% 
  left_join(bg_post1 %>% 
              dplyr::select(record_id, mean_bg, stress_hyperglycemia_def2),
            by = "record_id") %>% 
  dplyr::filter(!is.na(mean_bg)) %>% 
  left_join(advanced_biomarkers %>% 
              dplyr::filter(visit == 3),
            by = "record_id")

analytic_dataset %>% 
  group_by(stress_hyperglycemia_def2,aki_status ) %>% 
  tally()

final_analytic_dataset <- analytic_dataset %>% 
  dplyr::filter(!is.na(supar), race != "Other") %>% 
  mutate(high_supar = case_when(supar >= 2858 & sex == "Female" ~ 1,
                                supar >= 3908 & sex == "Male" ~ 1,
                                TRUE ~ 0))

library(survival)
m1 <-    coxph(as.formula(paste0("Surv(t, aki_status) ~ high_supar + stress_hyperglycemia_def2")), 
            data = final_analytic_dataset, method='efron',x=TRUE)


summary(m1)

broom::tidy(m1,exponentiate = TRUE) %>% 
  View()

adj_survival_fit = surv_direct(outcome_model = m1,data=analytic_dataset,
                               variable = "stress_hyperglycemia_def2",times = seq(0,7,by=1),
                               conf_int = TRUE)


l1 <- glm(aki_status ~ high_supar + stress_hyperglycemia_def2, data = final_analytic_dataset,family=binomial())
broom::tidy(l1,exponentiate = TRUE) %>% 
  View()
