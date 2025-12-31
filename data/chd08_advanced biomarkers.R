# Hyperglycemia -- dehydration -- AKI



# CRP -- C-reactive protein produced in liver -- IL-6 increases CRP (could have increased endothelial dysfunction) 
# <2.5-3: 
# 2.5-10: Chronic subclinical inflammation

# SuPAR -- AKI
# Age related -- no clear cutoff yet
# Independent of glucose
# <2475 - >=4189

# BNP -- Stretching of atrium of heart -- marker of heart failure
# - >100: Heart failure predictor
# BNP could be associated with obesity (lower in people with obesity)

# HSTN -- high sensitive troponin - marker of destruction of muscle of heart
# - High values worse outcomes
# 2.7 is a very low value



visit1 <- readxl::read_excel(paste0(path_metacabg_paper,"/working/raw/FINAL Endo Alinity Results .xlsx"),sheet="Visit 1") %>% 
  rename(record_id = "SAMPLE ID",
         crp = "CRP (mg/L)",
         supar = "SuPAR (pg/mL)",
         bnp = "BNP (pg/mL)",
         hstn = "HsTn (pg/mL)") %>% 
  mutate(across(crp:hstn,.fns=~as.character(.)))
visit2 <- readxl::read_excel(paste0(path_metacabg_paper,"/working/raw/FINAL Endo Alinity Results .xlsx"),sheet="Visit 2") %>% 
  rename(record_id = "SAMPLE ID",
         crp = "CRP (mg/L)",
         supar = "SuPAR (pg/mL)",
         bnp = "BNP (pg/mL)",
         hstn = "HsTn (pg/mL)") %>% 
  mutate(across(crp:hstn,.fns=~as.character(.)))
visit3 <- readxl::read_excel(paste0(path_metacabg_paper,"/working/raw/FINAL Endo Alinity Results .xlsx"),sheet="Visit 3") %>% 
  rename(record_id = "SAMPLE ID",
         crp = "CRP (mg/L)",
         supar = "SuPAR (pg/mL)",
         bnp = "BNP (pg/mL)",
         hstn = "HsTn (pg/mL)") %>% 
  mutate(across(crp:hstn,.fns=~as.character(.)))
visit4 <- readxl::read_excel(paste0(path_metacabg_paper,"/working/raw/FINAL Endo Alinity Results .xlsx"),sheet="Visit 4") %>% 
  rename(record_id = "SAMPLE ID",
         crp = "CRP (mg/L)",
         supar = "SuPAR (pg/mL)",
         bnp = "BNP (pg/mL)",
         hstn = "HsTn (pg/mL)") %>% 
  mutate(across(crp:hstn,.fns=~as.character(.))) 
visit5 <- readxl::read_excel(paste0(path_metacabg_paper,"/working/raw/FINAL Endo Alinity Results .xlsx"),sheet="Visit 5") %>% 
  rename(record_id = "SAMPLE ID",
         crp = "CRP (mg/L)",
         supar = "SuPAR (pg/mL)",
         bnp = "BNP (pg/mL)",
         hstn = "HsTn (pg/mL)") %>% 
  mutate(across(crp:hstn,.fns=~as.character(.)))

advanced_biomarkers <- bind_rows(visit1 %>% mutate(visit = 1),
          visit2 %>% mutate(visit = 2),
          visit3 %>% mutate(visit = 3),
          visit4 %>% mutate(visit = 4),
          visit5 %>% mutate(visit = 5)) %>% 
  mutate(crp = case_when(crp == "< 0.10" ~ 0.10,
                         crp %in% c("> 10.0","> 10.00") ~ 10.0,
                         TRUE ~ as.numeric(crp)),
         supar = as.numeric(supar)*1000,
         bnp = case_when(bnp == "< 10.0" ~ 10.0,
                         TRUE ~ as.numeric(bnp)),
         hstn = case_when(hstn == "< 2.7" ~ 2.7,
                          hstn == "> 3600" ~ 3600,
                          TRUE ~ as.numeric(hstn)))

saveRDS(advanced_biomarkers,paste0(path_metacabg_paper,"/working/data/advanced_biomarkers.RDS"))
write_csv(advanced_biomarkers,paste0(path_metacabg_paper,"/working/data/advanced_biomarkers.csv"))

fig_crp = advanced_biomarkers %>% 
  ggplot(data = ., aes(x= visit,
                    y = crp)) +
  geom_path(aes(group = record_id),col = "grey20",alpha = 0.2) +
  geom_smooth(method = "loess",col="black") +
  xlab("Visit") +
  ylab("CRP (mg/L)")
fig_crp

fig_supar = advanced_biomarkers %>% 
  ggplot(data = ., aes(x= visit,
                       y = supar)) +
  geom_path(aes(group = record_id),col = "grey20",alpha = 0.2) +
  geom_smooth(method = "loess",col="black") +
  xlab("Visit") +
  ylab("SuPAR (pg/mL)") +

fig_supar


fig_bnp = advanced_biomarkers %>% 
  ggplot(data = ., aes(x= visit,
                       y = bnp)) +
  geom_path(aes(group = record_id),col = "grey20",alpha = 0.2) +
  geom_smooth(method = "loess",col="black") +
  xlab("Visit") +
  ylab("BNP (pg/mL)")

fig_bnp


fig_hstn = advanced_biomarkers %>% 
  ggplot(data = ., aes(x= visit,
                       y = hstn)) +
  geom_path(aes(group = record_id),col = "grey20",alpha = 0.2) +
  geom_smooth(method = "loess",col="black") +
  xlab("Visit") +
  ylab("HsTN (pg/mL)")

fig_hstn
