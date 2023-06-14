sh_variables <- readxl::read_excel("data/Stress Hyperglycemia Variable List.xlsx",sheet="aha glucose") %>% 
  dplyr::filter(!is.na(new_var))

patients_with_descriptives <- read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/raw/METABOCABG-AHAAbstractPtCategor_DATA_LABELS_2023-06-09_1404.csv")) %>% 
  dplyr::select(one_of("Record ID   Group: [screeningrandomiza_arm_1][bllinded_group]")) %>% 
  pull()

# read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/raw/METABOCABG-AHAAbstractPtCategor_DATA_LABELS_2023-06-09_1404.csv")) %>% 
#   head(.,n=10) %>% 
#   write_csv(.,file="aha abstract/head_METABOCABG insulin req.csv")


# read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/raw/METABOCABG-AHAAbstractPtCategor_DATA_LABELS_2023-06-12_1127.csv")) %>%
#   head(.,n=10) %>%
#   write_csv(.,file="aha abstract/head_METABOCABG insulin req.csv")

glucose_df <- read_csv(paste0(path_sh_folder,"/Glucose and Insulin Data/raw/METABOCABG-AHAAbstractPtCategor_DATA_LABELS_2023-06-12_1127.csv"),
                       col_select = sh_variables$variable) %>% 
  rename_with(~ sh_variables$new_var[which(sh_variables$variable == .x)], 
              .cols = sh_variables$variable)  %>% 
  dplyr::filter(record_id %in% patients_with_descriptives) %>% 
  # https://stackoverflow.com/questions/61940984/using-pivot-longer-with-multiple-paired-columns-in-the-wide-dataset
  pivot_longer(cols=-one_of("record_id","event_name",
                            "duration_surgery","surgery_start_time",
                            "surgery_end_time","date_cabg"),
               names_to=c(".value","var"),
               names_sep="_") %>% 
  # Error with MCM045
  mutate(date_cabg = case_when(record_id == "MCM045" ~ ymd("2020-11-27"),
                               TRUE ~ date_cabg)) %>% 
  mutate(time = case_when(var == "postop" ~ hms::as_hms(surgery_end_time),
                          TRUE ~ time)) %>% 
  dplyr::filter(!is.na(time) | !is.na(glucose)) %>% 
  # Grouping to identify next day
  group_by(record_id) %>% 
  # Checking if surgery was overnight
  mutate(is_overnight = case_when(format(surgery_end_time, "%m/%d/%Y") > format(surgery_start_time, "%m/%d/%Y") ~ 1,
                                  TRUE ~ 0),
         # Checking if serial observations are of lower time --> indicates next day starts
         is_lower_time = case_when(time < dplyr::lag(time,1) ~ 1,
                              TRUE ~ 0)) %>% 
  mutate_at(vars(duration_surgery,surgery_start_time,surgery_end_time,date_cabg),~zoo::na.locf(.)) %>% 
  mutate(added_days = cumsum(is_lower_time)) %>% 
  mutate(date_event =  date_cabg + days(added_days)) %>% 
  ungroup() %>% 
  mutate(timestamp = lubridate::as_datetime(paste0(date_event," ",time)))  %>% 
  mutate(surgery_end_time_plus24 = surgery_end_time + lubridate::days(1)) %>% 
  dplyr::filter(timestamp >= surgery_start_time,timestamp <= surgery_end_time_plus24) %>% 
  group_by(record_id,date_cabg) %>% 
  mutate(timepoint = 1:n()) 


# From sha01_glucose before stress hyperglycemia -----

mean_glucose = glucose_df %>% 
  group_by(record_id) %>% 
  summarize(estimate = mean(glucose,na.rm=TRUE),
            sd = sd(glucose,na.rm=TRUE),
            n = n(),
            max = max(glucose,na.rm=TRUE)) %>% 
  mutate(cv = 100*sd/estimate) %>% 
  mutate(selection = case_when(max > 180 & estimate > 140 ~ "High",
                               max > 150 & estimate > 140 ~ "Possible High",
                               max <= 180 | estimate <= 140 ~ "Low",
                               TRUE ~ NA_character_)) %>% 
  rename(mean = estimate,
         npoints = n)

write_csv(mean_glucose, paste0(path_sh_folder,"/Glucose and Insulin Data/selected_patients for aha abstract.csv"))
