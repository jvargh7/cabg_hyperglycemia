rm(list=ls());gc();source(".Rprofile")

screening <- readRDS(paste0(path_metacabg_paper,"/working/data/screening_cs.RDS"))
surgery_cs <- readRDS(paste0(path_metacabg_paper,"/working/data/surgery_cs.RDS")) %>% 
  dplyr::select(record_id,surgery_date,surgery_start_time,surgery_end_time)
# Blood glucose -----------

bg_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/data/bg_longitudinal.RDS")) 

bg_events <- bg_longitudinal %>% 
  left_join(surgery_cs,
            by = "record_id") %>% 
  mutate(surgery_period = case_when(timestamp < surgery_start_time ~ 1,
                                    timestamp >= surgery_start_time & timestamp <= surgery_end_time ~ 2,
                                    timestamp > surgery_end_time & timestamp < (surgery_end_time +hours(4)) ~ 3,
                                    timestamp >= (surgery_end_time +hours(4)) & timestamp <= (surgery_end_time +hours(24)) ~ 4,
                                    timestamp > (surgery_end_time +hours(24)) & timestamp <= (surgery_end_time +hours(48)) ~ 5,
                                    timestamp > (surgery_end_time +hours(48)) ~ 6)) %>%
  mutate(surgery_period = factor(surgery_period,levels=c(1:6),labels=c("Pre-surgery","Surgery","0 to <4h after surgery","4 to 24h after surgery",
                                                                       ">24 to 48h after surgery",">48h after surgery"))) %>% 
  group_by(record_id,surgery_period) %>% 
  tally() %>% 
  left_join(screening %>% 
              dplyr::select(record_id, type_of_participation, blinded_group),
            by = "record_id") %>% 
  mutate(group = case_when(!is.na(blinded_group) ~ blinded_group,
                           type_of_participation == "Observation ONLY" ~ "Observation",
                           TRUE ~ "Missing"))

(fig_bg <- bg_events %>% 
  ggplot(data=.,aes(y=surgery_period,x = record_id,fill = n)) +
  geom_tile() +
  scale_fill_gradient2("Number",low="white",mid="lightblue",high="darkblue",midpoint = 5,limits=c(0,40),na.value = "white") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(~group,scales="free_x") +
  xlab("") +
  ylab("")) %>% 
  ggsave(.,file = paste0(path_metacabg_paper,"/figures/events and data availability for glucose.png"),width = 14,height=8)

# Metabolomics --------
c18_class_2 <- read_table(paste0(path_metabolomics_folder,"/c18/class_2.txt")) %>% 
  mutate(Sample = str_replace(Sample,"MCE011","MCG001")) %>% 
  mutate(record_id = str_replace(Sample,"_V[1-5]",""),
         visit_id = str_replace(Sample,"[A-Z0-9]+_",""))

metabolomics_events = c18_class_2 %>% 
  dplyr::select(record_id,visit_id) %>% 
  mutate(n = 1) %>% 
  left_join(screening %>% 
              dplyr::select(record_id, type_of_participation, blinded_group),
            by = "record_id") %>% 
  mutate(group = case_when(!is.na(blinded_group) ~ blinded_group,
                           type_of_participation == "Observation ONLY" ~ "Observation",
                           TRUE ~ "Missing"))

(fig_metabolomics <- metabolomics_events %>% 
    ggplot(data=.,aes(y=visit_id,x = record_id,fill = n)) +
    geom_tile() +
    scale_fill_gradient("Number",low="white",high="darkblue",na.value = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none") +
    facet_grid(~group,scales="free_x") +
    xlab("") +
    ylab("")) %>% 
  ggsave(.,file = paste0(path_metacabg_paper,"/figures/events and data availability for metabolomics.png"),width = 14,height=8)


# Creatinine ----------
creatinine_events <- readRDS(paste0(path_metacabg_paper,"/working/data/labtests_longitudinal.RDS")) %>% 
  dplyr::select(record_id, event_name, date_event_name, creatinine) %>% 
  dplyr::filter(!is.na(creatinine)) %>% 
  group_by(record_id,event_name) %>% 
  tally() %>% 
  left_join(screening %>% 
              dplyr::select(record_id, type_of_participation, blinded_group),
            by = "record_id") %>% 
  mutate(event_name = factor(event_name,levels=c("screening","surgery",paste0("post",1:19)))) %>% 
  mutate(group = case_when(!is.na(blinded_group) ~ blinded_group,
                           type_of_participation == "Observation ONLY" ~ "Observation",
                           TRUE ~ "Missing"))

(fig_creatinine <- creatinine_events %>% 
    ggplot(data=.,aes(y=event_name,x = record_id,fill = n)) +
    geom_tile() +
    scale_fill_gradient("Number",low="white",high="darkblue",na.value = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none") +
    facet_grid(~group,scales="free_x") +
    xlab("") +
    ylab("")) %>% 
  ggsave(.,file = paste0(path_metacabg_paper,"/figures/events and data availability for creatinine.png"),width = 14,height=8)


# CGM1 ------------


cgm1_long_glucose <- readRDS(paste0(path_metacabg_paper,"/working/data/cgm harmonization.RDS")) %>% 
  dplyr::filter(cgm_type == "CGM 1",!record_id %in% excluded_cgm_record_ids) %>% 
  mutate(date = date(timestamp)) %>% 
  left_join(surgery_cs,
            by="record_id") %>% 
  group_by(record_id) %>% 
  mutate(min_date = min(date)) %>% 
  mutate(days_since_surgery_date = difftime(date,surgery_date,units="days")) %>% 
  ungroup() %>% 
  group_by(record_id,days_since_surgery_date) %>% 
  summarize(available_glucose = sum(!is.na(sensorglucose)),
            missing_glucose = sum(is.na(sensorglucose))) %>% 
  left_join(screening %>% 
              dplyr::select(record_id, type_of_participation, blinded_group),
            by = "record_id") %>% 
  mutate(group = case_when(!is.na(blinded_group) ~ blinded_group,
                           type_of_participation == "Observation ONLY" ~ "Observation",
                           TRUE ~ "Missing"))

(fig_cgm1_available_glucose <- cgm1_long_glucose %>% 
    ggplot(data=.,aes(y=days_since_surgery_date,x = record_id,fill = available_glucose)) +
    geom_tile() +
    scale_fill_gradient("Number",low="white",high="darkblue",na.value = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_grid(~group,scales="free_x") +
    xlab("") +
    ylab("Days since surgery")) %>% 
  ggsave(.,file = paste0(path_metacabg_paper,"/figures/CGM1 available glucose values.png"),width = 14,height=8)

(fig_cgm1_missing_glucose <- cgm1_long_glucose %>% 
    ggplot(data=.,aes(y=days_since_surgery_date,x = record_id,fill = missing_glucose)) +
    geom_tile() +
    scale_fill_gradient("Number",low="white",high="darkred",na.value = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_grid(~group,scales="free_x") +
    xlab("") +
    ylab("Days since surgery")) %>% 
  ggsave(.,file = paste0(path_metacabg_paper,"/figures/CGM1 missing glucose values.png"),width = 14,height=8)

# CGM2 ------------
cgm2_long_glucose <- readRDS(paste0(path_metacabg_paper,"/working/data/cgm harmonization.RDS")) %>% 
  dplyr::filter(cgm_type == "CGM 2",!record_id %in% excluded_cgm_record_ids) %>% 
  mutate(date = date(timestamp)) %>% 
  left_join(surgery_cs,
            by="record_id") %>% 
  group_by(record_id) %>% 
  mutate(min_date = min(date)) %>% 
  mutate(days_since_surgery_date = difftime(date,surgery_date,units="days")) %>% 
  ungroup() %>% 
  group_by(record_id,days_since_surgery_date) %>% 
  summarize(available_glucose = sum(!is.na(sensorglucose)),
            missing_glucose = sum(is.na(sensorglucose))) %>% 
  left_join(screening %>% 
              dplyr::select(record_id, type_of_participation, blinded_group),
            by = "record_id") %>% 
  mutate(group = case_when(!is.na(blinded_group) ~ blinded_group,
                           type_of_participation == "Observation ONLY" ~ "Observation",
                           TRUE ~ "Missing"))

(fig_cgm2_available_glucose <- cgm2_long_glucose %>% 
    ggplot(data=.,aes(y=days_since_surgery_date,x = record_id,fill = available_glucose)) +
    geom_tile() +
    scale_fill_gradient("Number",low="white",high="darkblue",na.value = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_grid(~group,scales="free_x") +
    xlab("") +
    ylab("Days since surgery")) %>% 
  ggsave(.,file = paste0(path_metacabg_paper,"/figures/CGM2 available glucose values.png"),width = 14,height=8)

(fig_cgm2_missing_glucose <- cgm2_long_glucose %>% 
    ggplot(data=.,aes(y=days_since_surgery_date,x = record_id,fill = missing_glucose)) +
    geom_tile() +
    scale_fill_gradient("Number",low="white",high="darkred",na.value = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_grid(~group,scales="free_x") +
    xlab("") +
    ylab("Days since surgery")) %>% 
  ggsave(.,file = paste0(path_metacabg_paper,"/figures/CGM2 missing glucose values.png"),width = 14,height=8)
