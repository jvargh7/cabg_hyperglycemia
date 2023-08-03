rm(list=ls());gc();source(".Rprofile")

# From chpre03_cgm harmonization.R -
cgm_long <- readRDS(paste0(path_metacabg_paper,"/working/cgm harmonization.RDS"))

source("C:/code/external/functions/cgm/plot_agp.R")

unique_traces <- unique(cgm_long$cgm_id)
timestamp_ranges = data.frame()

pdf(paste0(path_metacabg_paper,"/figures/ambulatory glucose profile.pdf"),width=12,height=8)

for(u_t in unique_traces){
  
  df = cgm_long %>% 
    dplyr::filter(cgm_id == u_t)
  
  timestamp_ranges = bind_rows(timestamp_ranges,
                               df %>% 
                                 summarize(min = min(timestamp,na.rm=TRUE),
                                           max = max(timestamp,na.rm=TRUE),
                                           n = sum(!is.na(sensorglucose))
                                 ) %>% 
                                 mutate(cgm_id = u_t))
  
  df  %>% 
    plot_agp(.,title=u_t,y_label = "Sensor Glucose (mg/dL)") %>% 
    print()
}

dev.off()

write_csv(timestamp_ranges,"paper/timestamp ranges of ambulatory glucose profile.csv")