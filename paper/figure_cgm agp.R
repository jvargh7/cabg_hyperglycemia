cgm_summary <- readRDS(paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_summary.RDS"))
inputdirectory = paste0(path_sh_folder,"/Glucose and Insulin Data/raw/CGM/")
files = list.files(inputdirectory)
files = files[regexpr("\\.csv",files)>0]

library(lubridate)
source("C:/code/external/functions/cgm/plot_agp.R")
timestamp_ranges = data.frame()
pdf(paste0(path_sh_folder,"/Glucose and Insulin Data/figures/figure_cgm agp.pdf"),width=12,height=8)

for (f in files){
  
  df = read_csv(paste0(inputdirectory,f)) %>% 
    mutate(sensorglucose = case_when(sensorglucose == "Low" ~ NA_real_,
                                     TRUE ~ as.numeric(sensorglucose)) )
  

  
  
  if(!is.POSIXct(df$timestamp) & ! is.Date(df$timestamp)){
    df = df %>% 
      mutate(timestamp = mdy_hms(timestamp))
  }
  
  timestamp_ranges = bind_rows(timestamp_ranges,
                               df %>% 
                                 summarize(min = min(timestamp,na.rm=TRUE),
                                           max = max(timestamp,na.rm=TRUE),
                                           n = sum(!is.na(sensorglucose))
                                           ) %>% 
                                 mutate(file = f))
  
  df  %>% 
    plot_agp(.,title=str_replace(f,"\\.csv",""),y_label = "Sensor Glucose (mg/dL)") %>% 
    print()
  
}

dev.off()

write_csv(timestamp_ranges,"data/cgm timestamp ranges.csv")