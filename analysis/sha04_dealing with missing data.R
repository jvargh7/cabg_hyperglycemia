rm(list = ls())
source(".Rprofile")
library(lubridate)

source("preprocessing/shapre_cgm for imputation.R")
# https://cran.r-project.org/web/packages/Amelia/vignettes/using-amelia.html

library(Amelia)
for (u_d in unique_devices){
      before_imputation = cgm_for_imputation %>% 
        dplyr::filter(subject_id == u_d) %>% 
        as.data.frame();
      print(u_d)
      if(sum(is.na(before_imputation$cgm_glucose))==0){
        # print()
        return(paste0(u_d," : Not imputed"))
      }
      # If cross-sectional units are specified these polynomials can be interacted with the cross-section unit 
      # to allow the patterns over time to vary between cross-sectional units.
      set.seed(2022)
      amelia_out = amelia(before_imputation, m = 10, ts = "timestamp", cs = "phase",intercs = TRUE,
                          polytime = 1,p2s=0,parallel = "no",
                          idvars = c("file","subject_id",
                                     "record_id","sensorglucose",
                                     "surgery_start_time","surgery_end_time","duration_surgery"));
     
      saveRDS(amelia_out,paste0(path_sh_folder,"/Glucose and Insulin Data/working/amelia/",u_d,".RDS"));
      
      png(paste0(path_sh_folder,"/Glucose and Insulin Data/working/amelia/",u_d," Imputation.png"),width = 1200,height=800);
      par(mfrow=c(2,3));
      
      map(unique(before_imputation$phase),
          function(p){
            tscsPlot(amelia_out, cs = p, main = p,
                     var = "cgm_glucose", ylim = c(0, 400))
          });
      dev.off();
      
      rm(before_imputation); gc();
      
}

