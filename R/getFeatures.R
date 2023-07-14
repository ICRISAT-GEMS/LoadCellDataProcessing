getFeatures <- function(x) {
  
  a <- Sys.time()
  
  spl.smth <- x
  
  # TS and load-cell cols of raw data
  dd.ts <- spl.smth[9:nrow(spl.smth), 6:ncol(spl.smth)] 
  rownames(dd.ts) <- spl.smth$old_unit[9:nrow(spl.smth)]
  dd.ts <- as.data.frame(t(dd.ts))
  dd.ts$TS <- ymd_hms(rownames(dd.ts))
  #dd.ts$TS <- ymd_hms(sub("X","",rownames(dd.ts)))
  dd.ts <- dd.ts[ ,c(ncol(dd.ts), 1:(ncol(dd.ts)-1))]
  rownames(dd.ts) <- seq(1, nrow(dd.ts))
  
  # TS, weather and ETref cols of raw data
  dd.meta <- spl.smth[1:8, 6:ncol(spl.smth)] 
  rownames(dd.meta) <- rownames(spl.smth)[1:8]
  dd.meta <- as.data.frame(t(dd.meta))
  dd.meta$TS <- dd.ts$TS
  dd.meta$date <- lubridate::date(dd.meta$TS)
  dd.meta$time <- strftime(dd.meta$TS, format="%H:%M:%S", tz="UTC")
  dd.meta <- dd.meta[ ,c((ncol(dd.meta)-2), (ncol(dd.meta)-1), ncol(dd.meta), 
                         1:(ncol(dd.meta)-3))]
  rownames(dd.meta) <- seq(1, nrow(dd.meta))
  
  
  # dd.xts <- xts(dd.ts.f[,-1], order.by = as.POSIXct(dd.ts.f$TS, format = "%Y-%m-%d %H:%M")) ## 1392 entries
  
  ## Create Input Data
  
  spl.smth.df <- dd.ts[,-1]
  
  # Features to be extracted: "maxET", "curv.maxET", "slope.maxET-6", "slope.07-maxET", "slope.00-07", "slope.19-23:45",
  # "auc.10-15", "sd.10-15", "auc.07-19", "sd.07-19", "auc.prop.10-15", "auc.prop.07-19",
  # "auc.night","total.auc","cos.sim.index"  
  
  ## Create Feature Set list
  F.mat <- matrix(NA, nrow = length(unique(dd.meta$date)), ncol = 15)
  
  colnames(F.mat) <- c("maxET", "slope.maxET-6", "slope.07-maxET", "slope.00-07", "slope.19-23:45", "curvmaxET", 
                       "total.auc","auc.10-15", "sd.10-15", "auc.prop.10-15", "auc.07-19", "sd.07-19",  
                       "auc.prop.07-19", "auc.night", "cos.sim.index")
  
  F.mat.list <- replicate(ncol(spl.smth.df), F.mat, simplify=F) # Make a list for each of the geno
  
  dates <- unique(dd.meta$date)
  
  # create a unique matrix to be able to calculate the daily features
  # without performing a double loop but using group function
  # frome dplyr
  
  # unfold the genotype values
  d <- melt(data = spl.smth.df, measure.vars = 1:ncol(spl.smth.df),
            variable.name = "sector")
  d$date <- dd.meta$date
  
  d_proc <- d %>% group_by(sector, date) %>%
    summarise(maxET = ETmax_fct(x = value),
              slope_min6_maxET = slope_min6_maxET(x = value),
              slope_7_maxET = slope_7_maxET(x = value),
              slope_0_7 = slope_t1_t2(x = value, t1 = 1, t2 = 29),
              slope_19_23h45 = slope_t1_t2(x = value, t1 = 77, t2 = 96),
              curvature = curvature_fct(x = value),
              AUC_tot = AUC_fct(x = value),
              AUC_10_15 = AUC_fct(x = value, t1 = 41, t2 = 61),
              SD_10_15 = SD_t1_t2(x = value, t1 = 41, t2 = 61),
              AUC_7_19 = AUC_fct(x = value, t1 = 29, t2 = 77),
              SD_7_19 = SD_t1_t2(x = value, t1 = 29, t2 = 77))
  
  d_proc$maxET[d_proc$maxET < 0] <- 0
  d_proc$AUC_10_15_prop <- d_proc$AUC_10_15/d_proc$AUC_tot
  d_proc$AUC_7_19_prop <- d_proc$AUC_7_19/d_proc$AUC_tot
  d_proc$AUC_night <- d_proc$AUC_tot - d_proc$AUC_7_19
  d_proc <- d_proc[, -2]
  
  # re-order the columns
  d_proc <- d_proc[, c(1:10, 13, 11, 12, 14:15)]
  
  
  F.mat.list <- split(x = d_proc, f = d_proc$sector)
  
  return(F.mat.list)
  
}