################
# TR_data_proc #
################

#' Transpiration data processing
#' 
#' Processing of the transpiration data collected on the load cells of the
#' LeasyScan platform.
#' 
#' The processing procedure is divided in three stages.
#' 
#' \strong{Stage 1.} The first stage start by the organisation of the data in
#' a certain format (extractRawLCmatrix()). Then the pipeline move with
#' a series of operations to detect outliers,
#' remove extreme values, and impute missing observations
#' (curateRawLC(), filterLCExtremeCols(), imputed.DF.final()). After that,
#' the function also filters the sectors for extreme value behaviour if the
#' moments of their distribution (0, 25, 50, 75, and 100\% quantiles) are
#' all outlying compared to the rest of the sector distribution values
#' (FilterLCExtremeCols()).
#' 
#' Then the pipeline produce a first evapotranspiration (ETr_obs) profile for
#' each sectors (getETr()). The function use a one lag difference to build
#' the ETr profile. This profile will also be cleaned for extreme observations.
#' using the same approach as before (comparison of the moments of the
#' distribution).
#' 
#' \strong{Stage 2} The first objective of the second stage is to calculate
#' a reference ETr profile (ETr_wth) using the weather information and the Penman-Monteith
#' equation. To realize that, the pipeline goes throught the following steps:
#' a) Organisation of the weather data information, b) calculation of
#' ETr_wth, c) remove the day with irrigation, d) Filtering of Etr_obs using ETr_wth to get ETr_filt
#' (calculation of r = ETr_wth/ETr_obs, values with r > 1 for day time (06:30-18:30)
#' and r > 1.5 for night time (18:30-06:30) are filtered), e) Use ETr_filt
#' to construct a first raw TR profile (TR_raw), and f) calculate the
#' raw TR parameters from TR_raw (See Value section for parameter definition). 
#' 
#' \strong{Stage 3} In this step, TR_raw time series are smoothed (TR_smth).
#' Then the same parameters are extracted but from TR_smth. 
#'
#' @param lc_data \code{data.frame} representing the load cell data information
#' containing the following columns (variables):
#' \describe{
#' \item{\code{sector}}{a character vector describing the sector position in
#' format (A-G)-X-YY (need to be check).}
#' \item{\code{genotype}}{a numeric or character vector indicating the
#' genotype numbers.}
#' \item{\code{g_alias}}{a character vector indicating the genotype identifiers.}
#' \item{\code{treatment}}{a character vector indicating the experiment treatment.
#' If not available set to NA.}
#' \item{\code{timestamp}}{a character vector indicating the time of measurement in format yyyy-mm-dd hh:mm:ss.}
#' \item{\code{Mass..g}}{a integer vector indicating the weight (mass) measured in g.}
#' }
#' The column names must be strictly equivalent to the one specified:
#' 'sector', 'genotype', 'g_alias', 'treatment', 'timestamp', 'Mass..g'.
#' Default = NULL.
#'
#' @param pe_data \code{data.frame} representing the plant eye data information
#'containing the following columns (variables):
#'
#'\describe{
#' \item{\code{sector}}{a character vector describing the sector position in
#' format (A-G)-X-YY (need to be checked).}
#' \item{\code{genotype}}{a numeric or character vector indicating the genotype numbers.}
#' \item{\code{replicate}}{a numeric vector indicating the replication number.
#' If not available or not existing set to NA.}
#' \item{\code{timestamp}}{a character vector indicating the time of measurement
#' in format yyyy-mm-dd hh:mm:ss.}
#' \item{\code{leaf_area}}{a integer vector indicating the leaf area.}
#' }
#'
#' The column names must be strictly equivalent to the one specified:
#' 'sector', 'genotype', 'replicate', 'timestamp', 'leaf_area'. Default = NULL.
#'
#' @param wth_data \code{data.frame} representing the weather data information
#'containing the following columns (variables):
#'
#' \describe{
#'  \item{\code{sensor}}{a character vector describing the sensor information}
#'  \item{\code{variable}}{a character vector indicating the type of weather information. Must be one of: Temperature (Â°C), Relative humidity (\%)), Windspeed average (m/s), Windspeed max (m/s), Solar radiation (W/(s*mÂ²)), Precipitation (mm), or Wind direction (Â°).}
#'  \item{\code{timestamp}}{a character vector indicating the time of measurement in format yyyy-mm-dd hh:mm:ss.}
#'  \item{\code{value}}{a numeric vector indicating the variable value.}
#' }
#'
#' The column names must be strictly equivalent to the one specified:
#' 'sensor', 'variable', 'timestamp', 'value'. Default = NULL.
#' 
#' @param sensor_data \code{data.frame} representing the sensor data information
#'containing the following columns (variables):
#'
#' \describe{
#'  \item{\code{sector}}{a character vector describing the sector position.}
#'  \item{\code{sensor}}{a character vector indicating the sensor information.}
#' }
#'
#' The column names must be strictly equivalent to the one specified:
#' 'sector', 'sensor'. Default = NULL.
#' 
#' @param lastDate \code{character} string indicating the last day of the
#' experiment. If not specified, it will be set as the last date find
#' in the lc_data minus one day. Default = NULL.
#'
#' @param irrg.dts \code{Character vector} of irrigation dates, each date mentioned as
#' in YYYY-MM-DD format. If no such date is needed, same as the last date.
#' Default = NULL.
#' 
#' @param skew_test \code{Logical value} specifying if skewness test
#' should be used to filter weather data. Default = TRUE.
#' 
#' @param get_feature_h2 \code{Logical value} specifying if the heritability
#' of the features (using gam function) should be calculated? Default = FALSE.
#' 
# #' @param res_path path location where the results will be saved. Default = NULL.
#' 
#' @return Return:
#' 
#' A \code{list} that contain two lists: one for the raw transpiration results,
#' the other for the smooth transpiration results. In both case. the list contains
#' the following features organised per genotype and per day
#' (example for results from TR raw):
#' 
#'  \item{TR_raw}{Transpiration per genotype and time point (here every 15min)}
#'  \item{Max_TR_raw}{Maximum transpiration per genotype and per day}
#'  \item{slope_6pt_bfr_maxTR_raw}{Slope of the curvature between 6 data points (90 min)}
#'  \item{slope_00_07_raw}{slope of the curve between 00:00 and 07:00}
#'  \item{slope_07_maxTR_raw}{slope of the curve between 07:00 and maxTR}
#'  \item{slope_19h_23h45_raw}{slope of the curve between 19:00 and 23:45}
#'  \item{curve_maxTR_raw}{curvature or angle of the curve at maxTR}
#'  \item{total_auc_raw}{Total area under the curve}
#'  \item{auc_10h_15h_raw}{area under the curve 10-15h}
#'  \item{sd_10h_15h_raw}{standard deviation TR values 10-15h}
#'  \item{prop_auc_10h_15h_raw}{proportion of area under the curve between 10-15h}
#'  \item{auc_7h_19h_raw}{Total area under the curve between 07:00 and 19:00}
#'  \item{sd_7h_19h_raw}{standard deviation TR values between 07:00 and 19:00}
#'  \item{prop_auc_7h_19h_raw}{proportion of area under the curve between 07:00 and 19:00}
#'  \item{auc_night_raw}{Area under the curve during the night}
#'  \item{sim_ETobs_ETPenMont_raw}{similarity between the ETr profile and the Penman Monteith ET}
#'
#' @author Soumyashree Kar, ICRISAT-GEMS
#'
#' @references
#' 
#' Kar, S., Tanaka, R., Korbu, L. B., Kholova, J., Iwata, H., Durbha, S. S., ...
#' & Vadez, V. (2020). Automated discretization of ‘transpiration restriction
#' to increasing VPD’features from outdoors high-throughput phenotyping data.
#' Plant methods, 16(1), 1-20.
#'
#' @examples
#'
#' library(plyr)
#' 
#' data(lc_data)
#' data(pe_data)
#' data(wth_data)
#' data(sensor_data)
#' 
#' \dontrun{
#' 
#' TR_res <- TR_data_proc(lc_data = lc_data, pe_data = pe_data,
#' wth_data = wth_data, sensor_data = sensor_data,
#' lastDate = '2018-03-06', skew_test = FALSE)
#' 
#' }
#' 
#' @import BioFTF
#' @import dplyr
#' @import ggplot2
#' @import highfrequency
#' @import lubridate
#' @import mgcv
#' @import PerformanceAnalytics
#' @import splitstackshape
#' @import tsfeatures
#' @importFrom h2o ifelse
#' @importFrom grDevices boxplot.stats
#' @importFrom graphics boxplot lines
#' @importFrom plyr create_progress_bar
#' @importFrom psych describe
#' @importFrom nonlinearTseries nonLinearNoiseReduction
#' @importFrom stats dist lm median na.exclude na.omit quantile sd smooth.spline time ts var
#' @importFrom utils head
#' @importFrom xts xts
#' @importFrom zoo na.aggregate.default na.approx na.locf
# #' @import easypackage
# #' @import tidyverse
# #' @import readxl
# #' @import sqldf
# #' @import plantecophys
# #' @import stringr
# #' @import signal
# #' @import hms
#'
#'
#' @export
#'


TR_data_proc <- function(lc_data = NULL, pe_data = NULL, wth_data = NULL,
                         sensor_data = NULL, lastDate = NULL,
                         irrg.dts = NULL, skew_test = TRUE, get_feature_h2 = FALSE){
  
  ##### check function arguments format ----
  lc_data <- check_lc_data(lc_data)
  pe.df <- check_pe_data(pe_data)
  rm(pe_data)
  clm.df <- check_wth_data(wth_data)
  rm(wth_data)
  sensor.unit.df <- check_sensor_data(sensor_data)
  rm(sensor_data)
  
  ##### creation of extra variables ----
  
  # get the starting and end date of the experiment (range)
  E_range <- exp_range(lc_data$timestamp)

  # be careful to have limit date with second set as 00
  Date1 <- E_range$date_min
  Date2 <- E_range$date_max
  
  # fix the last date and irrigation date to last date of the experiment
  # if not provided
  if(is.null(lastDate)){lastDate <- substr(Date2, 1, 10)}
  if(is.null(irrg.dts)){irrg.dts <- lastDate} 
  irrg.dts <- as.Date(irrg.dts) # in Date format

  # transform the time stamp data into dd-mm-yy hh:ss format
  lc_data$timestamp <- time_format(lc_data$timestamp)

  # form the meta data file
  lc_data_unique_plot <- lc_data[match(unique(lc_data$unit),
                                       lc_data$unit), ]

  meta.d <- data.frame(unit = lc_data_unique_plot$unit,
                          old_unit = lc_data_unique_plot$unit,
                          Experiment = 'Exp',
                          Treatment = lc_data_unique_plot$treatment,
                          Species = 'species',
                          Genotype = lc_data_unique_plot$genotype,
                          G..Alias = lc_data_unique_plot$g_alias,
                          Replicates = NA)
  
  # Include the species ID e.g. 1 for 'Pearl Millet' as per the data
  meta.d.sp <- meta.d[meta.d$Species==unique(meta.d$Species)[1], ]
  
  ####### Stage-I: Process LC data and generate ETr matrix ----
  st.time <- Sys.time()
  
  # Get load cells data i.e. weights of sector
  m.lc <- lc_data
  rm(lc_data)
  
  # Find sectors with missing metadata
  noEntrySecs <- which(!unique(m.lc$unit) %in% unique(meta.d.sp$unit))
  noEntrySecNms <- unique(m.lc$unit)[noEntrySecs]
  
  # Remove sectors-without-metadata from original loadcells file
  m.lc <- m.lc[! m.lc$unit %in% noEntrySecNms, ]
  
  # Extract matrix of loadcell data #
  LC.MAT.OP <- extractRawLCmatrix(x = m.lc, y = meta.d.sp, z = lastDate)
  
  LC.MAT.f <- LC.MAT.OP$LC.MAT.f

  # Add metadata to LC matrix #
  # select from Metadata: "unit", "old_unit", "Genotype, "G..Alias", "Replicates"
  meta.d.LCmat <- meta.d.sp[meta.d.sp$unit %in% colnames(LC.MAT.f)[-1],
                            c(1,2,6,7,8)]

  LC.MAT.f.t <- as.data.frame(t(LC.MAT.f))

  colnames(LC.MAT.f.t) <- LC.MAT.f$TS

  LC.MAT.f.t <- LC.MAT.f.t[-1,]

  # reorder rows of 'meta.d.sp' according to rownames/unit of LC.MAT.f.t
  meta.LCDF <- meta.d.LCmat[order(match(meta.d.LCmat$unit, rownames(LC.MAT.f.t))), ]

  LC.MAT.raw <- as.data.frame(cbind(meta.LCDF, LC.MAT.f.t))

  # Outlier detection, removal and imputation of LC Matric to generate ETr profiles #
  # Pre-process raw LC data: outliers removal and imputation #
  imputed.DF.final <- curateRawLC(x = LC.MAT.f, y = meta.LCDF,
                                  col_names = colnames(LC.MAT.raw))

  # Identify the highly extreme valued sectors #
  err.sec.info <- filterLCExtremeCols(x = imputed.DF.final, y = meta.LCDF)

  err.sec.nm <- err.sec.info$err.sec.NM

  # Remove the err.cols i.e. sectors with extreme values
  imp_data_sec_rem <- imputed.DF.final[!imputed.DF.final$unit %in% err.sec.nm, ]

  # Generate ETr profiles from "imp_data_sec_rem" dataframe #
  et.vals <- getETr(x = imp_data_sec_rem)

  et.obs <- et.vals$obsETr_core

  ETr_Meta <- et.vals$obsETr_meta

  # Convert ETr in grams to mm #
  ETr_Meta[, 6:ncol(ETr_Meta)] <- et.obs*4/1000

  # Identify error plots from ETr values using the similar method as above #
  ETr_err.sec.info <- filterETrExtremeCols(x = ETr_Meta, y = meta.LCDF)

  err.sec.nm <- ETr_err.sec.info$ETr_err.sec.NM

  # Remove the err.cols i.e. sectors with extreme values #
  ETr_Meta_ERRsec.rmvd <- ETr_Meta

  ETr_Meta_ERRsec.rmvd <- ETr_Meta_ERRsec.rmvd[!ETr_Meta_ERRsec.rmvd$unit %in% err.sec.nm, ]

  ####### Stage-II: Process Weather data to obtain ETref and ETr ratio matrix ----

  clm.df.mapped <- clm.df[clm.df$sensor %in% sensor.unit.df$sensor, ]
  unq.clm.var <- unique(clm.df.mapped$variable)

  # Extract weather variables individually; Assign numbers appropriately #
  temperature.DF <- extractWthrVar(y = clm.df.mapped, sel_wth_var = unq.clm.var[1],
                                   skew_test = skew_test)
  temperature.DF$ts <- temperature.DF$ts + 5.5*60*60

  relHUM.DF <- extractWthrVar(y = clm.df.mapped, sel_wth_var = unq.clm.var[2],
                              skew_test = skew_test)
  relHUM.DF$ts <- relHUM.DF$ts + 5.5*60*60

  windS.DF <- extractWthrVar(y = clm.df.mapped, sel_wth_var = unq.clm.var[3],
                             skew_test = skew_test)
  windS.DF$ts <- windS.DF$ts + 5.5*60*60

  solarRad.DF <- extractWthrVar(y = clm.df.mapped, sel_wth_var = unq.clm.var[5],
                                skew_test = skew_test)
  solarRad.DF$ts <- solarRad.DF$ts + 5.5*60*60

  # Read the ET obs data and get the first-last time stamps #
  et.obs <- ETr_Meta_ERRsec.rmvd
  et.f.ts <- colnames(et.obs[6]);
  et.l.ts <- colnames(et.obs[ncol(et.obs)])

  Date1<-ymd_hms(Date1)  ## Make sure it's a Complete Cycle
  Date2<-ymd_hms(Date2)

  dates <- seq(Date1, Date2, by="min")

  # Subset weather data #
  temperature.DF.filt <- subset(temperature.DF, ts %in% dates)
  relHUM.DF.filt <- subset(relHUM.DF, ts %in% dates)
  solarRad.DF.filt <- subset(solarRad.DF, ts %in% dates)
  windS.DF.filt <- subset(windS.DF, ts %in% dates)


  # Create dataframe for Subsetted weather data #
  weather.DF <- as.data.frame(matrix(nrow = length(dates), ncol = 6))

  names(weather.DF) <- c("TS", "Temp", "RH", "VPD", "SR", "WS")

  weather.DF[, 1]<-dates

  i<-nrow(weather.DF)
  pbar <- create_progress_bar('text')
  pbar$init(i)


  for(i in 1:nrow(weather.DF))
  {
    if(i == 1)print("Weather matrix timestamp mapping status")
    ifelse(weather.DF$TS[i] %in% temperature.DF.filt$ts,
           weather.DF[i, 2] <- temperature.DF.filt$value[temperature.DF.filt$ts==weather.DF$TS[i]],
           weather.DF[i, 2] <- NA)

    ifelse(weather.DF$TS[i] %in% relHUM.DF.filt$ts,
           weather.DF[i, 3] <- relHUM.DF.filt$value[relHUM.DF.filt$ts==weather.DF$TS[i]],
           weather.DF[i, 3] <- NA)

    ifelse(weather.DF$TS[i] %in% solarRad.DF.filt$ts,
           weather.DF[i, 5] <- solarRad.DF.filt$value[solarRad.DF.filt$ts==weather.DF$TS[i]],
           weather.DF[i, 5] <- NA)

    ifelse(weather.DF$TS[i] %in% windS.DF.filt$ts,
           weather.DF[i, 6] <- windS.DF.filt$value[windS.DF.filt$ts==weather.DF$TS[i]],
           weather.DF[i, 6] <- NA)
    pbar$step()
  }


  # Preprocess each weather variable except VPD #
  weather.DF[ , 2] <- prepcsWthr(x = weather.DF, y = 2) # temperature
  weather.DF[ , 3] <- prepcsWthr(x = weather.DF, y = 3) # relative humidity
  weather.DF[ , 5] <- prepcsWthr(x = weather.DF, y = 5) # solar radiation
  weather.DF[ , 6] <- prepcsWthr(x = weather.DF, y = 6) # wind speed

  #### VPD calculation

  # Compute VPD and insert into the weather DF #
  SVP <- 610.7*(10^(7.5*weather.DF[ ,2]/(237.3+weather.DF[ ,2])))
  VPD <- ((1 - (weather.DF[ ,3]/100))*SVP)/1000
  weather.DF[ , 4] <- VPD

  wthr.DFxts.TS <- xts(weather.DF, order.by = as.POSIXct(weather.DF$TS, format="%Y-%m-%d %H:%M"))

  wthr.DFagg15min = highfrequency::aggregateTS(ts = wthr.DFxts.TS, alignBy ="minutes",
                                               alignPeriod =15, dropna=TRUE)

  ### Run below if TS is like 00:14:00, 00:29:00, etc. Else save directly ###
  ts.x <- ymd_hms(wthr.DFagg15min$TS)
  ts.x <- ts.x + hms("00:01:00")
  wthr.DFagg15min$TS <- as.character(ts.x)

  # Check and Filter duration of weather data as per observed ET
  et.obs.TS <- colnames(et.obs)[6:ncol(et.obs)]

  # IMP: Check and set the date format of et.obs.TS #
  et.obs.TS <- ymd_hms(et.obs.TS)
  et.obs.TS.chr <- as.character(et.obs.TS)
  wthr.DFagg15min_t <- wthr.DFagg15min[wthr.DFagg15min$TS %in% et.obs.TS.chr, ]

  # Calculate Penman Monteith ET #
  wthr.df1 <- calculateETref(wthr.DFagg15min)
  wthr.ETref.df <- as.data.frame(wthr.df1)
  empty.MAT <- matrix(nrow = 8, ncol = (ncol(et.obs)-nrow(wthr.df1)))

  # select columns "Temp"  "RH"    "VPD"   "SR"    "WS"    "Tmax"  "Tmin"  "ETref"
  empty.MAT.wthr.ETref <- as.data.frame(cbind(empty.MAT, t(wthr.ETref.df[,c(2:6, 9:11)])))
  colnames(empty.MAT.wthr.ETref) <- colnames(et.obs)
  wthr.ETref.ETobs <- as.data.frame(rbind(empty.MAT.wthr.ETref, et.obs))

  ### Start ET0 Ratio-based Thresholding ###
  ET_ratio_mat <- generateETrRatio(x = wthr.ETref.ETobs)
  ETr_baseFILE <- wthr.ETref.ETobs
  baseFILE <- ET_ratio_mat

  # correct for irrigation (alternative way compare to the previous version)

  # Prepare a copy of raw data as the Input File for the Smoothing process
  ETr_smth_IP <- ETr_baseFILE
  Ratio_smth_IP <- baseFILE

  col_names_ref <- colnames(ETr_smth_IP)

  # for now, we assume that that the colnames are the same

  meta_inf_pos <- which(col_names_ref %in% colnames(meta.d.LCmat))
  date_pos <- which(!(col_names_ref %in% colnames(meta.d.LCmat)))

  sel_dates <- col_names_ref[date_pos]
  sel_dates <- strsplit(x = sel_dates, ' ')
  sel_dates <- unlist(lapply(sel_dates, FUN = function(x) x[1]))

  sel_dates <- which(!(sel_dates %in% as.character(irrg.dts)))

  sel_col <- c(meta_inf_pos, date_pos[sel_dates])

  # remove the information that fall on a day with irrigation
  ETr_smth_IP <- ETr_smth_IP[ , sel_col]
  Ratio_smth_IP <- Ratio_smth_IP[ , sel_col]


  # Thresholding using 2 time-windows 06:30 to 18:30 and remaining #
  by_Genotype <- Ratio_smth_IP[-c(1:8), c(3, 6:ncol(Ratio_smth_IP))] %>% group_by(Genotype)

  by_Genotype_Mean <- by_Genotype  %>% summarise_all(~mean(.))

  by_Genotype_Mean$Genotype <- factor(by_Genotype_Mean$Genotype)

  geno.ETr <- as.data.frame(t(by_Genotype_Mean))

  geno.ETr <- geno.ETr[-1, ]

  geno.ETr <- as.data.frame(apply(geno.ETr, 2, function(x) {as.numeric(as.character(x))}))

  colnames(geno.ETr) <- paste0("G_", by_Genotype_Mean$Genotype)

  ETref <- as.data.frame(Ratio_smth_IP[8, 6:ncol(Ratio_smth_IP)])

  baseDF <- as.data.frame(cbind(t(ETref), geno.ETr))
  colnames(baseDF)[1] <- c("ETref")


  # Check date format in rownames and run accordingly
  baseDF$TS <- ymd_hms(rownames(baseDF));
  baseDF$date <- lubridate::date(baseDF$TS);
  baseDF$time <- strftime(baseDF$TS, format="%H:%M:%S", tz="UTC")
  baseDF$solarRAD <- as.numeric(as.character(Ratio_smth_IP[4, 6:ncol(Ratio_smth_IP)])) ## Need to convert row to vector using c()

  baseDF <- baseDF[ ,c((ncol(baseDF)-3), (ncol(baseDF)-2), (ncol(baseDF)-1), ncol(baseDF), 1:(ncol(baseDF)-4))]


  ### Steps to find the Thresholds and filter raw ETr ###

  # 1. First find the set of unique dates.
  # 2. Divide the whole data into 2 time-windows, TW (06:30 - 18:30, Rest).
  # 3. For each TW, get the threshold values.
  # 4. Partition ETr as per TWs and apply thresholds.
  # 5. Filter ETr, impute and merge the TWs.


  # 1. First find the set of unique dates
  unq.dts <- unique(baseDF$date)

  # 2. Divide the whole data i.e. baseDF into 2 time-windows, TW (06:30 - 18:30, Rest)
  base_TW1 <- baseDF[baseDF$time >= "06:30:00" & baseDF$time < "18:30:00", ]
  base_TW2 <- baseDF[!baseDF$time %in% base_TW1$time, ]

  # 3. For each TW, get the threshold values.
  baseTW1_ThreshVALS <- genThreshVal(x = unq.dts, y = base_TW1)
  baseTW2_ThreshVALS <- genThreshVal(x = unq.dts, y = base_TW2)

  TW1.thresh <- floor(median(baseTW1_ThreshVALS$Q_75, na.rm = TRUE))
  TW2.thresh <- ceiling(median(baseTW2_ThreshVALS$Q_75, na.rm = TRUE))

  # 4. Make ETr partitions
  ETr_TWs <- dataPART(x = ETr_smth_IP)

  ETr_P1 <- ETr_TWs$P1
  ETr_P2 <- ETr_TWs$P2


  # Make ETr Ratio partitions
  ETr_Ratio_TWs <- dataPART(x = Ratio_smth_IP)

  ETr_Ratio_P1 <- ETr_Ratio_TWs$P1
  ETr_Ratio_P2 <- ETr_Ratio_TWs$P2


  # Apply TW-specific thresholds
  ETr_filt_TW1<- threshETr(x = TW1.thresh, y = ETr_Ratio_P1, z = ETr_P1)
  ETr_filt_TW2<- threshETr(x = TW2.thresh, y = ETr_Ratio_P2, z = ETr_P2)

  # Save filtered ETr data
  ETr_filt_TW1_ord <- ordFiltETr(x = ETr_filt_TW1, y = ETr_smth_IP)
  ETr_filt_TW2_ord <- ordFiltETr(x = ETr_filt_TW2, y = ETr_smth_IP)

  ETr_filt_ord <- as.data.frame(rbind(ETr_filt_TW1_ord, ETr_filt_TW2_ord))

  ETr_filt_ord$TS <- ymd_hms(rownames(ETr_filt_ord))

  ETr_filt_ord <- ETr_filt_ord[order(ETr_filt_ord$TS), ]

  ETr_filt_FILE <- ETr_smth_IP
  subs.d <- as.matrix(t(ETr_filt_ord[,-1]))

  ETr_filt_FILE[9:nrow(ETr_filt_FILE), 6:ncol(ETr_filt_FILE)] <- subs.d

  # 5. Thresholded/Filtered ETr interpolation
  ETr_filt_imputed_FILE <- ETr_filt_FILE
  subs.d.imp <- subs.d
  na.sum <- c()

  for(i in 1:nrow(subs.d.imp))
  {
    subs.d.imp[i, ] <- na.aggregate.default(subs.d.imp[i, ])
    na.sum[i] <- sum(is.na(subs.d.imp[i, ]))
  }
  # print(paste0("#Sectors still with NA: ", (length(na.sum) - length(!na.sum == 0))))

  ETr_filt_imputed_FILE[9:nrow(ETr_filt_imputed_FILE), 6:ncol(ETr_filt_imputed_FILE)] <- subs.d.imp

  ### Start Tr extraction to get raw Tr profiles ###
  # Check format and set TS
  pe.df$TS <- ymd_hms(pe.df$timestamp)

  pe.df$date <- lubridate::date(pe.df$TS)

  sel.secs <- unique(na.omit(ETr_filt_imputed_FILE$old_unit))

  pe.df.ETr <- pe.df[pe.df$date %in% unq.dts & pe.df$sector %in% sel.secs, ]
  pe.df.ETr <- pe.df.ETr[, -4] # remove timestep information

  colnames(pe.df.ETr) <- c("old_unit", "Genotype", "Replicates", "LeafArea3D", "TS", "date")
  pe.df.ETr$Genotype <- factor(pe.df.ETr$Genotype)
  pe.df.ETr$Replicates <- factor(pe.df.ETr$Replicates)

  pe.ETr.grpDT <- pe.df.ETr %>% group_by(old_unit, date, Genotype, Replicates) %>%
    dplyr::summarise(Max = max(LeafArea3D, na.rm=TRUE))

  names(pe.ETr.grpDT)[5] <- "LeafArea3D"

  # make a matrix of LA3D of dim = ETr_core data, then calculate TR #
  ## LAI=((((3DLA/100) - 29.9)/0.36)*(1/0.26))/10000 ##
  ## T = (1-exp(-0.463*LAI))*ETr ##

  LAI.mat <- matrix(NA, nrow = length(sel.secs), ncol = ncol(subs.d))
  rownames(LAI.mat) <- sel.secs

  # sort LAI.mat rownames as per the ETR_smooth file
  LAI.mat <- LAI.mat[order(match(rownames(LAI.mat),
                                 ETr_filt_imputed_FILE[9:nrow(ETr_filt_imputed_FILE), "old_unit"])),]

  LAI.all.dates <- LAI.mat; unq.dts.copy <- unq.dts

  LAI.all.dates <- LAI.all.dates[ , c(1:length(unq.dts))]
  colnames(LAI.all.dates) <- c(as.character(unq.dts.copy))


  # Calculate raw Transpiration, Tr
  Tr_OP <- calculateTr(x = ETr_filt_imputed_FILE, y = pe.df.ETr, z = LAI.mat,
                       d = unq.dts, LAI.all.dates = LAI.all.dates)

  LAI.mat <- Tr_OP$LAI.mat

  raw.trans <- ETr_filt_imputed_FILE

  raw.trans[9:nrow(raw.trans), 6:ncol(raw.trans)] <- Tr_OP$Trans.mat


  ### Feature Extraction of RAW Transpiration Data ###
  featuresRES <- getFeatures(x = raw.trans)

  allFeatures <- featuresRES$allFeatures

  # create H2 dataframe to store H2 est. of each feature for each day
  F.He <- as.data.frame(matrix(NA, nrow = length(unq.dts), ncol = 15)) # Date-ROW, feature-COL
  colnames(F.He) <- c("maxET", "slope.maxET-6", "slope.07-maxET", "slope.00-07", "slope.19-23:45", "curvmaxET",
                      "total.auc","auc.10-15", "sd.10-15", "auc.prop.10-15", "auc.07-19", "sd.07-19",
                      "auc.prop.07-19", "auc.night", "cos.sim.index")
  rownames(F.He) <- unq.dts

  # For now, do not save features per day.

  if(get_feature_h2){

    # featureHeRES <- getFeatureHe(x = allFeatures, y = raw.trans, d = unq.dts, p = opPATH.raw)
    featureHeRES <- getFeatureHe(x = allFeatures, y = raw.trans, d = unq.dts,
                                 F.He = F.He)

    }



  ### save all features as feature Time Series ###
  ### Each feature set: dim(length(unq.dts) x (nrow(raw.trans)-8)) ###

  ## Prepare data for 'each feature'
  maxET <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  slope.maxET.6 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  slope.07maxET <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  slope.00.07 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  slope.19.2345 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  curvmaxET <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  total.auc <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  auc.10.15 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  sd.10.15 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  auc.prop.10.15 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  auc.07.19 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  sd.07.19 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  auc.prop.07.19 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  auc.night <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  cos.sim.index <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))

  for (j in 1:(nrow(raw.trans)-8)){

    # "maxET", "slope.maxET-6", "slope.07-maxET", "slope.00-07", "slope.19-23:45", "curvmaxET",
    # "total.auc","auc.10-15", "sd.10-15", "auc.prop.10-15", "auc.07-19", "sd.07-19",
    # "auc.prop.07-19", "auc.night", "cos.sim.index"

    for(d in 1:length(unq.dts))
    {maxET[j, d] <- data.frame(allFeatures[[j]][d, 1])
    slope.maxET.6[j, d] <- data.frame(allFeatures[[j]][d, 2])
    slope.07maxET[j, d] <- data.frame(allFeatures[[j]][d, 3])
    slope.00.07 [j, d] <- data.frame(allFeatures[[j]][d, 4])
    slope.19.2345[j, d] <- data.frame(allFeatures[[j]][d, 5])

    curvmaxET[j, d] <- data.frame(allFeatures[[j]][d, 6])
    total.auc[j, d] <- data.frame(allFeatures[[j]][d, 7])
    auc.10.15[j, d] <- data.frame(allFeatures[[j]][d, 8])
    sd.10.15 [j, d] <- data.frame(allFeatures[[j]][d, 9])
    auc.prop.10.15[j, d] <- data.frame(allFeatures[[j]][d, 10])

    auc.07.19[j, d] <- data.frame(allFeatures[[j]][d, 11])
    sd.07.19[j, d] <- data.frame(allFeatures[[j]][d, 12])
    auc.prop.07.19[j, d] <- data.frame(allFeatures[[j]][d, 13])
    auc.night [j, d] <- data.frame(allFeatures[[j]][d, 14])
    cos.sim.index[j, d] <- data.frame(allFeatures[[j]][d, 15])
    }

  }

  names(maxET)=names(slope.maxET.6)=names(slope.07maxET)=names(slope.00.07)=names(slope.19.2345)<-unq.dts
  names(curvmaxET)=names(total.auc)=names(auc.10.15)=names(sd.10.15)=names(auc.prop.10.15)<-unq.dts
  names(auc.07.19)=names(sd.07.19)=names(auc.prop.07.19)=names(auc.night)=names(cos.sim.index)<-unq.dts

  # Features results

  # maximum transpiration
  Max_TR_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], maxET))

  # slope of the curvature between 6 data points (90 min)
  slope_6pt_bfr_maxTR_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], slope.maxET.6))

  # slope of the curve between 00:00 and 07:00
  slope_00_07_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], slope.00.07))

  # slope of the curve between 07:00 and maxTR
  slope_07_maxTR_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], slope.07maxET))

  # slope of the curve between 19:00 and 23:45
  slope_19h_23h45_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], slope.19.2345))

  # curvature or angle of the curve at maxTR
  curve_maxTR_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], curvmaxET))

  # Total area under the curve
  total_auc_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], total.auc))

  # area under the curve 10-15h
  auc_10h_15h_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], auc.10.15))

  # standard deviation TR values 10-15h
  sd_10h_15h_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], sd.10.15))

  # proportion of area under the curve between 10-15h
  prop_auc_10h_15h_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], auc.prop.10.15))

  # Total area under the curve between 07:00 and 19:00
  auc_7h_19h_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], auc.07.19))

  # standard deviation TR values between 07:00 and 19:00
  sd_7h_19h_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], sd.07.19))

  # proportion of area under the curve between 07:00 and 19:00
  prop_auc_7h_19h_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], auc.prop.07.19))

  # Area under the curve during the night
  auc_night_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], auc.night))

  # similarity between the ETr profile and the Penman Monteith ET
  sim_ETobs_ETPenMont_raw <- as.data.frame(cbind(raw.trans[9:nrow(raw.trans), 1:5], cos.sim.index))


  ###### Stage III: Generate smooth ETr and obtain smooth Tr features ----
  # ETr smoothing
  smoothETrMAT <- smoothETr(x = subs.d.imp)

  # ETr smooth profiles per load cell...
  ETr_smoothFILE <- ETr_filt_imputed_FILE
  ETr_smoothFILE[9:nrow(ETr_smoothFILE) , 6:ncol(ETr_smoothFILE)] <- smoothETrMAT/1000

  # Calculate Tr from smooth ETr
  Tr_OP <- calculateTr(x = ETr_smoothFILE, y = pe.df.ETr, z = LAI.mat, d = unq.dts,
                       LAI.all.dates = LAI.all.dates)

  # smooth transpiration data
  smth.trans.mat <- Tr_OP$Trans.mat
  smth.trans <- ETr_smoothFILE
  smth.trans[9:nrow(smth.trans), 6:ncol(smth.trans)] <- smth.trans.mat

  ### Feature Extraction of SMOOTH Transpiration Data ###
  featuresRES <- getFeatures(x = smth.trans)

  allFeatures <- featuresRES$allFeatures

  # create H2 dataframe to store H2 est. of each feature for each day
  F.He <- as.data.frame(matrix(NA, nrow = length(unq.dts), ncol = 15)) # Date-ROW, feature-COL
  colnames(F.He) <- c("maxET", "slope.maxET-6", "slope.07-maxET", "slope.00-07", "slope.19-23:45", "curvmaxET",
                      "total.auc","auc.10-15", "sd.10-15", "auc.prop.10-15", "auc.07-19", "sd.07-19",
                      "auc.prop.07-19", "auc.night", "cos.sim.index")
  rownames(F.He) <- unq.dts

  if(get_feature_h2){

    featureHeRES <- getFeatureHe(x = allFeatures, y = smth.trans, d = unq.dts,
                                 F.He = F.He)

  }

  ### save all features as feature Time Series ###
  ### Each feature set: dim(length(unq.dts) x (nrow(raw.trans)-8)) ###
  
  ###FCT: Make a function to process all features matrices

  ## Prepare data for 'each feature'
  maxET <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  slope.maxET.6 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  slope.07maxET <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  slope.00.07 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  slope.19.2345 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  curvmaxET <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  total.auc <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  auc.10.15 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  sd.10.15 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  auc.prop.10.15 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  auc.07.19 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  sd.07.19 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  auc.prop.07.19 <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  auc.night <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))
  cos.sim.index <- as.data.frame(matrix(nrow = (nrow(raw.trans)-8), ncol = length(unq.dts)))

  for (j in 1:(nrow(raw.trans)-8)){

    # "maxET", "slope.maxET-6", "slope.07-maxET", "slope.00-07", "slope.19-23:45", "curvmaxET",
    # "total.auc","auc.10-15", "sd.10-15", "auc.prop.10-15", "auc.07-19", "sd.07-19",
    # "auc.prop.07-19", "auc.night", "cos.sim.index"

    for(d in 1:length(unq.dts))
    {maxET[j, d] <- data.frame(allFeatures[[j]][d, 1])
    slope.maxET.6[j, d] <- data.frame(allFeatures[[j]][d, 2])
    slope.07maxET[j, d] <- data.frame(allFeatures[[j]][d, 3])
    slope.00.07 [j, d] <- data.frame(allFeatures[[j]][d, 4])
    slope.19.2345[j, d] <- data.frame(allFeatures[[j]][d, 5])

    curvmaxET[j, d] <- data.frame(allFeatures[[j]][d, 6])
    total.auc[j, d] <- data.frame(allFeatures[[j]][d, 7])
    auc.10.15[j, d] <- data.frame(allFeatures[[j]][d, 8])
    sd.10.15 [j, d] <- data.frame(allFeatures[[j]][d, 9])
    auc.prop.10.15[j, d] <- data.frame(allFeatures[[j]][d, 10])

    auc.07.19[j, d] <- data.frame(allFeatures[[j]][d, 11])
    sd.07.19[j, d] <- data.frame(allFeatures[[j]][d, 12])
    auc.prop.07.19[j, d] <- data.frame(allFeatures[[j]][d, 13])
    auc.night [j, d] <- data.frame(allFeatures[[j]][d, 14])
    cos.sim.index[j, d] <- data.frame(allFeatures[[j]][d, 15])
    }

  }

  names(maxET)=names(slope.maxET.6)=names(slope.07maxET)=names(slope.00.07)=names(slope.19.2345)<-unq.dts
  names(curvmaxET)=names(total.auc)=names(auc.10.15)=names(sd.10.15)=names(auc.prop.10.15)<-unq.dts
  names(auc.07.19)=names(sd.07.19)=names(auc.prop.07.19)=names(auc.night)=names(cos.sim.index)<-unq.dts

  # Features results

  # maximum transpiration
  Max_TR_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], maxET))

  # slope of the curvature between 6 data points (90 min)
  slope_6pt_bfr_maxTR_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], slope.maxET.6))

  # slope of the curve between 00:00 and 07:00
  slope_00_07_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], slope.00.07))

  # slope of the curve between 07:00 and maxTR
  slope_07_maxTR_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], slope.07maxET))

  # slope of the curve between 19:00 and 23:45
  slope_19h_23h45_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], slope.19.2345))

  # curvature or angle of the curve at maxTR
  curve_maxTR_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], curvmaxET))

  # Total area under the curve
  total_auc_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], total.auc))

  # area under the curve 10-15h
  auc_10h_15h_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], auc.10.15))

  # standard deviation TR values 10-15h
  sd_10h_15h_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], sd.10.15))

  # proportion of area under the curve between 10-15h
  prop_auc_10h_15h_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], auc.prop.10.15))

  # Total area under the curve between 07:00 and 19:00
  auc_7h_19h_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], auc.07.19))

  # standard deviation TR values between 07:00 and 19:00
  sd_7h_19h_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], sd.07.19))

  # proportion of area under the curve between 07:00 and 19:00
  prop_auc_7h_19h_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], auc.prop.07.19))

  # Area under the curve during the night
  auc_night_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], auc.night))

  # similarity between the ETr profile and the Penman Monteith ET
  sim_ETobs_ETPenMont_smth <- as.data.frame(cbind(smth.trans[9:nrow(smth.trans), 1:5], cos.sim.index))

  #### return results ----

  res_raw <- list(TR_raw = raw.trans,
                  Max_TR_raw = Max_TR_raw,
                  slope_6pt_bfr_maxTR_raw = slope_6pt_bfr_maxTR_raw,
                  slope_00_07_raw = slope_00_07_raw,
                  slope_07_maxTR_raw = slope_07_maxTR_raw,
                  slope_19h_23h45_raw = slope_19h_23h45_raw,
                  curve_maxTR_raw = curve_maxTR_raw,
                  total_auc_raw = total_auc_raw,
                  auc_10h_15h_raw = auc_10h_15h_raw,
                  sd_10h_15h_raw = sd_10h_15h_raw,
                  prop_auc_10h_15h_raw = prop_auc_10h_15h_raw,
                  auc_7h_19h_raw = auc_7h_19h_raw,
                  sd_7h_19h_raw = sd_7h_19h_raw,
                  prop_auc_7h_19h_raw = prop_auc_7h_19h_raw,
                  auc_night_raw = auc_night_raw,
                  sim_ETobs_ETPenMont_raw = sim_ETobs_ETPenMont_raw)

  res_smth <- list(TR_smth = smth.trans,
                   Max_TR_smth = Max_TR_smth,
                   slope_6pt_bfr_maxTR_smth = slope_6pt_bfr_maxTR_smth,
                   slope_00_07_smth = slope_00_07_smth,
                   slope_07_maxTR_smth = slope_07_maxTR_smth,
                   slope_19h_23h45_smth = slope_19h_23h45_smth,
                   curve_maxTR_smth = curve_maxTR_smth,
                   total_auc_smth = total_auc_smth,
                   auc_10h_15h_smth = auc_10h_15h_smth,
                   sd_10h_15h_smth = sd_10h_15h_smth,
                   prop_auc_10h_15h_smth = prop_auc_10h_15h_smth,
                   auc_7h_19h_smth = auc_7h_19h_smth,
                   sd_7h_19h_smth = sd_7h_19h_smth,
                   prop_auc_7h_19h_smth = prop_auc_7h_19h_smth,
                   auc_night_smth = auc_night_smth,
                   sim_ETobs_ETPenMont_smth = sim_ETobs_ETPenMont_smth)

  results <- list(TR_raw = res_raw, TR_smth = res_smth)
  class(results) <- c('TRres', 'list')

  end.time <- Sys.time()

  print(paste0("Complete processing executed in: ", round((end.time-st.time), 2)))

  return(results)
  
}