#' wth_data_proc 
#'
#' @description processing of the weather data
#' 
#' @param wth_data \code{data.frame} representing the weather data information
#'containing the following columns (variables):
#'
#' \describe{
#'  \item{\code{sensor}}{a character vector describing the sensor information}
#'  \item{\code{variable}}{a character vector indicating the type of weather information. Must be one of: Temperature (°C), Relative humidity (\%)), Windspeed average (m/s), Windspeed max (m/s), Solar radiation (W/(s*m²)), Precipitation (mm), or Wind direction (°).}
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
#' @param skew_test \code{Logical value} specifying if skewness test
#' should be used to filter weather data. Default = FALSE.
#'
#' @return processed weather data
#'
#' @export

wth_data_proc <- function(wth_data, sensor_data, skew_test = FALSE){
  
  # check function arguments format
  clm.df <- check_wth_data(wth_data)
  rm(wth_data)
  sensor.unit.df <- check_sensor_data(sensor_data)
  rm(sensor_data)
  
  # get the starting and end date of the experiment (range)
  E_range <- exp_range(clm.df$timestamp)
  Date1 <- E_range$date_min
  Date2 <- E_range$date_max
  
  clm.df.mapped <- clm.df[clm.df$sensor %in% sensor.unit.df$sensor, ]
  
  # Extract weather variables individually; Assign numbers appropriately #
  temperature_DF <- extractWthrVar(y = clm.df.mapped, sel_wth_var = "Temperature (°C)",
                                   skew_test = skew_test)
  
  temperature_DF$ts <- temperature_DF$ts + 5.5*60*60
  
  relHUM_DF <- extractWthrVar(y = clm.df.mapped, sel_wth_var = "Relative humidity (%)",
                              skew_test = skew_test)
  relHUM_DF$ts <- relHUM_DF$ts + 5.5*60*60
  
  wind_DF <- extractWthrVar(y = clm.df.mapped, sel_wth_var = "Windspeed average (m/s)",
                            skew_test = skew_test)
  wind_DF$ts <- wind_DF$ts + 5.5*60*60
  
  solarRad_DF <- extractWthrVar(y = clm.df.mapped, sel_wth_var = "Solar radiation (W/(s*m²))",
                                skew_test = skew_test)
  solarRad_DF$ts <- solarRad_DF$ts + 5.5*60*60
  
  Date1<-ymd_hms(Date1)  ## Make sure it's a Complete Cycle
  Date2<-ymd_hms(Date2)
  
  dates <- seq(Date1, Date2, by="min")
  
  temperature_DF <- temperature_DF[, c("ts", "value")]
  colnames(temperature_DF)[2] <- "Temp"
  relHUM_DF <- relHUM_DF[, c("ts", "value")]
  colnames(relHUM_DF)[2] <- "RH"
  solarRad_DF <- solarRad_DF[, c("ts", "value")]
  colnames(solarRad_DF)[2] <- "SR"
  wind_DF <- wind_DF[, c("ts", "value")]
  colnames(wind_DF)[2] <- "WS"
  
  # create a reference dataframe to gather the weather data
  weather_DF <- data.frame(ts = dates)
  weather_DF <- merge(x = weather_DF, y = temperature_DF, by = "ts", all.x = TRUE)
  weather_DF <- merge(x = weather_DF, y = relHUM_DF, by = "ts", all.x = TRUE)
  weather_DF <- merge(x = weather_DF, y = solarRad_DF, by = "ts", all.x = TRUE)
  weather_DF <- merge(x = weather_DF, y = wind_DF, by = "ts", all.x = TRUE)
  weather_DF$VPD <- NA
  colnames(weather_DF)[1] <- "TS"
  weather_DF <- weather_DF[, c("TS", "Temp", "RH", "VPD", "SR", "WS")]
  
  # save a vector with the time when we have complete weather information
  wth_complete <- weather_DF %>% select(-VPD)
  wth_complete <- data.frame(TS = wth_complete$TS, comp_wth_data = complete.cases(wth_complete))
  wth_date <- lubridate::date(wth_complete$TS)
  wth_time <- strftime(wth_complete$TS, format="%H:%M:%S", tz="UTC")
  wth_complete$TS <- paste(wth_date, wth_time)
  
  # Preprocess each weather variable except VPD #
  weather_DF[ , 2] <- prepcsWthr(x = weather_DF, y = 2) # temperature
  weather_DF[ , 3] <- prepcsWthr(x = weather_DF, y = 3) # relative humidity
  weather_DF[ , 5] <- prepcsWthr(x = weather_DF, y = 5) # solar radiation
  weather_DF[ , 6] <- prepcsWthr(x = weather_DF, y = 6) # wind speed
  
  #### VPD calculation
  
  # Compute VPD and insert into the weather DF #
  SVP <- 610.7*(10^(7.5*weather_DF[ ,2]/(237.3+weather_DF[ ,2])))
  VPD <- ((1 - (weather_DF[ ,3]/100))*SVP)/1000
  weather_DF[ , 4] <- VPD
  
  wthr.DFxts.TS <- xts(weather_DF,
                       order.by = as.POSIXct(weather_DF$TS, format="%Y-%m-%d %H:%M"))
  
  wth_data <- highfrequency::aggregateTS(ts = wthr.DFxts.TS, alignBy ="minutes",
                                         alignPeriod =15, dropna=TRUE)
  
  TS <- wth_data$TS
  t_dmy <- lubridate::date(TS)
  t_hms <- strftime(TS, format="%H:%M:%S", tz="UTC")
  day <- hms(t_hms) >= hms("07:00:00") & hms(t_hms) < hms("19:00:00")
  
  TS <- as.POSIXlt(TS, format = "%Y-%m-%d %H:%M:%S")
  t_dmy <- strptime(TS, "%Y-%m-%d")
  
  d_time <- data.frame(timePoint = TS, dmy = t_dmy, hms = t_hms, day = day)
  d_EC <- as.data.frame(wth_data[, 2:ncol(wth_data)])
  d_EC <- apply(X = d_EC, MARGIN = 2, as.numeric)
  
  wth_data <- data.frame(d_time, d_EC)
  
  return(wth_data)
  
}
