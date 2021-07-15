##################
# EZTr functions #
##################

# time_split
############

time_split <- function(time){
  
  time <- strsplit(x = time, split = ' ')
  date <- unlist(lapply(time, function(x) x[1]))
  hour <- unlist(lapply(time, function(x) x[2]))
  
  list(date = date, hour = hour)
  
}

######

# time_format
############

time_format <- function(time){
  
  time <- strsplit(x = time, split = ' ')
  date <- unlist(lapply(time, function(x) x[1]))
  day <- substr(date, 9, 10)
  month <- substr(date, 6, 7)
  year <- substr(date, 3, 4)
  
  hour <- unlist(lapply(time, function(x) x[2]))
  hour <- substr(hour, 1, 5)
  
  new_date <- rep(NA, length(day))
  
  for(i in 1:length(new_date)){
    
    new_date[i] <- paste0(day[i], '-', month[i], '-', year[i],
                          ' ', hour[i])
    
  }
  
  return(new_date)
  
}

######

# exp_range
###########

exp_range <- function(time){
  
  date_num <- as.POSIXct(x = time)
  date_num <- as.numeric(date_num)
  
  date_min <- time[which.min(date_num)]
  date_max <- time[which.max(date_num)]
  
  # round the date
  date_min <- paste0(substr(date_min, 1, 17) , '00')
  date_max <- paste0(substr(date_max, 1, 17) , '00')
  
  # date max will be one day less than max at the same time of day min one minute
  # less
  
  date_min <- as.POSIXlt(date_min)
  date_max <- as.POSIXlt(date_max)
  
  d_diff <- date_max - date_min
  d_diff <- floor(as.numeric(d_diff)) - 1
  
  date_max <- date_min
  date_max$mday <- date_max$mday + d_diff
  
  date_max$min <- date_max$min - 1
  
  list(date_min = as.character(date_min), date_max = as.character(date_max))
  
}

########