#' check_wth_data 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

check_wth_data <- function(d){
  
  req_var <- c('sensor', 'variable', 'timestamp', 'value')
  
  if(!identical(colnames(d), req_var)){
    
    form_req <- paste(req_var, collapse = ', ')
    
    mess_err <- 'The column names of wth_data are not strictly equivalent to the required format:'
    
    stop(paste(mess_err, form_req))
    
  }
  
  # check the content of variable
  
  var_id <- c("Temperature (Â°C)", "Relative humidity (%)",
              "Windspeed average (m/s)", "Windspeed max (m/s)",        
              "Solar radiation (W/(s*mÂ²))", "Precipitation (mm)",
              "Wind direction (Â°)")
  
  if(any(!(d$variable[1:100] %in% var_id))){
    
    mess_err <- 'some elements of variable in weather file do not correspond to the required items:'
    req_item <- paste(var_id, collapse = ", ")
    
    stop(paste(mess_err, req_item))
    
  }
  
  return(d)
  
}

