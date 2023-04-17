#' check_sensor_data 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

check_sensor_data <- function(d){
  
  req_var <- c('sector', 'sensor')
  
  if(!identical(colnames(d), req_var)){
    
    form_req <- paste(req_var, collapse = ', ')
    
    mess_err <- 'The column names of sensor_data are not strictly equivalent to the required format:'
    
    stop(paste(mess_err, form_req))
    
  }
  
  return(d)
  
}

