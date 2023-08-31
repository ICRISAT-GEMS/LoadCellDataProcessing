#' check_pe_data 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

check_pe_data <- function(d){
  
  # Check the columns names
  
  req_var <- c('sector', 'genotype', 'replicate', 'timestamp', 'leaf_area')
  
  if(!identical(colnames(d), req_var)){
    
    form_req <- paste(req_var, collapse = ', ')
    
    mess_err <- 'The column names of pe_data are not strictly equivalent to the required format:'
    
    stop(paste(mess_err, form_req))
    
  }
  
  # check the format of the timestamp values
  check_timestamp_format(d$timestamp)
  
  return(d)
  
}