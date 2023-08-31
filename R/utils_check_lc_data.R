#' check_lc_data 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

check_lc_data <- function(d){
  
  # Check the columns names
  
  req_var <- c('sector', 'genotype', 'g_alias', 'treatment', 'timestamp', 'Mass..g')
  
  if(!identical(colnames(d), req_var)){
    
    form_req <- paste(req_var, collapse = ', ')
    
    mess_err <- 'The column names of lc_data are not strictly equivalent to the required format:'
    
    stop(paste(mess_err, form_req))
    
  }
  
  # check the format of the timestamp values
  check_timestamp_format(d$timestamp)
  
  colnames(d)[colnames(d) == 'sector'] <- 'unit'
  
  return(d)
  
}