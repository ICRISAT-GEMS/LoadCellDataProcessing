#' check_timestamp_format 
#'
#' @description Check if all elements from a vector of timestamp
#' match the %Y-%m-%d %H:%M:%S format.
#' 
#' @param x character vector of timestamp (time point) values  
#'
#' @return TRUE/FALSE value given the result of the text
#'
#' @export

check_timestamp_format <- function(x) {
  
  # Check if all elements of x are in the %Y-%m-%d %H:%M:%S format
  test <- all(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", x))
  
  if(!test){
    stop("The date in the vector do not all match the %Y-%m-%d %H:%M:%S format.")
  }
  
  return(test)
  
}
