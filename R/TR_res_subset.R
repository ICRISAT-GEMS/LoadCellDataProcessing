#################
# TR_res_subset #
#################

#' Subsetting of transpiration rate processed data
#' 
#' Subsetting of transpiration rate processed data
#' 
#'
#' @param TR_res \code{List} of class \code{TRres} obtained with function \code{\link{TR_data_proc}}
#' 
#' @param start_date Default = NULL.
#' 
#' @param end_date Default = NULL.
#' 
#' @return Return:
#' 
#' TR_res with only the results for the selected time range. 
#'
#' @author ICRISAT-GEMS
#'
#'
#' @examples
#'
#' # Later
#'
#'
#' @export
#'

# library(LoadCellDataProcessing)
# res_path="C:\\Users\\dsubhash\\Documents\\pipeline_blobs\\Loadcells\\output\\Exp45.cp"
# load(file = file.path(res_path, 'TR_res.RData'))
# start_date = as.Date("2021-02-15")
# end_date = as.Date("2021-02-17")

TR_res_subset <- function(TR_res, start_date = NULL, end_date = NULL){
 
  if(!inherits(x = TR_res, what = 'TRres')){
    
    stop('The argument TR_res is not of class TRres. Please use function TR_data_proc.')
    
  }
  
  ###Transpiration Rate for Raw and Smooth Function
  TR_raw_s <- TR_res$TR_raw$TR_raw
  TR_smth_s <- TR_res$TR_smth$TR_smth
  col_n <- colnames(TR_raw_s)
  col_date <- as_date(col_n)
  is_date <- !is.na(col_date)
  
  col_date <- col_date[!is.na(col_date)]
  
  # keep meta data
  
  meta <- TR_raw_s[, !is_date]
  
  # subset the part with date
  TR_raw_s_date <- TR_raw_s[, is_date]
  TR_smth_s_date <- TR_smth_s[, is_date]
  
  
  # select the specified dates
  
  sel_id <- (col_date > start_date) & (col_date < end_date)
  
  TR_raw_s_date <- TR_raw_s_date[, sel_id]
  TR_smth_s_date <- TR_smth_s_date[, sel_id]
  
  TR_res$TR_raw$TR_raw <- data.frame(meta, TR_raw_s_date)
  TR_res$TR_smth$TR_raw <- data.frame(meta, TR_smth_s_date)
  
 
  #####
  TR_slope_Max<- TR_res$TR_raw$slope_07_maxTR_raw
  # keep meta data
  col_n_param <- colnames(TR_slope_Max)
  col_date_param <- as_date(col_n_param)
  is_date_param <- !is.na(col_date_param)
  
  # select the specified dates
  
  sel_id_param <- (col_date_param >= start_date) & (col_date_param <= end_date)
  sel_id_param[is.na(sel_id_param)] <- TRUE
  
  # subsetting of TR_raw parameter
  
  for(i in 2:length(TR_res$TR_raw)){
    
    p_mat_i <- TR_res$TR_raw[[i]]
    p_mat_i <- p_mat_i[, sel_id_param]
    TR_res$TR_raw[[i]] <- p_mat_i
   }
     
  # subsetting of TR_smooth parameter
  
  for(i in 2:length(TR_res$TR_smth)){
    
    p_mat_i <- TR_res$TR_smth[[i]]
    p_mat_i <- p_mat_i[, sel_id_param]
    TR_res$TR_smth[[i]] <- p_mat_i
  }
return(TR_res)
   
}