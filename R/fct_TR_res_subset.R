#' TR_res_subset 
#'
#' @description A function to subset a TR_res object.
#'
#' @param TR_res object of class TRres obtained with \code{\link{TR_data_proc}}
#' @param start_day \code{numeric} values specifying the first day of the subseted
#' TR_res.
#' @param end_day \code{numeric} values specifying the last day of the subseted
#' TR_res.
#'
#' @return subseted TRres object
#' 
#' @examples
#' 
#' data("TR_res")
#' 
#' TR_res <- TR_res_subset(TR_res = TR_res, start_day = 6, end_day = 21)
#'
#' @export
#' 

TR_res_subset <- function(TR_res, start_day, end_day){
  
  if(end_day < start_day){
    
    stop("end_day is smaller than start_day. No time windows can be formed")
    
  }
  
  # identify the unique days in daily features matrix
  TR_feature <- TR_res$TR_smth$Max_TR_smth
  ts_date <- colnames(TR_feature)[6:ncol(TR_feature)]
  days_sel <- ts_date[start_day:end_day]
  
  # identify the unique days on TR_smth
  TS_smth <- TR_res$TR_smth$TR_smth
  ts <- ymd_hms(colnames(TS_smth)[6:ncol(TS_smth)], tz = "UTC")
  ts_date2 <- as.character(date(ts))
  
  # subset the complete times series
  d_sel_complete_TS <- which(ts_date2 %in% days_sel)
  TR_res$TR_smth$TR_smth <- TS_smth[, c(1:5, d_sel_complete_TS)]
  
  # modify the daily feature TS
  d_sel_daily_TS <- which(ts_date %in% days_sel) + 5
  for(i in 2:length(TR_res$TR_smth)){
    
    TR_res_i <- TR_res$TR_smth[[i]]
    TR_res$TR_smth[[i]] <- TR_res_i[, c(1:5, d_sel_daily_TS)]
    
  }
  
  # subset TR_res_raw if present
  if(!is.null(TR_res$TR_raw)){
    
    TR_res$TR_raw$TR_raw <- TR_res$TR_raw$TR_raw[, c(1:5, d_sel_complete_TS)]
    
    for(i in 2:length(TR_res$TR_raw)){
      
      TR_res_i <- TR_res$TR_raw[[i]]
      TR_res$TR_raw[[i]] <- TR_res_i[, c(1:5, d_sel_daily_TS)]
      
    }
    
  }
  
  # subset weather complete
  wth_complete <- TR_res$wth_complete
  ts <- date(ymd_hms(wth_complete$TS, tz = "UTC"))
  TR_res$wth_complete <- TR_res$wth_complete[ts %in% days_sel, ]
  
  # subset LAI missing
  LAI_miss <- TR_res$LAI_miss
  TR_res$LAI_miss <- LAI_miss[, colnames(LAI_miss) %in% days_sel]
  
  class(TR_res) <- c('TRres', 'list')
  
  return(TR_res)
  
}
