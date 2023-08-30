#' TR_res_get_traits
#'
#' @description Function to extract traits from a TRres object
#' 
#' @param TR_res object of class TRres obtained with \code{\link{TR_data_proc}}
#'
#' @return A \code{data.frame} with the following traits extracted for each sectors:
#' 
#'  \item{max_TR_av}{Average maximum daily transpiration over the selected period}
#'  \item{tot_AUC_sum}{Cumulated area under the transpiration curve over the selected period
#'  (Total amount transpired water)}
#'  \item{tot_AUC_av}{Average daily transpiration over the selected period}
#'  \item{AUC_10h15h_sum}{Cumulated area under the transpiration curve between 10h00 and 15h00
#'  over the selected period (Total amount transpired water between 10h00 and 15h00)}
#'  \item{AUC_10h15h_av}{Average daily transpiration between 10h00 and 15h00 over the selected period}
#'  \item{slope_07_maxTR_av}{Average daily transpiration ~ time slope between 07h00 and the time of max transpiration.}
#'  \item{slope_TR_VPD}{Linear regression coefficient Transpiration ~ VPD}
#'  
#' @examples
#' 
#' data("TR_res")
#' 
#' TR_traits <- TR_res_get_traits(TR_res = TR_res)
#'
#' @export

TR_res_get_traits <- function(TR_res){
  
  # meta information
  d_meta <- TR_res$TR_smth$Max_TR_smth[, 1:5]
  
  # maxTr
  d <- TR_res$TR_smth$Max_TR_smth[, 6:ncol(TR_res$TR_smth$Max_TR_smth)]
  max_TR_av <- rowMeans(d, na.rm = TRUE)
  
  # Total AUC
  d <- TR_res$TR_smth$total_auc_smth[, 6:ncol(TR_res$TR_smth$total_auc_smth)]
  tot_AUC_sum <- rowSums(d, na.rm = TRUE)
  tot_AUC_av <- rowMeans(d, na.rm = TRUE)
  
  # Total AUC 10h - 15h
  d <- TR_res$TR_smth$auc_10h_15h_smth[, 6:ncol(TR_res$TR_smth$auc_10h_15h_smth)]
  AUC_10h15h_sum <- rowSums(d, na.rm = TRUE)
  AUC_10h15h_av <- rowMeans(d, na.rm = TRUE)
  
  # slope between 07h and maxTr time
  d <- TR_res$TR_smth$slope_07_maxTR_smth[, 6:ncol(TR_res$TR_smth$slope_07_maxTR_smth)]
  slope_07_maxTR_av <- rowMeans(d, na.rm = TRUE)
  
  # slope Tr ~ VPD
  TR_VPD_slope <- TR_VPD_reg(TR_res = TR_res, do_plot = FALSE)
  d_meta2 <- TR_VPD_slope[, 1:5]
  
  if(!identical(d_meta, d_meta2)){
    
    stop("non similar meta data in the TR results")
    
  }
  
  res <- data.frame(d_meta, max_TR_av,
                    tot_AUC_sum, tot_AUC_av,
                    AUC_10h15h_sum, AUC_10h15h_av,
                    slope_07_maxTR_av,
                    slope_TR_VPD = TR_VPD_slope$Tr_VPD_slope)
  
  return(res)
  
}