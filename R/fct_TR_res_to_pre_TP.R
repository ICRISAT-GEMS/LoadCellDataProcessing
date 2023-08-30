#' TR_res_to_pre_TP 
#'
#' @description Function to transform the results from a TR_res object
#' into a pre TP object format just before the addition of the
#' experimental design information.
#' 
#' @param TR_res object of class TRres obtained with \code{\link{TR_data_proc}}
#'
#' @return The return value, if any, from executing the function.
#'
#' @export

TR_res_to_pre_TP <- function(TR_res){
  
  tr_mat <- TR_res[[1]][2:length(TR_res[[1]])]
  tr_nm <- names(tr_mat)
  
  # for now we assume the exact structure: "unit", "old_unit", "Genotype", "G..Alias",
  # "Replicates" dates 1, 2, ... Need to be changed if different structure.
  sel_col <- c(1, 3, 6:ncol(tr_mat[[1]]))
  n_days <- length(6:ncol(tr_mat[[1]]))
  time_vec <- colnames(tr_mat[[1]])[6:ncol(tr_mat[[1]])]
  n_unit <- nrow(tr_mat[[1]])
  
  tr_mat_1 <- tr_mat[[1]][, sel_col]
  meta_mat <- data.frame(unit = rep(tr_mat_1$unit, n_days),
                         genotype = rep(tr_mat_1$Genotype, n_days),
                         timestamp = rep(time_vec, each = n_unit))
  
  tr_val_mat <- c()
  
  for(i in 1:length(tr_mat)){
    
    tr_i <- as.matrix(tr_mat[[i]][, 6:ncol(tr_mat[[1]])])
    tr_val_mat <- cbind(tr_val_mat, c(tr_i))
    
  }
  
  df <- data.frame(meta_mat, tr_val_mat)
  colnames(df)[4:ncol(df)] <- tr_nm
  
  return(df)
  
}
