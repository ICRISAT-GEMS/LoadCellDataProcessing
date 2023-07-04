#' TR_res_processing 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

TR_res_processing <- function(allFeatures, unq.dts, d_trans, raw = TRUE){
  
  # create some empty space to store the TS results
  feature_list <- c('TR_raw', 'Max_TR_raw', 'slope_6pt_bfr_maxTR_raw',
                    'slope_07_maxTR_raw', 'slope_00_07_raw', 'slope_19h_23h45_raw',
                    'curve_maxTR_raw', 'total_auc_raw', 'auc_10h_15h_raw',
                    'sd_10h_15h_raw', 'prop_auc_10h_15h_raw', 'auc_7h_19h_raw',
                    'sd_7h_19h_raw', 'prop_auc_7h_19h_raw', 'auc_night_raw',
                    'sim_ETobs_ETPenMont_raw')
  
  if(!raw){
    feature_list <- gsub(pattern = '_raw', replacement = '_smth', x = feature_list)
    
  }
  
  n_features <- length(feature_list) - 1
  
  res_list <- vector(mode = 'list', length = n_features + 1)
  res_list[[1]] <- d_trans
  
  des_info <- d_trans[9:nrow(d_trans), 1:5]
  
  for(i in 1:n_features){
    
    df_res <- t(data.frame(lapply(allFeatures, function(x) x[, i])))
    rownames(df_res) <- NULL
    df_res <- data.frame( df_res)
    colnames(df_res) <- unq.dts
    df_res <- as.data.frame(cbind(des_info, df_res))
    
    res_list[[i+1]] <- df_res
    
  }
  
  names(res_list) <- feature_list
  
  return(res_list)
  
}
