#' add_exp_des_col 
#'
#' @description Add extra columns with experimental design information
#' to a data.frame.
#' 
#' @param data experimental data with experimental unit defined by the
#' argument "data_unit".
#' 
#' @param d_exp_des experimental design information (e.g. row, column)
#' corresponding to experimental unit defined by the argument
#' "d_exp_unit".
#' 
#' @param data_unit character string specifying the name of the column
#' of "data" that contain the identifier of the experimental unit.
#' Default = "unit".
#' 
#' @param d_exp_unit character string specifying the name of the column
#' of "d_exp_des" that contain the identifier of the experimental unit.
#' Default = "new_unit".
#' 
#' @param col_add column names of the column from "d_exp_des" that need
#' to be added to data. Default = c("rowNum", "colNum").
#' 
#' @return Extended data.frame with experimental design information
#'
#' @export

add_exp_des_col <- function(data, d_exp_des, data_unit = "unit",
                            d_exp_unit = "new_unit",
                            col_add = c("rowNum", "colNum")){
  
  # select the columns and the unit in the exp des data
  unit_vec_ref <- unlist(data[, data_unit])
  unit_vec_des <- unlist(d_exp_des[, d_exp_unit])
  
  d_add_col <- unit_vec_ref
  
  for(i in 1:length(col_add)){
    
    c_i_lk <- unlist(d_exp_des[, col_add[i]])
    names(c_i_lk) <- unit_vec_des
    d_add_col <- data.frame(d_add_col, c_i_lk[unit_vec_ref])
    
  }
  
  d_add_col <- d_add_col[, -1]
  colnames(d_add_col) <- col_add
  
  df <- data.frame(data, d_add_col)
  
  return(df)
  
}