#' TR_VPD_reg 
#'
#' @description Calculate linear regression coefficient TR-VPD
#' 
#' @param TR_res object of class TRres obtained with \code{\link{TR_data_proc}}
#' 
#' @param do_plot \code{Logical} value specifying if the plot of the regression
#' should be saved. Default = FALSE.
#' 
#' @param plot_loc \code{Character} string specifying the path where the plot
#' should be saved.
#' 
#' @param plot_name Optional \code{character} specifying the name of the plot.
#' Default: TR_VPD_regression
#'
#' @return A \code{data.frame} with the Tr ~ VPD linear regression coefficients.
#'
#' @examples
#' 
#' data("TR_res")
#' 
#' TR_VPD_reg_coeff <- TR_VPD_reg(TR_res = TR_res, do_plot = FALSE)
#'
#' @export


TR_VPD_reg <- function(TR_res, do_plot = FALSE, plot_loc, plot_name = NULL){
  
  d <- TR_res$TR_smth$TR_smth
  d_meta <- d[9:nrow(d), 1:5]
  
  VPD_vect <- unlist(d[which(rownames(d) == 'VPD'), 6:ncol(d)])
  Tr_mat <- t(d[9:nrow(d), 6:ncol(d)])
  
  Tr_VPD_slope <- rep(NA, ncol(Tr_mat))
  
  if(do_plot){
    
    if(is.null(plot_name)){plot_file <- "TR_VPD_regression.pdf"
    } else {plot_file <- paste0(plot_name, ".pdf") }
    
    pdf(file = file.path(plot_loc, plot_file), width = 12, height = 8)
    
  }
  
  for(i in 1:length(Tr_VPD_slope)){
    
    d_i <- data.frame(VPD = VPD_vect, Tr = Tr_mat[, i])
    m_i <- tryCatch(lm(Tr~VPD, data = d_i), error = function(x) NULL)
    if(!is.null(m_i)){
      
      Tr_VPD_slope[i] <- coefficients(m_i)[2]
      
      if(do_plot){
        plot(x = d_i$VPD, y = d_i$Tr, xlab = "VPD", ylab = "Transpiration",
             main = paste("Sector", d_meta[i, 1]))
        abline(m_i, col = "red")
      }
      
    }
    
  }
  
  if(do_plot){ dev.off() }
  
  res <- data.frame(d_meta, Tr_VPD_slope)
  
  return(res)
  
}