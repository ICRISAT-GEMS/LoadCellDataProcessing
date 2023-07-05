#' Boxplot_TR 
#'
#' @description Boxplot of the average trait value over the time series per genotype.
#' 
#' @param results object of class TRres obtained with \code{\link{TR_data_proc}}
#' @param trait Character string: one of "TRmax", "TR_AUC_total", or "TR_AUC_10h15h"
#' @param main Title of the graph.
#'
#' @return Boxplot
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Transpiration rate data processing obtained with
#' # TR_data_proc
#' data("TR_res")
#' 
#' p <- Boxplot_TR(results = TR_res, trait = "TRmax")
#' p
#' 
#' }
#' 
#' @export

Boxplot_TR <- function(results, trait = "TRmax", main = "TR feature Boxplot"){
  
  # get the data
  var_lk <- c("Max_TR_smth", "total_auc_smth", "auc_10h_15h_smth")
  var_id_lk <- c("Maximum transpiration rate",
                 "total area under the TR curve",
                 "Area under the curve (10-15h)")
  
  names(var_lk) <- names(var_id_lk) <- c("TRmax", "TR_AUC_total", "TR_AUC_10h15h")
  
  
  v_sel <- var_lk[trait]
  
  d <- results$TR_smth[v_sel][[1]]
  d <- data.frame(geno = d$Genotype,
                  m = rowMeans(x = d[, 6:ncol(d)], na.rm = TRUE))
  
  d <- d %>% group_by(geno) %>% summarise(m = mean(m, na.rm = TRUE))
  
  p <- ggplot(d, aes(y=m)) + geom_boxplot() + ylab(var_id_lk[trait]) +
    ggtitle(main)
  
  
  return(p)
  
}