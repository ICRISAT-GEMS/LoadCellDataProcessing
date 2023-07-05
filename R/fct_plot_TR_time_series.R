#' plot_TR_time_series 
#'
#' @description A function to plot the daily time series of selected trait from
#' transpiration rate (TR) profile obtained with \code{\link{TR_data_proc}}.
#'
#' @param results object of class TRres obtained with \code{\link{TR_data_proc}}
#' @param trait Character string: one of "TRmax", "TR_AUC_total", or "TR_AUC_10h15h"
#' @param n_sector By default all genotypes are plotted, otherwise a sample of n_sector is randomly selected to be ploted.
#' @param color If color = TRUE, all genotypes get a different colour.
#' @param main Title of the graph.
#'
#' @return time series plot
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Transpiration rate data processing obtained with
#' # TR_data_proc
#' data("TR_res")
#' 
#' p <- plot_TR_time_series(results = TR_res, trait = "TRmax",
#'                          n_sector = NULL, color = FALSE)
#' p
#' 
#' }
#' 
#' @export

plot_TR_time_series <- function(results, trait = "TRmax", n_sector = NULL,
                                color = FALSE, main = "TR feature TS plot"){
  
  # get the data
  var_lk <- c("Max_TR_smth", "total_auc_smth", "auc_10h_15h_smth")
  var_id_lk <- c("Maximum transpiration rate",
                 "total area under the TR curve",
                 "Area under the curve (10-15h)")
  
  names(var_lk) <- names(var_id_lk) <- c("TRmax", "TR_AUC_total", "TR_AUC_10h15h")
  
  v_sel <- var_lk[trait]
  
  d <- results$TR_smth[v_sel][[1]]
  n_info <- nrow(d)
  
  if(!is.null(n_sector)){
    
    d <- d[sample(x = 1:n_info, size = n_sector), ]
    
  }
  
  # for the data.frame for plotting
  sect_id <- paste0("sect_", 1:nrow(d))
  n_sect <- length(sect_id)
  
  days <- 1:(ncol(d) - 5)
  n_days <- length(days)
  
  tr_val <- d[, 6:ncol(d)]
  tr_val <- c(as.matrix(t(tr_val)))
  
  d_plot <- data.frame(tr = tr_val, sector = rep(sect_id, each = n_days),
                       day = rep(days, n_sect))
    
    p <- ggplot(data = d_plot, aes(x = day, y = tr, group = sector)) + geom_line() +
    ylab(var_id_lk[trait]) + ggtitle(main) +
    {if(!color)theme(legend.position = 'none')}
    
  
  return(p)
  
}
