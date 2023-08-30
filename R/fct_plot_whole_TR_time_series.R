#' plot_whole_TR_time_series 
#'
#' @description A function to plot the complete transpiration time series
#' obtained with \code{\link{TR_data_proc}} for selected sectors.
#'
#' @param results object of class TRres obtained with \code{\link{TR_data_proc}}
#' @param sector_sel Vector of numeric values specifying which sector need to be ploted.
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
#' p <- plot_whole_TR_time_series(results = TR_res, sector_sel = c(2, 8))
#' p
#' 
#' }
#' 
#' @export

plot_whole_TR_time_series <- function(results, sector_sel,
                                      main = "Transpiration time series plot"){
  
  d_TR <- results$TR_smth$TR_smth
  d_TR <- d_TR[9:nrow(d_TR), ]
  geno_id <- d_TR$G..Alias[sector_sel]
  d_TR <- d_TR[sector_sel, 6:ncol(d_TR)]
  
  # find the limit of the day
  ts <- ymd_hms(colnames(d_TR), tz = "UTC")
  pos_0h00 <- which(hour(ts) == 0 & minute(ts) == 0)
  
  n_sect <- length(sector_sel)
  n_dp <- ncol(d_TR)
  
  d_wth <- results$wth_complete
  d <- data.frame(geno = rep(geno_id, each = n_dp), TR = c(t(d_TR)),
                  ts = rep(1:n_dp, n_sect))
  max_TR <- max(d$TR, na.rm = TRUE)
  max_TR <- max_TR + 0.15*max_TR
  
  wth_vec <- rep(NA, nrow(d_wth))
  wth_vec[d_wth$comp_wth_data] <- max_TR
  
  d <- rbind(d, data.frame(geno = 'weather', TR = wth_vec, ts = 1:n_dp))
  
  p <- ggplot(data = d, aes(x = ts, y = TR, group = geno, col = geno)) + geom_line() +
    ylab("transpiration") + ggtitle(main) +
    geom_vline(xintercept = pos_0h00, linetype="dotted", color = "black", linewidth = 0.3)
  
  return(p)
  
}