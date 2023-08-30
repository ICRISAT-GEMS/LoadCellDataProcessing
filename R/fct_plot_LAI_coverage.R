#' plot_LAI_coverage 
#'
#' @description A function to plot the coverage of LAI plant eye data
#' obtained with \code{\link{TR_data_proc}}.
#'
#' @param results object of class TRres obtained with \code{\link{TR_data_proc}}
#'
#' @return matrix plot
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Transpiration rate data processing obtained with
#' # TR_data_proc
#' data("TR_res")
#' 
#' p <- plot_LAI_coverage(results = TR_res)
#' p
#' 
#' }
#' 
#' @export

plot_LAI_coverage <- function(results){
  
  d <- melt(results$LAI_miss)
  colnames(d) <- c("sector", "day", "z")
  d$day <- rep(1:ncol(results$LAI_miss), each = nrow(results$LAI_miss))
  d$y <- rep(1:nrow(results$LAI_miss), ncol(results$LAI_miss))
  days <- unique(d$day)
  
  p <- ggplot(d, aes(x = day, y = y)) + 
    geom_raster(aes(fill=z)) + 
    scale_fill_gradient(low="white", high="green") +
    labs(x="days", y="sectors", title="LAI coverage") +
    geom_vline(xintercept = days, linetype="dotted", color = "black", linewidth = 0.5)
  
  return(p)
  
}
