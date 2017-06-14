#' @title plot_cor
#' @description Plots rolling correlation for timepls_fit S4 object's date and residuals
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom roll roll_cor
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom lubridate month
#' @param fit time_pls fit object from time_pls function
#' @param window window for correlations
#' @param smooth smoothing window for correlation plot
#' @examples
#' \dontrun{
#' d <- climate_data
#' dates <- d$date
#' y <- d$cfs
#' X <- cbind(d$p,d$tmin,d$tmax)
#' fit <- time_pls(y,X,dates,lag=30,ncomps=10)
#' plot_cor(fit,window=60,smooth=90)
#' }
#' @export

plot_cor <- function(fit,window,smooth) {

  # extract data from timepls_fit object
  obs <- fit@observed
  est <- fit@estimated
  mat <- matrix(c(obs,est),ncol=2)

  # rolling correlation function
  r <- roll_cor(mat,window)[1,2,]

  # smooth correlation
  ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
  rsmooth <- ma(r,smooth)

  # prepare dataframe for plotting
  dplot <- data.frame(correlation=r,smooth=rsmooth,time=fit@dates) %>%
    mutate(month=ordered(month.abb[month(time)], month.abb)) %>%
    na.omit() %>%
    gather(key,value,-time,-month) %>%
    mutate(key = ifelse(key=="smooth","smoothed correlation",key))

  # custom month-season color scheme
  season.color = colorRampPalette(c("blue","green","red","orange","blue"))(12)

  # plot correlations
  ggplot(dplot) +
    geom_line(aes(time,value,color=month, group=1)) +
    scale_color_manual(values=season.color) +
    scale_x_date(date_breaks="3 years",date_labels = "%Y") +
    geom_hline(yintercept=0,linetype="dashed") +
    labs(y="correlation") +
    facet_wrap(~key,ncol=1) +
    #ggtitle("Rolling correlation with 50 step window") +
    theme_bw()
}
