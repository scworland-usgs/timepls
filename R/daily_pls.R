#' @title daily_pls
#' @description Performs pls regression on daily data for a given number of lags.
#' @param data Data frame that consists of columns named date, month, day, and the time series
#' to be used for pls regression. In version 0.0.1, the response data must be called "cfs", and the
#' explanatory time series must be called "p", "tmax", and "tmin".
#' @param lag Number of lags to be used for creating partial least squares components
#' @param ncomps Number of components for pls regression
#' @import gridExtra
#' @import plsdepot
#' @import ggplot2
#' @import assertive
#' @examples
#' \dontrun{
#' data <- data
#' daily_pls(data,lag=20,ncomps=2)
#' }
#' @export

daily_pls <- function(data,lag=365,ncomps=3) {

  if(!is.data.frame(data) || !all(c('date','day','month','cfs','p','tmax','tmin') %in% names(data))) {
    stop("data must be a data.frame with columns 'date', 'day', month','cfs','p','tmax', and 'tmin'")}

  #assert_is_date(data$date)
  assert_is_numeric(lag)
  assert_all_are_non_negative(lag)
  assert_is_numeric(ncomps)
  assert_all_are_non_negative(ncomps)

  mutate <- dplyr::mutate

  # prepare data
  lag <- lag
  X <- cbind(embed(data$p,lag+1),embed(data$tmax,lag+1),embed(data$tmin,lag+1))
  y <- data$cfs[(lag+1):nrow(data)]
  date <- data$date[(lag+1):nrow(data)]
  month <- data$month[(lag+1):nrow(data)]
  day <- data$day[(lag+1):nrow(data)]
  pls <- plsreg1(X, y, comps = ncomps)

  # plot daily residuals
  daily <- data.frame(date=date,
                      month=month,
                      day=day,
                      Q=y,
                      residuals=pls$resid)

  p1 <- ggplot(daily) +
    geom_line(aes(date,residuals)) +
    geom_hline(yintercept=0,linetype="dashed",color="red") +
    theme_bw()

  # plot median monthly residuals
  monthly <- daily %>%
    group_by(day,month) %>%
    summarize(residuals=median(residuals)) %>%
    ungroup() %>%
    arrange(month,day) %>%
    mutate(day=1:366,
           month=ordered(month.abb[month], month.abb))

  season.color = colorRampPalette(c("blue","green","red","orange","blue"))(12)

  p2 <- ggplot(monthly) + geom_line(aes(day,residuals),size=0.3) +
    geom_line(aes(day,residuals,color=month)) + theme_bw() +
    scale_color_manual(values=season.color) +
    guides(col = guide_legend(ncol = 12)) +
    geom_hline(yintercept=0, linetype="dashed") +
    labs(x="Julian day",y="median residuals") +
    theme(legend.position="bottom")

  grid.arrange(p1,p2,ncol=1,top="Residuals from PLS model")

}

