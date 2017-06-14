
#' @title time_pls
#' @description Performs partial lease squares regression on time series data for a given number of lags.
#' @param y Vector of variable to predict using pls
#' @param X Matrix of covariates for pls
#' @param lag Number of lags to be used for creating pls components
#' @param ncomps Number of components for pls regression
#' @import plsdepot
#' @import assertive
#' @examples
#' \dontrun{
#' d <- climate_data
#' dates <- d$date
#' y <- d$cfs
#' X <- cbind(d$p,d$tmin,d$tmax)
#' fit <- time_pls(y,X,dates,lag=30,ncomps=3)
#' }
#' @export

time_pls <- function(y,X,dates,lag=30,ncomps=3) {

  mutate <- dplyr::mutate

  # prepare data
  N <- length(y) # number of time steps
  lag <- lag # number of lags
  y <- y[(lag+1):N] # prepare response data
  dates <- dates[(lag+1):N] # dates if provided

  # function to embed covariate time series
  ts_embed <- list()
  for(i in 1:ncol(X)) {
    ts_embed[[i]] <- embed(X[,i],lag+1)
  }

  # combine time series from list
  X <- do.call(cbind,ts_embed)

  # run partial least squares regression
  pls_model <- plsdepot::plsreg1(X, y, comps = ncomps)

  fit <- timepls_fit(observed=y,
                     estimated=as.numeric(pls_model$y.pred),
                     residuals=as.numeric(pls_model$resid),
                     dates=dates)

  return(fit)

  # month <- data$month[(lag+1):nrow(data)]
  # day <- data$day[(lag+1):nrow(data)]
  # pls <- plsreg1(X, y, comps = ncomps)

}
