
# data <- data
# dates <- data$date
# y <- data$cfs
# X <- cbind(data$p,data$tmin,data$tmax)

time_pls <- function(y,X,dates,lag=365,ncomps=3) {

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
  pls_model <- plsreg1(X, y, comps = ncomps)
  return(pls_model)

  # month <- data$month[(lag+1):nrow(data)]
  # day <- data$day[(lag+1):nrow(data)]
  # pls <- plsreg1(X, y, comps = ncomps)

}
