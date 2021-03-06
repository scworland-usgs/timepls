
################################################################################
#' An S4 class to return the results of time_pls
#' @describeIn time_pls
#' @importFrom methods new
#' @slot observed observed values
#' @slot estimated estimated values
#' @slot residuals residuals from fit model
#' @slot dates dates of time series
#' @export

timepls_fit <- setClass("timepls_fit",
                    slots = c(observed="numeric",
                              estimated="numeric",
                              residuals = "numeric",
                              dates = "Date")
)


################################################################################
#' Plots timepls_fit S4 object's date and residuals
#' @describeIn timepls_fit
#' @aliases plot,timepls_fit-method
#' @import ggplot2
#' @importFrom magrittr %>%
#' @param x time_pls fit object from time_pls function.
#' @param y not used
#' @export

setMethod("plot", "timepls_fit",
          function(x,y,...) {

            d <- data.frame(date = x@dates,
                            residuals = x@residuals)

            ggplot(d) +
              geom_line(aes(date,residuals)) +
              geom_hline(yintercept=0,linetype="dashed",color="red") +
              theme_bw()
          }
)

