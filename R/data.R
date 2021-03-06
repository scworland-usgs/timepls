
#' Daily climate and streamflow data for a site in IL
#'
#' A dataset containing daily PRISM climate data and U.S. Geological survey (USGS) streamflow data for La Moine River at Colmar, IL.
#'
#' @format A data frame with 11688 rows and 13 variables:
#' \describe{
#'   \item{site}{USGS site number}
#'   \item{lon}{longitude for center of basin}
#'   \item{lat}{latitude for center of basin}
#'   \item{date}{date of record}
#'   \item{month}{month of record}
#'   \item{day}{Julian day of record}
#'   \item{year}{year of record}
#'   \item{da}{drainage area of basin in km^2}
#'   \item{cfs}{streamflow in cubic feet per second (cfs)}
#'   \item{p}{daily precipitation in mm}
#'   \item{tmin}{daily minimum temperature in celsius}
#'   \item{tmax}{daily maximum temperature in celsius}
#'   \item{tmean}{daily mean temperature in celsius}
#' }
#' @source \url{https://waterdata.usgs.gov/nwis/uv?site_no=05584500}
#'
"climate_data"
