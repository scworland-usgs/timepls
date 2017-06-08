#' @title combine_sites
#' @description Combines climate data from sites from folder
#' @param N Integer. Number of sites to combine from folder
#' @param file.names Array >=1. Character strings of file names in folder
#' @param folder Folder than contains files
#' @param site.coords Data frame. Contains unique ID for sites and coordinates
#' @param random Logical. Randomly select N files in folder?
#' @import dplyr
#' @import lubridate
#' @examples
#' \dontrun{
#' #set.seed(876)
#' #data <- combine_sites(N=1,file.names,folder='data/site_data',site.coords,random=T)
#' }
#' @export


combine_sites <- function(N,file.names,folder,site.coords,random=FALSE) {

  require(dplyr)
  require(lubridate)

  mutate = dplyr::mutate
  select = dplyr::select

  if (exists("dataset")){rm(dataset)}

  if (isTRUE(random)){

    sites=base::sample(length(file.names),N)

  }else{

    sites = 1:N

  }

  for(i in sites){

    if (!exists("dataset")){

      file <- file.path(folder,file.names[i])
      load(file)

      data.list <- list(data.frame(date=obs.date,obs),
                        data.frame(date=prcp.date,prcp),
                        data.frame(date=tmax.date,tmax),
                        data.frame(date=tmin.date,tmin))

      dataset <- plyr::join_all(data.list, by="date", type="inner") %>%
        na.omit() %>%
        mutate(site = site,
               da = drainage_area,
               month = month(date),
               day = day(date),
               year = year(date)) %>%
        group_by(date) %>%
        mutate(tmean = (tmax + tmin)/2) %>%
        ungroup()
    }

    if (exists("dataset")){
      file <- file.path(folder,file.names[i])
      load(file)

      data.list <- list(data.frame(date=obs.date,obs),
                        data.frame(date=prcp.date,prcp),
                        data.frame(date=tmax.date,tmax),
                        data.frame(date=tmin.date,tmin))

      data <- plyr::join_all(data.list, by="date", type="inner") %>%
        na.omit() %>%
        mutate(site = site,
               da = drainage_area,
               month = month(date),
               day = day(date),
               year = year(date)) %>%
        group_by(date) %>%
        mutate(tmean = (tmax + tmin)/2) %>%
        ungroup()

      dataset <- rbind(dataset, data)
    }
  }

  dataset <- left_join(dataset,site.coords,by="site") %>%
    select(site,lon,lat,date,month,day,year,da,cfs=obs,p=prcp,tmin,tmax,tmean) %>%
    arrange(site,date) %>%
    dplyr::distinct() #%>%
    #filter(paste0(month,day) != 229)

  return(dataset)
}
