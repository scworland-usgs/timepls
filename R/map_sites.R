

map_sites <- function(data,all.sites) {
  
  d <- data %>%
    distinct(site,lon,lat)
  
  states <- map_data("state")
  
  ggplot() + coord_fixed(1.3) + theme_void() + xlab("") + ylab("") +
    geom_polygon(data=states, aes(long,lat,group=group),color = "black", fill= "white",size=1) +
    geom_point(data=all.sites, aes(x=lon,y=lat), color="grey70",alpha=0.5) +
    geom_point(data=d, aes(x=lon,y=lat), color="red",size=2) 
}