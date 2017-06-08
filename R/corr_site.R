
corr_site <- function(data,v='p') {
  
  site_no <- length(unique(data$site))
  if(site_no > 1) {stop("Please do not provide more than 1 site")}
  
  require(ggplot2)
  require(dplyr)
  require(gridExtra)
  require(lazyeval)
  
  data_sub <- data[,c('site','month','day','cfs',v)]
  
  d <- data_sub %>%
    group_by(site,month,day) %>%
    mutate_(day_corr=interp(~cor(log10(cfs),v),v=as.name(v))) %>%
    ungroup() %>%
    distinct(site,month,day,day_corr) %>%
    arrange(month,day) %>%
    mutate(day=rep(1:366, length(unique(site))),
           month=ordered(month.abb[month], month.abb)) %>%
    na.omit()
  
  season.color = colorRampPalette(c("blue","green","red","orange","blue"))(12)
  
  site = unique(d$site)
  
  p <- ggplot(d) + geom_line(aes(day,day_corr,color=month)) + theme_bw() +
    scale_color_manual(values=season.color) +
    ggtitle(paste("Daily correlation between log10(Q) and",v,sep=" "),
            subtitle=paste("For site number", site, sep=" ")) +
    labs(x="Julian day",y="Daily correlation")
  
  m <- map_sites(data,site.coords)
  
  grid.arrange(p,m,ncol=2)
}








