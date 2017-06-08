
daily_pcr <- function(data,lag=365,percent_var=0.85) {
  require(gridExtra)
  
  # prepare data
  lag <- lag
  X <- cbind(embed(data$p,lag+1),embed(data$tmax,lag+1),embed(data$tmin,lag+1))
  y <- data$cfs[(lag+1):nrow(data)]
  date <- data$date[(lag+1):nrow(data)]
  month <- data$month[(lag+1):nrow(data)]
  day <- data$day[(lag+1):nrow(data)]
  pca <- prcomp(X)
  lim <- percent_var
  pc.prop <- cumsum((pca$sdev^2)/sum(pca$sdev^2))
  ndx <- which(pc.prop<=lim)
  pcs <- pca$x[,ndx]
  
  # principal component regression
  pcr <- lm(y~pcs,na.action=na.exclude)
  
  # plot daily residuals
  daily <- data.frame(date=date,
                      month=month,
                      day=day,
                      Q=y,
                      residuals=pcr$residuals)
  
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
    mutate(day=1:(lag+1),
           month=ordered(month.abb[month], month.abb))
  
  season.color = colorRampPalette(c("blue","green","red","orange","blue"))(12)
  
  p2 <- ggplot(monthly) + geom_line(aes(day,residuals),size=0.3) +
    geom_line(aes(day,residuals,color=month)) + theme_bw() +
    scale_color_manual(values=season.color) +
    guides(col = guide_legend(ncol = 12)) +
    geom_hline(yintercept=0, linetype="dashed") +
    labs(x="Julian day",y="median residuals") +
    theme(legend.position="bottom")
  
  grid.arrange(p1,p2,ncol=1,top="Residuals from PCR model")
  
}