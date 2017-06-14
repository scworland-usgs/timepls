#' @title plot_cor
#' @description Plots rolling correlation for timepls_fit S4 object's date and residuals
#' @import ggplot2
#' @import roll
#' @param x time_pls fit object from time_pls function.
#' @param y not used
#' @export

plot_cor <- function(fit,window) {
  obs <- fit@observed
  est <- fit@estimated

  mat <- matrix(c(obs,est),ncol=2)
  x <- roll_cor(mat,window)[1,2,]
cp <- cpt.mean(na.omit(x), method="PELT")
num <- length(cp@cpts)-1

ggplot(na.omit(data.frame(y=x,time=1:length(x)))) +
  geom_line(aes(time,y)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_segment(aes(x=0,y=0.8,xend=400,yend=0.8), color="blue") +
  geom_segment(aes(x=400,y=0,xend=800,yend=0), color="blue") +
  geom_segment(aes(x=800,y=-0.8,xend=1200,yend=-0.8), color="blue") +
  scale_x_continuous(breaks=(seq(0,1200,100))) +
  scale_y_continuous(breaks=(seq(-1,1,0.2))) +
  coord_cartesian(ylim=c(-1,1)) +
  labs(y="correlation") +
  ggtitle("Rolling correlation with 50 step window") +
  theme_bw()
}
