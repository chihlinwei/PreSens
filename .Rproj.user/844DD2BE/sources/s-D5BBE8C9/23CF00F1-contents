#' Plot TOU
#'
#' This function plots TOU and the 85% of the initial DO value
#'
#' @param x a dataframe derived from \code{\link[PreSens]{keepLast}}
#' @author Chih-Lin Wei <chihlinwei@@gmail.com>
#' @export
#' @examples
#' dat <- keepLast("D:/Projects_MOST/GPSC_cruise/OR1_1219/scoc1219/measurement")
#' touPlot(dat)

touPlot <- function(x){
  ini <- splitBy(~Sample, x) %>% lapply(FUN=function(x)x[1,]) %>% ldply(.id="Sample")
  ggplot(data=x, aes(x=delta_t, y=Value))+
    geom_point()+
    stat_smooth(method="lm", formula=y~x, se=F)+
    geom_hline(data=ini, aes(yintercept=Value*0.85), linetype=2, colour="red")+
    facet_wrap(~Sample, scales="free")+
    labs(y=expression(Dissolved~Oxygen~(mu*mol~L^-1)), x="Time (minutes)")+
    theme_bw()
}
