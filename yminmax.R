#obliczanie przedziału ufności do ggplot2
yminmax <- function(x) {
  se <- function(x){
    sd(x, na.rm=T)/sqrt(length(x)-1)
  }
  result = c(mean(x, na.rm=T) - 1.96*se(x), mean(x, na.rm=T) + 1.96*se(x))
  names(result) = c("ymin", "ymax")
  result
}


# ggplot(mo., aes(x=badacz, y=wynik, group=płeć))+
#   facet_wrap(~wskaźnik, nrow=2, ncol=3)+
#   geom_point(position = position_jitter(width=.2,height = .2), alpha=.1) +
#   stat_summary(aes(group=płeć),fun.data="yminmax", geom="errorbar", width=.3, alpha=.6)+
#   stat_summary(aes(group=płeć),fun.y=mean, geom="point", size=2)+
#   stat_summary(aes(group=płeć, linetype=płeć),fun.y=mean, geom="line", size=1)+
#   labs(y=paste0("wartość wskaźnika"," [95%CI]"), title="")+
#   #coord_cartesian(ylim=c(0,.75))+
#   theme_bw()