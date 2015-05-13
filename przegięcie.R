# Wyznaczanie punktu przegięcia

x = seq(1,15)
y = c(4,5,6,5,5,6,7,8,7,7,6,6,7,8,9)
plot(x,y,type="l",ylim=c(3,10))
lo <- loess(y~x)
xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
out = predict(lo,xl)
lines(xl, out, col='red', lwd=2)
infl <- c(FALSE, diff(diff(out)>0)!=0)
points(xl[infl ], out[infl ], col="blue")


inflection::findiplist(x,y,0) # wychodzą inne, ale funkcja dopasowania też jest inna




###
setwd("~/Dropbox/UAM--IP/---Jola Ch/punkty przegiecia/")
for(i in 2:9){
d1 <- d[complete.cases(d[,c(1,i)]) & d$mcy<240 ,]

x <- d1$mcy
#y = caTools::runmean(d1[,i], 3) #średnia ruchoma do wygładzania
y <- d1[,i]

png(paste0("Rplot",i,".png"))

plot(x,y,type="p", xlab="miesiące", ylab=colnames(d)[i])
lo <- loess(y~x)
xl <- seq(min(x),max(x), (max(x) - min(x))/10000)
out = predict(lo, xl)
lines(xl, out, col='red', lwd=2)
infl <- c(FALSE, diff(diff(out)>0)!=0)
(x.przeg <- min(xl[which(infl==T)]))
#points(xl[infl ], out[infl ], col="blue", pch=20)
points(x.przeg, out[min(which(infl==T))], col="blue", pch=20)
mtext(3, text = paste0("Punkt przegięcia dla ",colnames(d)[i],": mcy = ",x.przeg))

graphics.off()

}

