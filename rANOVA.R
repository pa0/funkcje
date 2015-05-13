# najprostsze podejście dla powtarzanych pomiarów
hh$delta <- with(hh, korekta+zmiana)
tmp <- aov(delta ~ nw * as.factor(zmiana) + Error(subject), data=hh)

op <- par(no.readonly = TRUE)
par(mar=c(4,3,3,2)+.1)
par(cex=.7)

with(hh, interaction.plot(x.factor = zmiana, trace.factor = nw, response = delta, 
                          ylim=c(-10,10), lty=c(1,12), lwd=2, cex=.6, trace.label="Group"))
par(op)


boxplot(hh$delta ~ hh$zmiana, at = 1:10 +.2, , boxwex=.2, col="yellow", subset= hh$nw=="W", notch=T)
boxplot(hh$delta ~ hh$zmiana, at = 1:10 -.2, , boxwex=.2, col="orange", subset= hh$nw=="N", notch=T, add=T, xlab=)
legend(4.5, -30, legend = c( "wysokie", "niskie" ), fill   = c( "yellow", "orange" ) )

#  poszczególne różnice
zmiany <- unique(hh$zmiana)
p.adj <- NA
for(i in 1:length(zmiany)){
  print(paste("===",zmiany[i],"==="))
  test <- with(tmp <- subset(hh, hh$zmiana==zmiany[i]), t.test(tmp$delta ~ tmp$nw))
  print(test)
  p.adj <- c(p.adj, test$p.value)
}
p.adj <- p.adj[-1] # usuwam NA
p.adj <- round(p.adj <- p.adjust(p.adj, "holm"),3)

text(1:10, tapply(hh$delta, hh$zmiana, mean)+1, p.adj )
mtext("p z korektą Holma dla różnic między grupami", 3)


#prepare data
labki=unique(hh$zmiana)
tmp <- data.frame(hh$korekta,factor(hh$zmiana, levels=labki, labels=as.character(labki)), hh$nw, hh$subject)
colnames(tmp) <- edit(colnames(tmp))

options(contrasts=c("contr.sum","contr.poly"))

# rAnova UNIVARIATE
library(nlme)
analysis <- lme(korekta ~ zmiana + nw, random = ~1 |subject/zmiana, data=tmp)
summary(analysis)

library(multicomp)
(posthoc <- summary(glht(analysis, linfct=mcp(zmiana="Tukey"))))


#----- MULTIVARIATE
hh$pomiar  <- rep(1:20, 2800/20)
tmp <- reshape(hh, direction = "wide", idvar = c("pomiar","subject","nw"), v.names="korekta", timevar="zmiana")

hh.wide <- cbind(tmp[,grepl("korekta", colnames(tmp))])
### tutaj jest błąd
hh.lm <- lm(hh.wide ~ 1)

### ---------
# sprawdzić kiedyś

rfactor <- factor(c("",""...))

library(car)

analiza  <- Anova(hh.lm, idata=data.frame(rfactor), idesign=~rfactor, type="III")
summary(hh.lm, multivariate=F)

#kontrasty

t.test(hh.wide[,1] - hh.wide[,2]) # różnica bez korekty

# korekta błędu I rodzaju
# t -> F
myF <- t^2 # dla df=1
myQ <- sqrt(2*myF)
nGroups <- length(levels(rfactor))
p.adj  <- 1 - ptukey(myQ, nGroups, (nGroups -1)*(nSubjectPerGroup - 1))

