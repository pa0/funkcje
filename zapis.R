#formatowanie zapisu wyniku testu
#-------------
#-------------ToDo: wielkości efektu
#-------------

zapis <- function(wynik, test = "", w = 1, doc = "html") #w zależności od testu formatuje zapis wyniku
{
  ifelse(doc=="pdf", dwa <- "$^2$", dwa <- "2")
  sep <- "." #future: do formatów PL
  test0 <- names(wynik$statistic)	
  if(!existsFunction("myround")){
  	source("~/Documents/naukowe/funkcje/myRound.R")
  }
  p <- myround(wynik$p.value,3)
  ifelse(p=="< 0.001", p <- paste0("< 0",sep,"001"), p <- paste0("= ",p))  
  ifelse(test=="r" | test=="rho", t <- myround(wynik$estimate,2), t <- myround(wynik$statistic,2))
  #---
  if(test=="msd"){
    return(paste0(myround(wynik[1],1), " ± ", myround(wynik[2],2)))
  }
  if(test=="t"){
    df <- round(wynik$parameter,0)
    return(paste0("t(", df, ") = ", t, ", p ", p))
  } 
  if(test=="chi"){
    df <- round(wynik$parameter,0)
    return(paste0("chi",dwa,"(", df, ") = ", t, ", p ", p))
  } 
  if(test=="w"){ #Wilcoxon
    z <- myround(qnorm(1-(wynik$p.value/2)),2)
    return(paste0("W", " = ", t,", z = ",z, ", p ", p))
  }
  if(test=="ks"){ #Kołmogorow-Smirnow
    z <- myround(qnorm(1-(wynik$p.value/2)),2)
    return(paste0("D = ",t,", z = ", z, ", p ", p))
  }
  if(test=="r"){ #Pearson
    return(paste0("r(",wynik$parameter,") = ",t,", p ",p,", 95%CI = (",myround(wynik$conf.int[1],2),";",myround(wynik$conf.int[2],2),")"))
  }
  if(test=="rho"){ #Spearman
    return(paste0("rho = ",t,", p ",p))
  }
  if(test=="aov"){ #ANOVA dla summary(aov) dla jednoczynnikowej
  
  	if(class(wynik)[1]=="aov"){wynik<-summary(wynik)}
  
    df <- wynik[[1]]$Df
    f <- round(wynik[[1]][4][1,1],2)
    p <- wynik[[1]][5][1,1]
    ifelse(p < .05, es <- paste0(", eta",dwa," = ",round(wynik[[1]][2][1,1]/(wynik[[1]][2][1,1]+wynik[[1]][2][2,1]),2)), es <- "")
		p <- myround(p,3)
    ifelse(p=="< 0.001", p <- "< 0,001", p <- paste0("= ",p)) 
    return(paste0("F(",df[1],", ",df[2],") = ",f,", p ",p,es))
  }
  if(test=="ph"){ #post hoc dla jednoczynnikowej
  	p <- myround(wynik[[1]][w,4],3)
  	ifelse(p=="< 0.000", p <- "< 0,001", p <- paste0("= ",p)) 
    return(paste0("d = ", myround(wynik[[1]][w,1],2),", 95%CI = ", myround(wynik[[1]][w,2],2),"; ", myround(wynik[[1]][w,3],2),", p ", p))
  }
  if(test=="maov"){ #do n czynnikowej z badań Dawida
  	df1 <- summary(wynik)[[1]][1][[1]][w]
		df2  <- summary(wynik)[[1]][1][[1]][length(summary(wynik)[[1]][1][[1]])] #liczba wierszy, w ostatnim reszty
		f <-   round(summary(wynik)[[1]][4][[1]][w],2)
		p <-   myround(summary(wynik)[[1]][5][[1]][w],3)
		ifelse(p < .05, es <- paste0(", eta",dwa," = ",round(summary(wynik)[[1]][2][[1]][w]/sum(summary(wynik)[[1]][2][[1]]),2)), es<-"")
		ifelse(p=="< 0.001", p <- "< 0,001", p <- paste0("= ",p)) 
		return(paste0("F(",df1,", ",df2,") = ",f,", p ",p,es))
	}
  if(test=="cmaov"){ # car::Anova(lm(dane.m$ME ~ dane.m$przestępca * dane.m$org_border), type="II")
  	df1 <- wynik[w,2]
  	df2 <- wynik[4,2]
  	f <- round(wynik[w,3],2)
  	p <- wynik[w,4]
  	ifelse(p < .05, es <- paste0(", eta",dwa," = ",round(wynik[w,1]/sum(wynik[,1]),2)), es<-"")
  	p <- myround(p,3)
  	ifelse(p=="< 0.001", p <- "< 0.001", p <- paste0("= ",p)) 
  	return(paste0("F(",df1,", ",df2,") = ",f,", p ",p,es))
  }
  
  
  if(test0=="Bartlett's K-squared"){
    return(paste0("K",dwa,"(",wynik$parameter,") = ",t,", p ",p))
  }
  if(test0=="Friedman chi-squared"){
    df <- round(wynik$parameter,0)
    return(paste0("chi",dwa,"(", df, ") = ", t, ", p ", p))
  } 
  
  return("error w zapis()")
}

# ------------------------------------------------------
hsd <- function(wynik, dec="."){
  #post hoc
  p <- myround(wynik['p adj'],3)
  ifelse(p=="< 0.001", p <- paste0("< 0",dec,"001"), p <- paste0("= ",p)) 
  return(paste0("d = ", myround(wynik['diff'],2),", 95%CI = ", myround(wynik['lwr'],2),"; ", myround(wynik['upr'],2),", p ", p))
}

# -----------------------------------------------------
r.aov <- function(wynik, dec="."){ #anova powtarzane pomiary
	eps <- 1 #mnożnik do korekty df
	f <- wynik[[4]][2,5]
	p <- wynik[[4]][2,6]
	
	if(wynik[[6]][2] < .05){ #test Mauchly'ego na sferyczność
		p <- wynik[[5]][2]
		m <- wynik[[5]][1]
		if(wynik[[5]][1] > .75){ #epsilon Greenhouse-Geisser lub Huynh-Feldt
			p <- wynik[[5]][4]
			eps <- wynik[[5]][3]
		}
	}
	if(eps > 1){m <- 1}
	df1 <- round(wynik[[4]][2,2] * eps,1)
	df2 <- round(wynik[[4]][2,4] * eps,1)
	ifelse(p < .05, eta2 <- paste0(", eta2 = ",round(wynik[[4]][2,1]/sum(wynik[[4]][,1]),2)), eta2 <- "")
	p <- myround(p,3)
	ifelse(p=="< 0.001", p <- paste0("< 0",dec,"001"), p <- paste0("= ",p)) 
	return(paste0("F(",df1,", ",df2,") = ",round(f,2),", p ",p,eta2))
	
}
