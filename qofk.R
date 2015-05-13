# jakość klasyfikacji

qofk <- function(x, odw = F){
#
# E+ - przewidywany "jest"
# E- - przewidywany "brak"
#
# macierz domyślna ma kolejność:
#      | E- | E+
# --------------- 
# brak | TN | FP
# jest | FN | TP
#
# macierz odw=T ma kolejność:
#      | E+ | E-
# --------------- 
# jest | TP | FN
# brak | FP | TN


  # obsługa błędów
  if(!is.matrix(x)){
   print("X nie jest macierzą")
    break
  }
  if(!(dim(x)[1]==2 & dim(x)[2]==2)){
    print("Zła liczba wymiarów")
    break
  }
  # obliczanie wskaźników
  x <- as.vector(x)
  # kolejność w tabeli 
  ifelse(odw==T,kol <- 1:4, kol <- 4:1)
  
  tp <- x[kol[1]]
  fp <- x[kol[2]]
  fn <- x[kol[3]]
  tn <- x[kol[4]]

  # poprawność klasyfikacji
  a <- round((tp+tn)/sum(x)*100,1)
  # błąd
  # er <- round((fp+fn)/sum(x)*100,1)
  # specyficzność
  s <- round(tn/(tn+fp)*100,1)
  # czułość
  c <- round(tp/(tp+fn)*100,1)
  # średnia geometryczna s i c
  gm <- round(sqrt(s*c),1)
  # dobroć
  pauc <- round(((1 + c - (fp/(fp+tp)))/2),1)
  # iloraz wiarygodności
  lr <- round((tp/(tp+fn))/(fp/(fp+tn)),2)
  
  # wynik
  wynik <- c(a,s,c,gm,pauc,lr)
  names(wynik) <- c("poprawność", "specyficzność", "czułość", "GMean", "pseudo.AUC","LR")
  wynik
}

# To Do
# The overall accuracy and unweighted Kappa statistic are calculated. A p-value from McNemar's test is also computed using mcnemar.test (which can produce NA values with sparse tables).
# 
# The overall accuracy rate is computed along with a 95 percent confidence interval for this rate (using binom.test) and a one-sided test to see if the accuracy is better than the "no information rate," which is taken to be the largest class percentage in the data.