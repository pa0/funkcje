### Funkcja, która robi ładną tabelkę ze średnimi i odchyleniami
### ToDo:=> opcje: SD, SDerr, CI; obsługa NA; sprawdzanie błędów
###

tabMSD <- function(df){
  # ramka z danymi, pierwsza kolumna jest grupująca

require(dplyr)

  
  if(!is.data.frame(df)){stop}
  colnames(df)[1] <- "grupa"

  N <- table(df$grupa)
  M <- df %>% group_by(grupa) %>% summarise_each(funs(mean))
  SD <- df %>% group_by(grupa) %>% summarise_each(funs(sd))

  nw <- dim(M)[1]
  nk <- dim(M)[2]
  
  W <- data.frame(matrix(NA,nw,nk))

  for(w in 1:nw){
    for(k in 2:nk){
      W[w,k] <- paste0(round(M[w,k],1)," (",round(SD[w,k],2),")")
    }
  }
  
  W[,1] <- M[,1]
  W <- data.frame(W[,1],as.vector(N),W[,-1]) #tu gubi kob i me
  colnames(W)[-2] <- colnames(M)  
  colnames(W)[2] <- "N"

  ## obrót tabeli: grupy w kolumnach
  if(nw<nk){
  W  <- t(W[,-1])
  colnames(W) <- M[,1]$grupa
  }
  
  return(W)
  
}