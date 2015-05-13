#
# kasuje wartości mniejsze niż .3 - do ukrywania ładunków czynnikowych
#
hide3 <- function(x,prog=.3)
  {
  x <- as.numeric(x)
  if(x<prog){x <- NA}
  return(x)
  }
