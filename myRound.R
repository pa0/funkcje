#
# intencjonalne zaokrąglanie do podanych miejsc po przecinku
#
myround <- function(x, digits=1)
{
  if(digits < 0) 
    stop("This is intended for the case digits >= 0.")
  
  if(length(digits) > 1) {
    digits <- digits[1]
    warning("Using only digits[1]")
  }

  tmp <- sprintf(paste("%.", digits, "f", sep=""), x)

  # deal with "-0.00" case
  zero <- paste0("0,", paste(rep("0", digits), collapse=""))
  tmp[tmp == paste0("-", zero)] <- zero

	return(ifelse(tmp=="0.000",tmp<-"< 0.001",tmp))
}