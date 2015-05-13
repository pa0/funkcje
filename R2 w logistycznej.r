# Funkcja do obliczania R2 Cox'a-Snella i R2 Neglerkego

rki <- function(model0,model1,n){
	llm <- logLik(model1) 
	ll0 <- logLik(model0)
	
	r2cs <- 1-exp(-2*(llm-ll0)/n)
	r2n <- (1 - exp(-2 * (llm - ll0)/n)) / (1 - exp(2 * ll0/n))
	
	result <- list("R^2 Cox'a-Snella"=r2cs,"R^2 Neglerkego"=r2n)
	result
}