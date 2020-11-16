#' calcMCMCRickerBM
#'
#' This function is the BUGS version of the Ricker model fit and associated benchmarks it is used inside
#' of calcMCMCRickerBM() which passes it to  doRJAGS(), which is a wrapper for the jags() function from the R2jags package.
#' This function has not arguments, because the jags call sets iup the inputs.


ricker.BUGS <- function(){
	# adapted from code originally developed by Catherine Michielsens, Sue GRant, and Bronwyn MacDonald.
    for (i in 1:N) {                       #loop over N sample points
      R_Obs[i] ~ dlnorm(logR[i],tau_R)          #likelihood -> predicted value for NA in data set
      logR[i] <- RS[i] +log(S[i])               # calc log(R) - fitted values
      RS[i] <- ln.alpha - beta * S[i]           # ricker model 
   }

    
    ln.alpha ~ dnorm(p.alpha,tau_alpha)        #prior for alpha (actually ln.alpha!) -> fix? 
    beta <-1/C					   # prior for beta
    C~ dlnorm(p.beta, tau_beta)       			   # prior for beta 
    tau_R ~ dgamma(0.001,0.001)                    #prior for precision parameter
    sigma <- 1/sqrt(tau_R) 			# changed based on Fleishman and Evenson     	
	
	ln.alpha.c <- ln.alpha + (sigma * sigma / 2) # bias correction for lognormal skewness
    
    #BIOLOGICAL BENCHMARKS
    # adapted from code in Miller & Pestal 2020 Taku Sockeye Res Doc
    S.max <- 1 / beta 
    alpha.c <- min(exp(ln.alpha.c),1.0E4)
    S.eq.c <- ln.alpha.c * S.max 
    U.msy.c <- ln.alpha.c * (0.5-0.07*ln.alpha.c)
    S.msy.c <- S.eq.c *(0.5-0.07*ln.alpha.c)  
    
    positive.lna.c <- step(ln.alpha.c)
    ln.alpha.c.nonneg <- ln.alpha.c * positive.lna.c
    S.eq.c2 <- ln.alpha.c.nonneg * S.max 
    peterman.approx.c <- (0.5 - 0.65*pow(ln.alpha.c.nonneg,1.27) / (8.7 +pow(ln.alpha.c.nonneg,1.27)))
    U.msy.c2 <- ln.alpha.c.nonneg * peterman.approx.c 
    S.msy.c2 <- U.msy.c2 / beta  
    U.max.c2 <- 1 - 1 / exp(ln.alpha.c.nonneg) 
    S.msy.c.80 <- S.msy.c *0.80
    S.msy.c2.80 <- S.msy.c2 *0.80    

}