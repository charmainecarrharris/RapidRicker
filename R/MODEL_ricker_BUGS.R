#' ricker.BUGS
#'
#' This function is the BUGS version of the Ricker model fit and associated benchmarks. It is used inside
#' of calcMCMCRickerBM() which passes it to  doRJAGS(), which is a wrapper for the jags() function from the R2jags package. For details such as model form and variable definitions, refer to \href{https://github.com/SOLV-Code/RapidRicker/wiki/MCMC-Using-R2Jags}{this wiki page}.
#' This function has no arguments, because the jags call sets up the inputs. The code is adapted from code originally developed by Catherine Michielsens, Sue Grant, Bronwyn MacDonald, Carrie Holt and others. Examples of previous use are \href{https://www.dfo-mpo.gc.ca/csas-sccs/publications/resdocs-docrech/2015/2015_048-eng.html}{Taku Coho} and \href{https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_035-eng.html}{Taku Sockeye}.
#' @export

ricker.BUGS <- function(){
	# adapted from code originally developed by Catherine Michielsens, Sue Grant, and Bronwyn MacDonald.
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
	
	# Hilborn Proxy ("_h" in the output from calcDetRickerBM)
    U.msy.c <- ln.alpha.c * (0.5-0.07*ln.alpha.c)
    S.msy.c <- S.eq.c *(0.5-0.07*ln.alpha.c)  
    
	# Peterman Approximation ("_p" in the output from calcDetRickerBM)
    positive.lna.c <- step(ln.alpha.c)
    ln.alpha.c.nonneg <- ln.alpha.c * positive.lna.c
    S.eq.c2 <- ln.alpha.c.nonneg * S.max 
    peterman.approx.c <- (0.5 - 0.65*pow(ln.alpha.c.nonneg,1.27) / (8.7 +pow(ln.alpha.c.nonneg,1.27)))
    U.msy.c2 <- ln.alpha.c.nonneg * peterman.approx.c 
    S.msy.c2 <- U.msy.c2 / beta  
    U.max.c2 <- 1 - 1 / exp(ln.alpha.c.nonneg) 

}