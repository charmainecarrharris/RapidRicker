#' calcMCMCRickerBM
#'
#' This function calculates Ricker Model parameters for spawner-recruit data with a simple linear fit of log(R/S) ~ S implemented with an MCMC using the jags() function from the R2jags package. For details such as model form and variable definitions, refer to \href{https://github.com/SOLV-Code/RapidRicker/wiki/MCMC-Using-R2Jags}{this wiki page}. Note that these are designed as quick fits to support initial exploration of Bayesian posteriors,  to support a pre-screeing of assumptions before setting up a proper model fit. Some standard diagnostics can be part of the output, but the function does NOT do any quality control for you. Also, the default priors and inits may not make sense for your data. There are many tutorials available online for linear model fits and associated diagnostics using R2jags (e.g. \href{http://biometry.github.io/APES//LectureNotes/StatsCafe/Linear_models_jags.html}{here},\href{https://rpubs.com/corey_sparks/30893}{here}, and \href{https://rpubs.com/Niko/332320}{here}).
#' Also calculates standard biological benchmarks (Smsy, Seq, Smax, Umsy). Benchmark calculations were adapted from BUGS code used in Miller & Pestal (2020), available \href{https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_035-eng.pdf}{here}.
#' Two versions for some BM are produced: "_h" = Hilborn Proxy (\href{https://cdnsciencepub.com/doi/pdf/10.1139/f85-230}{Hilborn 1985}) and "_p" = Peterman Proxy" (\href{https://cdnsciencepub.com/doi/pdf/10.1139/f99-204}{Peterman et al. 2000}).
#' @param sr_obj a data frame with Spn and Rec (Data for 1 Stock!). Other variables can be there but are not used (RpS, Qual, ExpF etc)
#' @param min.obs min number of S-R pairs needed to fit a model
#' @param mcmc.settings a list with n.chains (2), n.burnin (20000), n.thin (60), and n.samples (50000). Default values in brackets.
#' @param mcmc.inits a list of lists with inits for each chain. Default is "list(list(tau_R=3, C=1),list(tau_R=7, C=2))"
#' @param mcmc.priors a list with p.alpha, p.beta, tau_alpha,tau_beta
#' @param pars.track one of "short" (only track a few key paramters) or "all"
#' @param output one of "short" (only return summary stats for key parameters in a list object), "post" (also save posterior distribution samples to folder), or "all" (also produce pdf files with standard diagnostic plots)
#' @param out.path text string specifying  folder. if output is "post" or "all", the generated files will be stored to this folder
#' @param out.label label use in the output files if output is "post" or "all"
#' @param mcmc.seed either "default" or an integer giving the random seed for starting MCMC (R2Jags default is 123)
#' @keywords Ricker fit, Bayesian, MCMC, posterior, Smsy, Smax, Seq, Umsy
#' @export
#' @examples
#' ricker.bm <- calcDetRickerBM(SR_Sample[SR_Sample$Stock == "Stock1",],min.obs = 10)
#' print(ricker.bm)

calcMCMCRickerBM <- function(sr_obj,min.obs=15, 
					mcmc.settings = list(n.chains=2, n.burnin=20000, n.thin=60,n.samples=50000),
					mcmc.inits = list(list(tau_R=3, C=1),list(tau_R=7, C=2)),
					mcmc.priors = list(p.alpha = 0,tau_alpha = 0.0001, p.beta = 1, tau_beta = 0.1)
					pars.track = "short",
					output = "short"
					out.path = "MCMC_Out",
					out.label = "MCMC",
					mcmc.seed = "default"
					){


# Prep the data
sr.use  <- sr_obj %>% dplyr::filter(!is.na(Rec),!is.na(Spn)) # drop incomplete records
mcmc.data <- list(S = sr.use$Spn, R_Obs = sr.use$Rec, N = dim(sr.use)[1])


# Define the model
mcmc.model <- function(){
	# adapted from code originally developped by Catherine Michielsens, Sue GRant, and Bronwyn MacDonald.
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

if(pars.track == "short"){pars.track.in <- c("ln.alpha.c","beta","sigma","deviance","S.max","S.msy.c2"))

if(pars.track == "all"){pars.track.in <- c("ln.alpha","ln.alpha.c","beta","sigma","deviance", "S.max",
						"S.eq.c","S.msy.c", "U.msy.c","S.msy.c.80",
						"S.eq.c2","S.msy.c2", "U.msy.c2","S.msy.c2.80")}


# Do the MCMC

tmp.out <- mcmc.sub(data.obj = mcmc.data, 
                    model.in = mcmc.model, 
                    inits.in = mcmc.inits, 
                    settings.in = mcmc.settings ,
                    pars.to.track.in = pars.track.in, 
                    out.label= out.label,
					out.path= out.path,
					output=output,
                    mcmc.seed = mcmc.seed,	
					)
					
					# prefix=gsub("/","",cu.do), 					 
                     #DIC.in=FALSE,debug.in=FALSE, save.history.in=FALSE,
                     #tracing.in=FALSE, write.CODA.in=TRUE,
                     #diag.plots.in=FALSE, CODA.diag.in=TRUE,
                     #perc.vec=seq(5,95,by=5),



#extract the results

if(pars.track == "short"){
out.vec <-  c(
			n_obs = dim(sr.use)[1] ,
			ln_a_c = median(tmp.out$ln.alpha.c,na.rm=FALSE),
			b = median(tmp.out$beta,na.rm=FALSE),
			sd = median(tmp.out$sigma,na.rm=FALSE),
			deviance = median(tmp.out$deviance,na.rm=FALSE),
			Smax = median(tmp.out$S.max,na.rm=FALSE),
			Smsy_p = median(tmp.out$S.msy.c2,na.rm=FALSE)
			)
}  #end if "short"


if(pars.track == "all"){

# "ln.alpha","ln.alpha.c","beta","sigma","deviance", "S.max",
#						"S.eq.c","S.msy.c", "U.msy.c","S.msy.c.80",
#						"S.eq.c2","S.msy.c2", "U.msy.c2","S.msy.c2.80"

out.vec <-  c(
			n_obs = dim(sr.use)[1] ,
			ln_a_c = median(tmp.out$ln.alpha.c,na.rm=FALSE),
			b = median(tmp.out$beta,na.rm=FALSE),
			sd = median(tmp.out$sigma,na.rm=FALSE),
			deviance = median(tmp.out$deviance,na.rm=FALSE),
			Smax = median(tmp.out$S.max,na.rm=FALSE),
			Smsy_p = median(tmp.out$S.msy.c2,na.rm=FALSE)
			)
}  #end if "all"



} # if n >= min.obs


if(dim(sr.use)[1] < min.obs){

out.vec <-  c(n_obs = dim(sr.use)[1],
			ln_a = NA,
			ln_a_c = NA,
			a = NA,
			b = NA,
			sd = NA,
			Smax = NA,
			Seq = NA,
			Seq.c = NA,
			Smsy_h = NA,
			Umsy_h = NA,
			Smsy_p = NA,
			Umsy_p = NA
			)

}

return(out.vec)
  
}











