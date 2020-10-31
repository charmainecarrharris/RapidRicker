#' calcDetRickerBM
#'
#' This function calculates Ricker Model parameters for spawner-recruit data using a simple linear regression of log(R/S) ~ S as well as resulting biological benchmarks.  Note that these are simple deterministic model fits intended for rapid testing of input data!
#' Benchmark calculations were adapted from BUGS code used in Miller & Pestal (2020), 
#' available at https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_035-eng.pdf
#' Two versions for some BM are produced: "h = Hilborn Proxy" (REF), "p = Peterman Proxy" (REF)
#' @param sr_obj a data frame with Year and Spn, logRpS (Data for 1 Stock!). Other variables can be there but are not used (RpS, Qual, ExpF etc)
#' @param min.n min number of S-R pairs needed to fit a model
#' @keywords Ricker fit, Smsy, Smax, Seq, Umsy
#' @export
#' @examples
#' ricker.bm <- calcDetRickerBM(SR_Sample[SR_Sample$Stock == "Stock1",],min.obs = 10)
#' print(ricker.bm)

calcDetRickerBM <- function(sr_obj,min.n=15){
# 
# 
# 
# 
  

sr.use  <- sr_obj %>% dplyr::filter(!is.na(logRpS),!is.na(Spn))

if(dim(sr.use)[1] >= min.n){

ricker.fit <- lm(sr.use$logRpS ~ sr.use$Spn)
ricker.sigma <- sigma(ricker.fit)
ricker.lna <- ricker.fit$coefficients[1]
ricker.lna.c <- ricker.lna + (ricker.sigma^2  / 2)
ricker.a <- exp(ricker.lna.c)
ricker.b <- - ricker.fit$coefficients[2]

S.max <- 1 / ricker.b 
S.eq <- ricker.lna  * S.max
S.eq.c <- ricker.lna.c  * S.max

# hilborn proxy
U.msy.h <- ricker.lna.c * (0.5-0.07*ricker.lna.c)
S.msy.h <- S.eq.c *(0.5-0.07*ricker.lna.c)  
  
# peterman correction
peterman.approx.c <- (0.5 - 0.65 * ricker.lna.c^1.27 / (8.7 + ricker.lna.c^1.27))
U.msy.p <- ricker.lna.c * peterman.approx.c 
S.msy.p <- U.msy.p / ricker.b  



out.vec <-  c(
			n_obs = dim(sr.use)[1] ,
			ln_a = round(as.vector(ricker.lna),3), # need as.vector to fix names in output)
			ln_a_c = round(as.vector(ricker.lna.c),3),
			a = round(as.vector(ricker.a),3),
			b = c(as.vector(ricker.b)),
			sd = round(as.vector(ricker.sigma),3),
			Smax = round(as.vector(S.max)),
			Seq = round(as.vector(S.eq)),
			Seq.c = round(as.vector(S.eq.c)),
			Smsy_h = round(as.vector(S.msy.h)),
			Umsy_h = round(as.vector(U.msy.h),2),
			Smsy_p = round(as.vector(S.msy.p)),
			Umsy_p = round(as.vector(U.msy.p),2)
			)

} # if n >= min.n


if(dim(sr.use)[1] < min.n){

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











