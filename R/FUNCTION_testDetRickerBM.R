#' testDetRickerBM
#'
#' This function calculates standard biological benchmarks (Smsy, Seq, Smax, Umsy) for different subsets of the input data, using the function calcDetRickerBM(). See calculation details there.
#' @param sr_obj a data frame with Year and Spn, logRpS (Data for 1 Stock!). Other variables can be there but are not used (RpS, Qual, ExpF etc). 
#' @param min.obs min number of S-R pairs needed to fit a model
#' @param type one of "jack" (for a drop 1 jackknife test), "retro" (retrospective test starting with min.obs, then adding more years), or "revretro" (reverse retrospective, starting with all years and them dropping the earlier obs one at a time until only the most recent min.obs are left).
#' @param trace if TRUE, print various intermediate diagnostic output to the console
#' @keywords sensitivity test
#' @export
#' @examples
#' ricker.test <- testDetRickerBM(SR_Sample[SR_Sample$Stock == "Stock1",],min.obs = 10,  type="retro")



testDetRickerBM <- function(sr_obj,min.obs=15, type="jack",trace = FALSE){

sr.use  <- sr_obj %>% dplyr::filter(!is.na(logRpS),!is.na(Spn))

bm.cols <- c("n_obs", "ln_a","ln_a_c","a","b","sd","Smax","Seq","Seq.c","Smsy_h","Umsy_h",
             "Smsy_p","Umsy_p")
			 

if(type == "jack"){
	yrs.do <- sr.use$Year
	bm.test.store <- as.data.frame(matrix(NA,nrow=length(yrs.do),ncol= length(bm.cols),
                                     dimnames=list(yrs.do,bm.cols)))
	for(yr in yrs.do){
			sr.in <- sr.use %>% dplyr::filter(Year != yr)
			bm.i <- calcDetRickerBM(sr_obj = sr.in,min.obs=min.obs)
			bm.test.store[as.character(yr),] <- bm.i
		}
		
	bm.test.store <-  bm.test.store %>% rownames_to_column(var="DropYr")
} # end if "jack"



if(type == "retro"){
	
	
	yrs.do <- (min(sr.use$Year) + min.obs):max(sr.use$Year)
	
	#print(yrs.do)
	
	bm.test.store <- as.data.frame(matrix(NA,nrow=length(yrs.do),ncol= length(bm.cols),
                                     dimnames=list(yrs.do,bm.cols)))
	for(yr in yrs.do){
			sr.in <- sr.use %>% dplyr::filter(Year <= yr)
			bm.i <- calcDetRickerBM(sr_obj = sr.in,min.obs=min.obs)
			bm.test.store[as.character(yr),] <- bm.i
		}
		
	bm.test.store <-  bm.test.store %>% rownames_to_column(var="UpToYr")
} # end if "retro"



if(type == "revretro"){
	
	
	yrs.do <- min(sr.use$Year) : (max(sr.use$Year) - min.obs)
	
	if(trace){print(yrs.do)}
	
	bm.test.store <- as.data.frame(matrix(NA,nrow=length(yrs.do),ncol= length(bm.cols),
                                     dimnames=list(yrs.do,bm.cols)))
	for(yr in yrs.do){
			sr.in <- sr.use %>% dplyr::filter(Year >= yr)
			if(trace){print(sr.in$Year)}
			bm.i <- calcDetRickerBM(sr_obj = sr.in,min.obs=min.obs)
			bm.test.store[as.character(yr),] <- bm.i
		}
		
	bm.test.store <-  bm.test.store %>% rownames_to_column(var="SinceYr")
} # end if "revretro"



return(bm.test.store)

}



