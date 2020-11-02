#' RapidRicker
#'
#' This function is a wrapper for testDetRickerBM(). It loops throug multiple stocks and applies all the sensitivity test variations, then calculates the perc differences to base case. For calculation details, refer to calcDetRickerBM and testDetRickerBM.
#' @param sr_obj_m a data frame with Stock, Year and Spn, logRpS (Data for multiple stocks!). Other variables can be there but are not used (RpS, Qual, ExpF etc). 
#' @param min.obs min number of S-R pairs needed to fit a model
#' @param trace if TRUE, print various intermediate diagnostic output to the console
#' @keywords sensitivity test
#' @export
#' @examples
#' rapid.ricker.out <- RapidRicker(SR_Sample,min.obs = 10,  trace=FALSE)


RapidRicker <- function(sr_obj_m,min.obs = 10, trace = TRUE){


stk.list <- sort(unique(sr_obj_m$Stock))
if(trace){
  print(paste(length(stk.list), "Stocks"))
  print(stk.list)
}


#-----------------------------------------------------------------------
# Part 1: Simple Ricker BM

bm.cols <- c("n_obs", "ln_a","ln_a_c","a","b","sd","Smax","Seq","Seq.c","Smsy_h","Umsy_h",
             "Smsy_p","Umsy_p")

bm.det.store <- as.data.frame(matrix(NA,nrow=length(stk.list),ncol= length(bm.cols),
                                     dimnames=list(stk.list,bm.cols)))


for(stk in stk.list){

  print("------------")
  print(stk)
  sr.sub <- sr_obj_m %>% dplyr::filter(Stock == stk)
  bm.tmp <- calcDetRickerBM(sr_obj = sr.sub,min.obs=)
  bm.det.store[stk,] <- bm.tmp
}

bm.det.store <- bm.det.store %>% rownames_to_column(var="Stock")
if(trace){print(head(bm.det.store))}


#-----------------------------------------------------------------------
# Part 2: Drop-1 Jackknife Simple Ricker BM


jack.bm.det.store <- bm.det.store[FALSE,] %>% mutate(DropYr = NA) %>% select(Stock, DropYr, everything())

for(stk in stk.list){

  print("------------")
  print(stk)

  sr.sub <- sr_obj_m %>% dplyr::filter(Stock == stk) 

  if(sum(!is.na(sr.sub$Spn) & !is.na(sr.sub$logRpS) ) > min.obs){

    jack.tmp <- testDetRickerBM(sr_obj = sr.sub,min.obs=min.obs,type="jack")
    jack.tmp <- jack.tmp %>% mutate(Stock = stk) %>% select(Stock, DropYr,everything())
    jack.bm.det.store <- rbind(jack.bm.det.store,jack.tmp)
    }

}

if(trace){print(head(jack.bm.det.store))}



#-----------------------------------------------------------------------
# Part 3: Retrospective

retro.bm.det.store <- bm.det.store[FALSE,] %>% mutate(DropYr = NA) %>% select(Stock, DropYr, everything())

for(stk in stk.list){

  print("------------")
  print(stk)

  sr.sub <- sr_obj_m %>% dplyr::filter(Stock == stk) 
  
  if(sum(!is.na(sr.sub$Spn) & !is.na(sr.sub$logRpS) ) > min.obs){

    retro.tmp <- testDetRickerBM(sr_obj = sr.sub,min.obs=min.obs,type="retro")
    retro.tmp <- retro.tmp %>% mutate(Stock = stk) %>% select(Stock, UpToYr,everything())
    retro.bm.det.store <- rbind(retro.bm.det.store,retro.tmp)
  }

}


if(trace){print(head(retro.bm.det.store))}




#-----------------------------------------------------------------------
# Part 4: Reverse Retrospective

revretro.bm.det.store <- bm.det.store[FALSE,] %>% mutate(SinceYr = NA) %>% select(Stock, SinceYr, everything())

for(stk in stk.list){

  print("------------")
  print(stk)

  sr.sub <- sr_obj_m %>% dplyr::filter(Stock == stk) 
  
  if(sum(!is.na(sr.sub$Spn) & !is.na(sr.sub$logRpS) ) > min.obs){

    revretro.tmp <- testDetRickerBM(sr_obj = sr.sub,min.obs=min.obs,type="revretro",trace=FALSE)
    revretro.tmp <- revretro.tmp %>% mutate(Stock = stk) %>% select(Stock, SinceYr,everything())
    revretro.bm.det.store <- rbind(revretro.bm.det.store,revretro.tmp)
  }

}



if(trace){print(head(revretro.bm.det.store))}

#-----------------------------------------------------------------------
# Part 5: Drop 2 largest Spn (to check how much "pull" they have on the fit)


drop2.bm.det.store <- as.data.frame(matrix(NA,nrow=length(stk.list),ncol= length(bm.cols),
                                           dimnames=list(stk.list,bm.cols)))

for(stk in stk.list){

  print("------------")
  print(stk)



  sr.sub <- sr_obj_m %>% dplyr::filter(Stock == stk) 

    # drop the 2 R,S pairs with the largest Spn
    drop.val <- sort(sr.sub$Spn[!is.na(sr.sub$logRpS)],decreasing = TRUE)[2]
    sr.sub <- sr.sub %>% dplyr::filter(Spn < drop.val)


  #print(paste("n.obs =",sum(!is.na(sr.sub$Spn) & !is.na(sr.sub$logRpS) )))
  bm.tmp <- calcDetRickerBM(sr_obj = sr.sub,min.obs=min.obs)
  #print(bm.tmp)
  drop2.bm.det.store[stk,] <- bm.tmp

}

drop2.bm.det.store <- drop2.bm.det.store %>% rownames_to_column(var="Stock")
if(trace){print(head(drop2.bm.det.store))}



bm.list <- list(BaseCase = bm.det.store,
                Retro= retro.bm.det.store,
                RevRetro = revretro.bm.det.store,
                Jack = jack.bm.det.store,
                Drop2 = drop2.bm.det.store)



# calculate the perc differences to base case

# Jackknife
jack.max <- left_join(bm.list$BaseCase %>% select(Stock),
                      bm.list$Jack %>% select(-DropYr, -n_obs) %>% group_by(Stock) %>% summarize_all(list(max=max),na.rm=TRUE),
                      by="Stock") # need to merge it with base case to get full list of stocks, with NA for those not in jackknife output
names(jack.max) <- gsub("_max","",names(jack.max))

jack.min <- left_join(bm.list$BaseCase %>% select(Stock),
                      bm.list$Jack %>% select(-DropYr, -n_obs) %>% group_by(Stock) %>% summarize_all(list(min=min),na.rm=TRUE),
                      by="Stock")
names(jack.min) <- gsub("_min","",names(jack.min))

jack.max.effect <- cbind(Stock = bm.list$BaseCase[,1],
                         round((jack.max[,-1] - bm.list$BaseCase[,-c(1,2)])/bm.list$BaseCase[,-c(1,2)]*100))

jack.min.effect <- cbind(Stock = bm.list$BaseCase[,1],
                         round((jack.min[,-1] - bm.list$BaseCase[,-c(1,2)])/bm.list$BaseCase[,-c(1,2)]*100))


# Retro

retro.max <- left_join(bm.list$BaseCase %>% select(Stock),
                       bm.list$Retro %>% select(-UpToYr, -n_obs) %>% group_by(Stock) %>% summarize_all(list(max=max),na.rm=TRUE),
                       by="Stock") # need to merge it with base case to get full list if stocks, with NA for those not in retro output
names(retro.max) <- gsub("_max","",names(retro.max))

retro.min <- left_join(bm.list$BaseCase %>% select(Stock),
                       bm.list$Retro %>% select(-UpToYr, -n_obs) %>% group_by(Stock) %>% summarize_all(list(min=min),na.rm=TRUE),
                       by="Stock") # need to merge it with base case to get full list if stocks, with NA for those not in retro output
names(retro.min) <- gsub("_min","",names(retro.min))

retro.max.effect <- cbind(Stock = bm.list$BaseCase[,1],
                          round((retro.max[,-1] - bm.list$BaseCase[,-c(1,2)])/bm.list$BaseCase[,-c(1,2)]*100))

retro.min.effect <- cbind(Stock = bm.list$BaseCase[,1],
                          round((retro.min[,-1] - bm.list$BaseCase[,-c(1,2)])/bm.list$BaseCase[,-c(1,2)]*100))


# Reverse Retro

# calculate the % change for largest and smallest reverse retrospective outputs

revretro.max <- left_join(bm.list$BaseCase %>% select(Stock),
                          bm.list$RevRetro %>% select(-SinceYr, -n_obs) %>% group_by(Stock) %>% summarize_all(list(max=max),na.rm=TRUE),
                          by="Stock") # need to merge it with base case to get full list if stocks, with NA for those not in retro output
names(revretro.max) <- gsub("_max","",names(revretro.max))

revretro.min <- left_join(bm.list$BaseCase %>% select(Stock),
                          bm.list$RevRetro %>% select(-SinceYr, -n_obs) %>% group_by(Stock) %>% summarize_all(list(min=min),na.rm=TRUE),
                          by="Stock") # need to merge it with base case to get full list if stocks, with NA for those not in retro output
names(revretro.min) <- gsub("_min","",names(revretro.min))



revretro.max.effect <- cbind(Stock = bm.list$BaseCase[,1],
                             round((revretro.max[,-1] - bm.list$BaseCase[,-c(1,2)])/bm.list$BaseCase[,-c(1,2)]*100))

revretro.min.effect <- cbind(Stock = bm.list$BaseCase[,1],
                             round((revretro.min[,-1] - bm.list$BaseCase[,-c(1,2)])/bm.list$BaseCase[,-c(1,2)]*100))



diff.list <- list(
  RetroPercDiffMin = retro.min.effect,
  RetroPercDiffMax = retro.max.effect,
  RevRetroPercDiffMin = revretro.min.effect,
  RevRetroPercDiffMax = revretro.max.effect,
  JackPercDiffMin = jack.min.effect,
  JackPercDiffMax = jack.max.effect,
  Drop2PercDiff=  cbind(Stock = bm.list$BaseCase[,1],
                        round((bm.list$Drop2[,-c(1,2)] - bm.list$BaseCase[,-c(1,2)])/bm.list$BaseCase[,-c(1,2)]*100))
)




out.list <- list(BM = bm.list, PercDiff = diff.list)

return(out.list)

}