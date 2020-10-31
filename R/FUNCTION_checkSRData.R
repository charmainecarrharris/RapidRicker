# NOTE: Helper Functions at the end


checkSRData <- function(sr.df,flags.df){
# designed for 1 stock at a time!!
# sr.df is a data frame with Year, Spn, Rec, and optionally SpnExp RecAge#,
# Note: only 1 stock input!
# flags.df is a data frame with Label, Lower, Upper. Lower and Upper define the triggers for values to be flagged.
# labels have to match the criteria used below
# Note: if only one direction applies, set the other to NA (e.g. [10,NA] means flag values less than 10).
# Metric calculations are explained throughout the fn code.




# Low contrast in Spn Data -------------------------------------
# calc: max(spn)/min(spn)

flag.label <- "Contr"
metric.in <- max(sr.df$Spn,na.rm=TRUE)/min(sr.df$Spn,na.rm=TRUE)

flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                      flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                       metric.val = metric.in,
                       round.val = 2)

flags.df[flags.df$Label == flag.label, "MetricVal"] <- flag.tmp$MetricVal
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged




# Insufficient SR data -------------------------------------
# calc: num(BY) with spn and rec
sr.idx <- !is.na(sr.df$Spn) & !is.na(sr.df$Rec)
metric.in  <- sum(sr.idx)
flag.label <- "NumObs"

flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                      flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                      metric.val = metric.in,
                      round.val = 2)

flags.df[flags.df$Label == flag.label, "MetricVal"] <- flag.tmp$MetricVal
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged




# Missing large Spn -------------------------------------
# calc: Max(spn)/(Max(spn in SR)
metric.in<- max(sr.df$Spn,na.rm = TRUE) / max(sr.df$Spn[sr.idx],na.rm = TRUE)
flag.label <- "LgSpn"

flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                      flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                      metric.val = metric.in,
                      round.val = 2)

flags.df[flags.df$Label == flag.label, "MetricVal"] <- flag.tmp$MetricVal
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged



# Missing large Rec -------------------------------------
# calc: Max(Rec)/(Max(Rec in SR)
metric.in <- max(sr.df$Rec,na.rm = TRUE) / max(sr.df$Rec[sr.idx],na.rm = TRUE)
flag.label <- "LgRec"

flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                      flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                      metric.val = metric.in,
                      round.val = 2)

flags.df[flags.df$Label == flag.label, "MetricVal"] <- flag.tmp$MetricVal
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged




# Missing small Spn -------------------------------------
# calc: Min(spn)/(Min(spn in SR)
metric.in<- min(sr.df$Spn,na.rm = TRUE) / min(sr.df$Spn[sr.idx],na.rm = TRUE)
flag.label <- "SmSpn"

flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                      flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                      metric.val = metric.in,
                      round.val = 2)

flags.df[flags.df$Label == flag.label, "MetricVal"] <- flag.tmp$MetricVal
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged


# Missing small Rec -------------------------------------
# calc: Min(rec)/(Min(rec in SR)
metric.in<- min(sr.df$Rec,na.rm = TRUE) / min(sr.df$Rec[sr.idx],na.rm = TRUE)
flag.label <- "SmRec"

flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                      flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                      metric.val = metric.in,
                      round.val = 2)

flags.df[flags.df$Label == flag.label, "MetricVal"] <- flag.tmp$MetricVal
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged




# Large Spn Expansion Factor (median) --------------------------------------------
# calc: Median(ExpSpn)

flag.label <- "LgExp"

if("SpnExp" %in% names(sr.df)){

metric.in <- median(sr.df$SpnExp, na.rm = TRUE)


flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                      flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                      metric.val = metric.in,
                      round.val = 2)

flags.df[flags.df$Label == flag.label, "MetricVal"] <- flag.tmp$MetricVal
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged
}

if(!("SpnExp" %in% names(sr.df))){
  flags.df[flags.df$Label == flag.label, "MetricVal"] <- NA
  flags.df[flags.df$Label == flag.label, "Flagged"] <- NA
}




# Unstable age comp --------------------------------------------
# calc: MaxDiffByAge

flag.label <- "VarAge"

if(sum(grepl("RecAge",names(sr.df)))>=2) { # don only of have at least 2 age classes in the data


  age.sub <- sr.df[,grepl("RecAge", names(sr.df))]

  metric.pre <- lapply(age.sub,range.calc)

  metric.in <- max(unlist(metric.pre))

  flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                        flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                        metric.val = metric.in,
                        round.val = 2)

  flags.df[flags.df$Label == flag.label, "MetricVal"] <- flag.tmp$MetricVal
  flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged
}

if(sum(grepl("RecAge",names(sr.df)))<2){
  flags.df[flags.df$Label == flag.label, "MetricVal"] <- NA
  flags.df[flags.df$Label == flag.label, "Flagged"] <- NA
}


# Unusual Spn obs --------------------------------------------
# calc: Spn / Med(Spn)

flag.label <- "OddSpn"
metric.obs <- sr.df$Spn / median(sr.df$Spn, na.rm = TRUE)

flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                        flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                        metric.val = metric.obs,
                        round.val = 2)
flags.df[flags.df$Label == flag.label, "MetricVal"] <- NA
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged
flags.df[flags.df$Label == flag.label, "NumFlagged"] <-sum(flag.tmp$FlaggedObs,na.rm=TRUE)

sr.df[[paste0(flag.label,"Val")]] <- flag.tmp$MetricVal
sr.df[[paste0(flag.label,"Flag")]] <- flag.tmp$FlaggedObs



# Unusual Rec obs --------------------------------------------
# calc: Rec / Med(Rec)

flag.label <- "OddRec"
metric.obs <- sr.df$Rec/ median(sr.df$Rec, na.rm = TRUE)

flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                      flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                      metric.val = metric.obs,
                      round.val = 2)
flags.df[flags.df$Label == flag.label, "MetricVal"] <- NA
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged
flags.df[flags.df$Label == flag.label, "NumFlagged"] <-sum(flag.tmp$FlaggedObs,na.rm=TRUE)

sr.df[[paste0(flag.label,"Val")]] <- flag.tmp$MetricVal
sr.df[[paste0(flag.label,"Flag")]] <- flag.tmp$FlaggedObs



# Unusual Prod obs --------------------------------------------
# calc: R/S (or scale by median R/S? NEED TO DISCUSS
flag.label <- "OddProd"
metric.obs <- (sr.df$Rec/sr.df$Spn) #/ median(sr.df$Rec/sr.df$Spn, na.rm = TRUE)

flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                      flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                      metric.val = metric.obs,
                      round.val = 2)
flags.df[flags.df$Label == flag.label, "MetricVal"] <- NA
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged
flags.df[flags.df$Label == flag.label, "NumFlagged"] <-sum(flag.tmp$FlaggedObs,na.rm=TRUE)

sr.df[[paste0(flag.label,"Val")]] <- flag.tmp$MetricVal
sr.df[[paste0(flag.label,"Flag")]] <- flag.tmp$FlaggedObs


# Unusual Spn Expansions --------------------------------------------
# calc: SpnExp / Med(SpnExp)

flag.label <- "OddExp"
metric.obs <- sr.df$SpnExp   / median(sr.df$SpnExp, na.rm = TRUE)

flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                      flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                      metric.val = metric.obs,
                      round.val = 2)
flags.df[flags.df$Label == flag.label, "MetricVal"] <- NA
flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged
flags.df[flags.df$Label == flag.label, "NumFlagged"] <-sum(flag.tmp$FlaggedObs,na.rm=TRUE)

sr.df[[paste0(flag.label,"Val")]] <- flag.tmp$MetricVal
sr.df[[paste0(flag.label,"Flag")]] <- flag.tmp$FlaggedObs


# Unusual Age Comp --------------------------------------------
# calc: recAge / Med(RecAge) for main age comp
#

flag.label <- "OddAge"

if(sum(grepl("RecAge",names(sr.df)))>=2) { # do only of have at least 2 age classes in the data

  age.sub <- sr.df[,grepl("RecAge", names(sr.df))]
  metric.pre <- lapply(age.sub,function(x){median(x,na.rm =TRUE)})
  age.main <- names(unlist(metric.pre))[unlist(metric.pre) == max(unlist(metric.pre))]

  metric.in <- sr.df[[age.main]] / metric.pre[[age.main]]

  flag.tmp <- flag.calc(flag.lower = flags.df[flags.df$Label == flag.label,"Lower"],
                        flag.upper = flags.df[flags.df$Label == flag.label,"Upper"],
                        metric.val = metric.in,
                        round.val = 2)

  flags.df[flags.df$Label == flag.label, "MetricVal"] <- NA
  flags.df[flags.df$Label == flag.label, "Flagged"] <- flag.tmp$Flagged
  flags.df[flags.df$Label == flag.label, "NumFlagged"] <-sum(flag.tmp$FlaggedObs,na.rm=TRUE)

  sr.df[[paste0(flag.label,"Val")]] <- flag.tmp$MetricVal
  sr.df[[paste0(flag.label,"Flag")]] <- flag.tmp$FlaggedObs
}

if(sum(grepl("RecAge",names(sr.df)))<2){
  flags.df[flags.df$Label == flag.label, "MetricVal"] <- NA
  flags.df[flags.df$Label == flag.label, "Flagged"] <- NA
  flags.df[flags.df$Label == flag.label, "NumFlagged"] <- NA
  sr.df[[paste0(flag.label,"Val")]] <- NA
  sr.df[[paste0(flag.label,"Flag")]] <- NA
}



#-----------------------------------------

main.cols <- flags.df$Scope == "Obs" & !grepl("Age",flags.df$Label)

sr.df[["NumFlagsMain"]]  <- rowSums(sr.df[,paste0(flags.df$Label[main.cols],"Flag")])
sr.df[["NumFlagsAll"]]  <- rowSums(sr.df[,paste0(flags.df$Label[flags.df$Scope == "Obs"],"Flag")])


list.out <- list(Summary = flags.df, Data = sr.df)

} # end checkSRData() function



# HELPER FUNCTIONS

flag.calc <- function(flag.lower, flag.upper,metric.val,round.val = NULL){
# internal subroutine
# metric.val can be a single value or a vector

  # based on https://stackoverflow.com/questions/16822426/dealing-with-true-false-na-and-nan
  flag.out <- any(c(metric.val < flag.lower, metric.val > flag.upper)  %in% TRUE)
  #print(metric.val < flag.lower)
  #print(metric.val > flag.upper)


  if(length(metric.val)>1){flag.vec <- ((metric.val < flag.lower) %in% TRUE) |   ((metric.val > flag.upper) %in% TRUE)  }
  if(length(metric.val)==1){flag.vec <- NA}


  if(!is.null(round.val)){ val.out <- round(metric.val,2) }
  if(is.null(round.val)){ val.out <- metric.val }

  out.list <- list(MetricVal = val.out, Flagged = flag.out, FlaggedObs = flag.vec)
  return(out.list)
}


range.calc <- function(x){
# internal subroutine
  range.out <- max(x,na.rm = TRUE) - min(x,na.rm = TRUE)
  return(range.out)
}


div.by.median <- function(x){
# internal subroutine
  vec.out <- x / median(x,na.rm=TRUE)
}