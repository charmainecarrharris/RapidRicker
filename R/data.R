#' Sample Spawner Recruit Data
#'
#' A dataset containing spawners, recruits, spawner estimate expansions, and age comp by brood year for mutiple stocks.
#'
#' @format A data frame with  4 required and some optional variables:
#' \describe{
#'   \item{Stock}{ stock name}
#'   \item{Year}{brood year}
#'   \item{Spn}{spawner estimate}
#'   \item{Rec}{recruitment estimate}
#'   \item{SpnExp}{OPTIONAL: expansion factor applied to get the spawner estimate}
#'   \item{RecAge#}{proportion each age class contributes to the total recruits}
#'   \item{}{}
#'   ...
#' }
#' @source Dummy Data
"SR_Sample"


#' Upper and Lower thresholds for flagging SR data issues
#'
#' A dataset containing observed recruits/spawner and various environmental covariates used in the 2019 Forecast for 
#' Fraser River Sockeye Salmon (REF).IMPORTANT NOTE: Time series are not lined up based on plausible mechanism (e.g. sea surface temp may affect overall productivity during early ocean entry, but RpEff values are by brood year. Stock-specific offsets are required to align values properly. INCLUDE LIST!
#'
#' @format A data frame with  3 required variables and optional context information (e.g. Reference):
#' \describe{
#'   \item{Label}{Labels identifyin the criteria. Have to match the criteria used in the function.}
#'   \item{Lower}{Any value lower than this is flagged.}
#'   \item{Upper}{Any value higher than this is flagged.}
#'   \item{}{}
#'   ...
#' }
#' @source Include REF
"flags_default"





