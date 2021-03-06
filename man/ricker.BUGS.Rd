% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MODEL_ricker_BUGS.R
\name{ricker.BUGS}
\alias{ricker.BUGS}
\title{ricker.BUGS}
\usage{
ricker.BUGS()
}
\description{
This function is the BUGS version of the Ricker model fit and associated benchmarks. It is used inside
of calcMCMCRickerBM() which passes it to  doRJAGS(), which is a wrapper for the jags() function from the R2jags package. For details such as model form and variable definitions, refer to \href{https://github.com/SOLV-Code/RapidRicker/wiki/MCMC-Using-R2Jags}{this wiki page}.
This function has no arguments, because the jags call sets up the inputs. The code is adapted from code originally developed by Catherine Michielsens, Sue Grant, Bronwyn MacDonald, Carrie Holt and others. Examples of previous use are \href{https://www.dfo-mpo.gc.ca/csas-sccs/publications/resdocs-docrech/2015/2015_048-eng.html}{Taku Coho} and \href{https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_035-eng.html}{Taku Sockeye}.
}
