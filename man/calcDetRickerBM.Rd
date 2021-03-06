% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FUNCTION_calcDetRickerBM.R
\name{calcDetRickerBM}
\alias{calcDetRickerBM}
\title{calcDetRickerBM}
\usage{
calcDetRickerBM(sr_obj, min.obs = 15)
}
\arguments{
\item{sr_obj}{a data frame with Year and Spn, logRpS (Data for 1 Stock!). Other variables can be there but are not used (RpS, Qual, ExpF etc)}

\item{min.obs}{min number of S-R pairs needed to fit a model}
}
\description{
This function calculates Ricker Model parameters for spawner-recruit data using a simple linear regression of log(R/S) ~ S.  Note that these are simple deterministic model fits intended for rapid testing of input data!
Also calculates standard biological benchmarks (Smsy, Seq, Smax, Umsy). Benchmark calculations were adapted from BUGS code used in Miller & Pestal (2020), available \href{https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_035-eng.pdf}{here}.
Two versions for some BM are produced: "_h" = Hilborn Proxy (\href{https://cdnsciencepub.com/doi/pdf/10.1139/f85-230}{Hilborn 1985}) and "_p" = Peterman Proxy" (\href{https://cdnsciencepub.com/doi/pdf/10.1139/f99-204}{Peterman et al. 2000}).
}
\examples{
ricker.bm <- calcDetRickerBM(SR_Sample[SR_Sample$Stock == "Stock1",],min.obs = 10)
print(ricker.bm)
}
\keyword{Ricker}
\keyword{Seq,}
\keyword{Smax,}
\keyword{Smsy,}
\keyword{Umsy}
\keyword{fit,}
