# RapidRicker

R Package to run spawner-recruit data quality checks and test the sensitivity of standard biological benchmarks.

Development Team: Gottfried Pestal, Charmaine Carr-Harris, Steven Cox-Rogers

**Important Note:** This package is under development. Functions may change rapidly and substantially. Do not use these if you are not part of the development team!


For now *RapidRicker* includes 3 functions:

*  *checkSRData()*: calculates a set of diagnostics for the spawner-recruit data. Some apply to the whole series (e.g. the contrast is spawner estimates), while others flag individual observations (e.g. R/S above user-specified plausible upper bound, pointing to a potential data error in either R or S).
* *calcDetRickerBM()*: fits a simple linear regression to ```log(R/S) ~ S``` and calculates standard biological
benchmarks (Smsy, Smax, Seq, Umsy)
* *testDetRickerBM()*: runs jackknife (drop 1), retrospective (gradually add recent data) or reverse retrospective (gradually drop earlier data) variations of the simple Ricker fit.

To get up and running with some examples, follow the *Quick Start* steps below.



## Quick Start

### Install

To install this package directly from github, use

```
install.packages("devtools") # Install the devtools package
library(devtools) # Load the devtools package.
install_github("SOLV-Code/RapidRicker", 
				dependencies = TRUE,
                build_vignettes = FALSE)
library(RapidRicker)		

# check the built in data set
?SR_Sample # opens help file
head(SR_Sample) # shows the first few rows

# check the function help files

?checkSRData
?calcDetRickerBM
?testDetRickerBM

		
```




### Worked Examples


```
library(RapidRicker)







```



