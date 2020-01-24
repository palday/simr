# simr

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/pitakakariki/simr?branch=master&svg=true)](https://ci.appveyor.com/project/pitakakariki/simr)
[![Build Status](https://travis-ci.org/pitakakariki/simr.svg?branch=master)](https://travis-ci.org/pitakakariki/simr)
[![Coverage Status](https://codecov.io/gh/pitakakariki/simr/branch/master/graph/badge.svg)](https://codecov.io/github/pitakakariki/simr?branch=master)

Power Analysis for Generalised Linear Mixed Models by Simulation.

## Getting Started

A tutorial has been published in [Methods in Ecology and Evolution](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12504/abstract).

This is also currently the citation given by `citation('simr')`, so it's what you should cite.  If you're taking advantages of the experimental features of the simultaneous branch, please also cite this work via Zenodo:  

[![DOI](https://zenodo.org/badge/60388682.svg)](https://zenodo.org/badge/latestdoi/60388682)

Once this has been intergrated into mainstream simr, double citation will no longer be necessary. 

## Installing and Using the `simultaneous` Branch

```r
if(!require(devtools)){
    install.packages("devtools")
    library("devtools")
}

install_github("palday/simr",ref="simultaneous")
library(simr)
?powerSimSimultaneous
?powerSimMultiple
```

## Old RLRsim Binaries

If you get the following error, try reinstalling RLRsim.

    > library(simr)
    Loading required package: lme4
    Loading required package: Matrix
    Error : object ‘sigma’ is not exported by 'namespace:lme4'
    Error: package or namespace load failed for ‘simr’