# NFLSimulatoR 
Simulating plays and drives in the NFL using [nflscrapR](https://ryurko.github.io/nflscrapR-data/) or [nflfastR](https://github.com/guga31bb/nflfastR-data) data.

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/NFLSimulatoR)](https://cran.r-project.org/package=NFLSimulatoR)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/NFLSimulatoR)](https://www.r-pkg.org/pkg/NFLSimulatoR)
[![R build status](https://github.com/rtelmore/NFLSimulatoR/workflows/R-CMD-check/badge.svg)](https://github.com/rtelmore/NFLSimulatoR/actions/)

  <!-- badges: end -->
## Website  
The NFLSimulatoR package website is hosted at [https://datacolorado.com/r/NFLSimulatoR](https://datacolorado.com/r/NFLSimulatoR/).

### Overview
The intent of NFLSimulatoR (version 0.1.0) is to enable the simulation of plays/drives and evaluate game-play strategies in the National Football League (NFL). Built-in strategies include going for it on fourth down and varying the proportion of passing/rushing plays during a drive. The user should be familiar with nflscrapR data before trying to write his/her own strategies. The package itself is inspired by a 
[blog post by Mike Lopez](https://statsbylopez.netlify.app/post/resampling-nfl-drives/), currently the Director of Data and Analytics at the NFL.

### Getting Started

If you are just getting started with NFLSimulatoR, we recommend reading an 
upcoming paper related to use cases.

### Installation Instructions

From CRAN (v0.2.0):
```r
install.packages("NFLSimulatoR")
```

The latest developmental version from GitHub (v0.2.1):
```r
install.packages("remotes")
remotes::install_github("rtelmore/NFLSimulatoR")
```

### Resources

* [Open an issue](https://github.com/rtelmore/NFLSimulatoR/issues/) (GitHub issues for bug reports, feature requests, etc.)

