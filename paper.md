---
title: 'NFLSimulatoR: An R Package for Simulating Plays and Drives in the NFL'
tags:
  - R
  - NFL
authors:
  - name: Ryan Elmore
    orcid: 0000-0002-0092-4532
  - name: Will Palmquist
    orcid: 0000-0002-6100-0923
  - name: Ben Williams
    orcid: 0000-0001-8474-5066
affiliations:
 - name: Department of Business Information and Analytics, Daniels College of Business, University of Denver
citation_author: Elmore et al.
date: 10 November 2020
year: 2020
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

NFLSimulatoR (version 0.1.0) is an R package [@r] that enables the simulation 
and evaluation of game-play strategies in the National Football League (NFL). 
The package itself is inspired by a blog post by Mike Lopez, currently the 
Director of Data and Analytics at the NFL [@statsbylopez].

NFLSimulatoR relies on NFL play-by-play data available via the NFL's Application Programming Interface, or API. This data is accessible within R using the nflscrapR R package [@scrapr] or by downloading it directly from the nflscrapR-data website [@yurko]. The NFLSimulatoR package includes a function `download_nflscrapR_data()` for downloading regular-, pre-, or post-season NFL play-by-play data directly from the nflscrapR-data website for the years 2009 - 2019 within an R session. With the requisite play-by-play data, NFLSimulatoR takes a statistically rigorous approach to simulate various NFL strategies: random sampling of plays. By restricting the sampling of plays to match a desired strategy, one can examine the benefits or drawbacks of various decisions made in the NFL. The package includes two strategies: (1) *going for it on fourth down* and (2) *whether to run or pass on an offensive play*. Fourth down decision making is well-studied [@yam2019lost; @romer2006firms; @fdbot] and is included in the package due to its popularity. There has been little academic study of the decision to pass or rush the football, however, and is included for its novelty. Both strategies, and functions that support them, illustrate how a user can contribute new strategies to future versions of the R package.

The NFLSimulatoR package will be available on CRAN and the latest developmental version is available on github. Additional package details related to issues, recent changes, etc. can be found at the [NFLSimulatoR website](http://datacolorado.com/NFLSimulatoR). 

# Statement of Need

To date, there is no systematic or codified standard for simulating drives and
evaluating strategies using the publicly available NFL play-by-play data. This 
package aims to fill that need. Specifically, we provide a unified approach to 
strategy evaluation along with auxilliary functions in order to facilitate this
goal. A main goal of the NFLSimulatoR package is to offer analysts and researchers 
a platform to examine any strategy of interest. The package framework is such that 
one can create new strategies to investigate. The inclusion of the two aforementioned 
strategies offers users a guide for building their own strategies. Our desire is for
the wider sports analytics community to use NFLSimulatoR, extend and refine this intial
work, and, perhaps most important, study other strategies in a analytically sound 
manner. The ultimate goal in this community-enabled endeavor is to improve decision 
making in the NFL. When someone creates a new strategy they are highly encouraged to 
contribute to the package on Github. Additional contributions, questions, or issues 
are gladly accepted as [tickets](https://github.com/rtelmore/NFLSimulatoR/issues) 
on Github.


# Acknowledgements

We would like to thank Mike Lopez for making his original idea public, the NFL 
for their excellent API, and, of course, the nflscrapR team for exposing the
NFL data to the R community through an excellent R package.

# References
