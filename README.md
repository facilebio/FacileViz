
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FacileViz

<!-- badges: start -->

[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
Experimental](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!--
[![Travis build status](https://travis-ci.org/facilebio/FacileViz.svg?branch=master)](https://travis-ci.org/facilebio/FacileViz)
[![Codecov test coverage](https://codecov.io/gh/facilebio/FacileViz/branch/master/graph/badge.svg)](https://codecov.io/gh/facilebio/FacileViz?branch=master)
--> <!-- badges: end -->

The purpose of this package is to provide helper functions to the
facile.bio ecosystem that produce javascript-based visualizations.

This should be considered an “internal” package and is not meant to be a
general and reusable plotting package. Currently it uses plotly
internally for the final plotting construct.

Useful functions found here:

  - `fscatterplot` scatterplots with faceting
  - `fboxplot` tried to implement grouped boxplots while supporting
    hover/jitter, we do this by leveraging the faceting approach
  - `with_aesthetics`: Maps columns in a data.frame to aesthetics. We
    should rely more heavily on the scales package.
