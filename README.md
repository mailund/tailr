
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tailr – Tail recursion optimisations for R programming

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--02--07-orange.svg)](/commits/master)
[![lifecycle](http://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

[![Travis build
status](https://travis-ci.org/mailund/tailr.svg?branch=master)](https://travis-ci.org/mailund/tailr)
[![Appveyor build
status](https://ci.appveyor.com/api/projects/status/1d36yh8klursko82/branch/master?svg=true)](https://ci.appveyor.com/project/mailund/tailr/branch/master)
[![Coverage
status](https://codecov.io/gh/mailund/tailr/branch/master/graph/badge.svg)](https://codecov.io/github/mailund/tailr?branch=master)
[![Coverage
status](http://coveralls.io/repos/github/mailund/tailr/badge.svg?branch=master)](https://coveralls.io/github/mailund/tailr?branch=master)

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.1-blue.svg)](https://cran.r-project.org/)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tailr)](https://cran.r-project.org/package=tailr)
[![packageversion](https://img.shields.io/badge/Package%20version-0.0.0.9000-orange.svg?style=flat-square)](commits/master)

-----

The goal of tailr is to automatically transform tail-recursive functions
into loops or trampolines.

## Installation

You can install tailr from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("mailund/tailr")
```
