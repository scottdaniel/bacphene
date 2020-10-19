
<!-- README.md is generated from README.Rmd. Please edit that file -->
bacphene
========

<!-- badges: start -->
<!-- badges: end -->
The goal of bacphene is to import bacterial phenotype data from bacdive.org into R.

Installation
------------

You can install bacphene from [github](https://github.com/scottdaniel/bacphene) with:

``` r
library(devtools)
install_github(repo = "scottdaniel/bacphene")
```

After this, you MUST register at bacdive.org [here](https://bacdive.dsmz.de/api/bacdive/registration/register/). Otherwise, you will not be able to access the API and get phenotype data.

Example
-------

This will only work once you have gotten an account at bacdive.org:


    library(bacphene)
    strain_list <- getStrains(genus = 'Bacteroides', species = 'xylanisolvens', userpassword = 'me@example.com:mypassword')
    phenotype_data <- getStrainData(jsondata = strain_list, selection = 1, userpassword = 'me@example.com:mypassword')
