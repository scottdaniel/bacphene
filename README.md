
<!-- README.md is generated from README.Rmd. Please edit that file -->
bacphene
========

<!-- badges: start -->
<!-- badges: end -->
The goal of bacphene is to import bacterial phenotype data from [bacdive.org](https://bacdive.dsmz.de/) into R.

Installation
------------

You can install bacphene from [github](https://github.com/scottdaniel/bacphene) with:

``` r
library(devtools)
install_github(repo = "scottdaniel/bacphene")
```

After this, you MUST register at bacdive.org [here](https://bacdive.dsmz.de/api/bacdive/registration/register/). Otherwise, you will not be able to access the API and get phenotype data.

Examples
--------

This will only work once you have gotten an account at bacdive.org:

``` r

library(bacphene)

user <- "usually_your_email"
passwd <- "a_good_password"

strain_list <- getStrains(page = 1, 
  genus = 'Bacteroides', 
  species = 'xylanisolvens', 
  userpassword = paste0(user,':',passwd))
  
phenotypes_list <- getStrainData(strain_list,
    selection=1,
    userpassword = paste0(user,':',passwd))
```

However, to facilitate demonstration, I have run the previous codeblock and written the results lists to flat files here: ./examples/Bacteroides\_xylanisolvens.RData

Here we can see that there are (as of 10/19/2020) three strains in the bacdive.org database with the species designation of Bacteroides xylanisolvens:

``` r

library(bacphene)

load("./examples/Bacteroides_xylanisolvens.RData")

strain_list$results
#> [[1]]
#> [[1]]$url
#> [1] "https://bacdive.dsmz.de/api/bacdive/bacdive_id/1626/"
#> 
#> 
#> [[2]]
#> [[2]]$url
#> [1] "https://bacdive.dsmz.de/api/bacdive/bacdive_id/131167/"
#> 
#> 
#> [[3]]
#> [[3]]$url
#> [1] "https://bacdive.dsmz.de/api/bacdive/bacdive_id/153256/"
```

Selecting the first strain (which you can set in [getStrainData](https://github.com/scottdaniel/bacphene/blob/master/R/bacPhene.R) with 'selection=1') we get the following information:

``` r

names(phenotypes_list)
#> [1] "taxonomy_name"                        
#> [2] "morphology_physiology"                
#> [3] "culture_growth_condition"             
#> [4] "environment_sampling_isolation_source"
#> [5] "application_interaction"              
#> [6] "molecular_biology"                    
#> [7] "strain_availability"                  
#> [8] "references"
```

We can get the gram-stain of the strain. Note that there may be multiple references for this information.

``` r

gramStain(phenotypes_list, reference = 1)
#> [1] "negative"
```

Additionally, there is a function to get the oxygen tolerance information. Again, there may be multiple references (and Caution: this data is not standardized, see below)

``` r

oxygenTolerance(phenotypes_list, reference = 1)
#> [1] "anaerobe"

oxygenTolerance(phenotypes_list, reference = 3)
#> [1] "obligate anaerobe"
```

In bacdive.org there is also information on antibiotic sensistivey / resistance. We can query that with two functions. We will have to load different examples here because Bacteroides xylanisolvens does not have antibiotic data.

``` r

load("examples/Pseudomonas_tarimensis.RData")

abxResistant(phenotypes_list, abx = "gentamicin")
#> [1] TRUE
```

``` r

load("examples/Actibacterium_pelagium.RData")

abxSensitive(phenotypes_list, abx = "vancomycin")
#> [1] TRUE
```
