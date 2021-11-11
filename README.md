
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bacphene

<!-- badges: start -->
<!-- badges: end -->

The goal of bacphene is to generate a table of phenotype characteristics
about your samples. Input is a table of Samples with counts per taxa.

The package utilizes the official [BacDive R
package](https://r-forge.r-project.org/R/?group_id=1573) that accesses
bacterial phenotype data from [bacdive.org](https://bacdive.dsmz.de).

## Installation

You can install bacphene from the [github
repo](https://github.com/scottdaniel/bacphene). You also need to install
the BacDive R package (if you want the full data - see below).

``` r
library(devtools)
install_github(repo = "scottdaniel/bacphene")
install.packages("BacDive", repos="http://R-Forge.R-project.org")
```

After this, you may register at bacdive.org
[here](https://api.bacdive.dsmz.de/login).

If you do not register, you will not be able to download the full
information on strains in BacDive. Instead, what you will get is access
to three data-frames: `bacdive_phenotypes`, `bacdive_susceptibility`,
and `bacdive_enzymes`. Which look like this:

``` r
library(bacphene)
head(bacdive_phenotypes)
#>       ID                       taxon    rank gram_stain aerobic_status
#> 1 159837       Abyssibacter profundi Species   negative         aerobe
#> 2 159709      Acidicapsa dinghuensis Species   negative         aerobe
#> 3 159652 Abditibacterium utsteinense Species       <NA>           <NA>
#> 4 157972      Acetivibrio mesophilus Species       <NA>           <NA>
#> 5 156534       Achromobacter anxifer Species       <NA>         aerobe
#> 6 156528  Achromobacter aegrifaciens Species       <NA>         aerobe
```

``` r
head(bacdive_susceptibility)
#>       ID                  taxon    rank      antibiotic       value
#> 1 159709 Acidicapsa dinghuensis Species        Amikacin   resistant
#> 2 159709 Acidicapsa dinghuensis Species chloramphenicol susceptible
#> 3 159709 Acidicapsa dinghuensis Species   ciprofloxacin susceptible
#> 4 159709 Acidicapsa dinghuensis Species    erythromycin susceptible
#> 5 159709 Acidicapsa dinghuensis Species      gentamicin   resistant
#> 6 159709 Acidicapsa dinghuensis Species       kanamycin   resistant
```

``` r
head(bacdive_enzymes)
#>       ID                 taxon    rank activity               value       ec
#> 1 159837 Abyssibacter profundi Species        -    acid phosphatase  3.1.3.2
#> 2 159837 Abyssibacter profundi Species        - alpha-galactosidase 3.2.1.22
#> 3 159837 Abyssibacter profundi Species        -   alpha-glucosidase 3.2.1.20
#> 4 159837 Abyssibacter profundi Species        -   alpha-mannosidase 3.2.1.24
#> 5 159837 Abyssibacter profundi Species        -    beta-glucosidase 3.2.1.21
#> 6 159837 Abyssibacter profundi Species        -  beta-glucuronidase 3.2.1.31
#>                      doi
#> 1 10.1099/ijsem.0.002999
#> 2 10.1099/ijsem.0.002999
#> 3 10.1099/ijsem.0.002999
#> 4 10.1099/ijsem.0.002999
#> 5 10.1099/ijsem.0.002999
#> 6 10.1099/ijsem.0.002999
```

### Recommended

Edit your $HOME/.Renviron file (you can open in R with
`usethis::edit_r_environ()`) and add your bacdive credentials like so:

    DSMZ_API_USER=your_email@something.com
    DSMZ_API_PASSWORD=your_password

# References

When using BacDive for research please consider citing the following
paper:

BacDive in 2019: bacterial phenotypic data for High-throughput
biodiversity analysis. Reimer, L. C., Vetcininova, A., Sardà Carbasse,
J., Söhngen, C., Gleim, D., Ebeling, C., Overmann, J. [Nucleic Acids
Research; database issue
2019](https://academic.oup.com/nar/article/47/D1/D631/5106998).

To cite this package enter `citation("bacphene")` in your R console.
