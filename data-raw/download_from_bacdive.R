## code to prepare `bacdive_phenotypes` and `bacdive_susceptibility` datasets
## also saves a large type strain list for later use if needed

library(BacDive)
library(tidyverse)
library(usethis)

credentials = Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))

bacdive <- open_bacdive(credentials[[1L]], credentials[[2L]])

full_list <- read_csv("data-raw/full_list_of_bacteria_from_bacdive_20211027.csv", skip = 2, show_col_types = FALSE)
# this was downloaded here: https://bacdive.dsmz.de/advsearch?filter-group%5B1%5D%5Bgroup-condition%5D=OR&filter-group%5B1%5D%5Bfilters%5D%5B1%5D%5Bfield%5D=Domain&filter-group%5B1%5D%5Bfilters%5D%5B1%5D%5Bfield-option%5D=contains&filter-group%5B1%5D%5Bfilters%5D%5B1%5D%5Bfield-value%5D=Bacteria&filter-group%5B1%5D%5Bfilters%5D%5B1%5D%5Bfield-validation%5D=strains-domain-1

type_strains <- full_list %>%
  filter(is_type_strain_header == 1)

# If you try to download more than 100 you get warnings and errors like this one:
# Warning message:
#   In download_json_with_retry(url, object) :
#   [API] title: BacDive API error; code: 400; message: You exceeded the maximum amount of 100 ids per request.

list_holder <- list()

i = 1

if (file.exists("data-raw/type_strain_large_list.rda")) {
  list_holder <- read_rds("data-raw/type_strain_large_list.rda")
} else {
  for (i in seq(1, length(type_strains$ID), 100)) {
    if (i + 99 > length(type_strains$ID)) {
      k = length(type_strains$ID) #since the last block would include NA's otherwise
    } else {
      k = i + 99
    }
    temp_list <- fetch(bacdive, type_strains$ID[i:k])
    list_holder <- c(list_holder, temp_list$results)
  }
  write_rds(list_holder, "data-raw/type_strain_large_list.rda") #so if we re-run the script we don't have to keep GETing data from bacdive
}

# now we want to extract the important bits (gram stain, oxygen tolerance, abx sensitivity / resistance, and urease activity)
# splitting into bacdive_phenotypes and bacdive_susceptibility for compatibility with abxidx

bacdive_phenotypes <- tibble()
for (i in 1:length(list_holder)) {
  bacdive_phenotypes[i,"ID"] <- list_holder[[i]]$General$`BacDive-ID`
  bacdive_phenotypes[i,"taxon"] <- list_holder[[i]]$`Name and taxonomic classification`$species
  bacdive_phenotypes[i,"rank"] <- "Species"
  if (is.null(list_holder[[i]]$Morphology$`cell morphology`$`gram stain`)) {
    bacdive_phenotypes[i,"gram_stain"] <- NA
  } else {
  bacdive_phenotypes[i,"gram_stain"] <- list_holder[[i]]$Morphology$`cell morphology`$`gram stain`
  }
  if (is.null(list_holder[[i]]$`Physiology and metabolism`$`oxygen tolerance`$`oxygen tolerance`)) {
    bacdive_phenotypes[i,"aerobic_status"] <- NA
  } else {
    bacdive_phenotypes[i,"aerobic_status"] <- list_holder[[i]]$`Physiology and metabolism`$`oxygen tolerance`$`oxygen tolerance`
  }
}

# If we want to exactly match the values for abxidx
# bacdive_phenotypes <- bacdive_phenotypes %>%
#   mutate(gram_stain = case_when(gram_stain %in% "positive" ~ "Gram-positive",
#                                 gram_stain %in% "negative" ~ "Gram-negative"))
# this turns all the "variable" values for gram_stain into NA's

usethis::use_data(bacdive_phenotypes, overwrite = TRUE)

bacdive_susceptibility <- tibble()
for (i in 1:length(list_holder)) {
  abx_df <-
    list_holder[[i]]$`Physiology and metabolism`$`antibiotic resistance`
  my_df <- tibble()

  if (!is.null(abx_df)) {
    if (!is.null(names(abx_df))) {
      #for when you have a single antibiotic entry since a longer list will not have a `names` attribute
      my_df[1, "ID"] <- list_holder[[i]]$General$`BacDive-ID`
      my_df[1, "taxon"] <-
        list_holder[[i]]$`Name and taxonomic classification`$species
      my_df[1, "rank"] <- "Species"
      my_df[1, "antibiotic"] <- abx_df$metabolite
      resistant <- abx_df$`is resistant`
      #there is probably a smarter way to do this
      sensitive <- abx_df$`is sensitive`
      if (!is.null(resistant)) {
        if (resistant %in% "yes") {
          #you can not test both at once because R will error
          my_df[1, "value"] <- "resistant"
        }
      }
      if (!is.null(sensitive)) {
        if (sensitive %in% "yes") {
          my_df[1, "value"] <- "susceptible"
        }
      }
    } else {
      for (j in 1:length(abx_df)) {
        my_df[j, "ID"] <- list_holder[[i]]$General$`BacDive-ID`
        my_df[j, "taxon"] <-
          list_holder[[i]]$`Name and taxonomic classification`$species
        my_df[j, "rank"] <- "Species"
        my_df[j, "antibiotic"] <- abx_df[[j]]$metabolite
        resistant <- abx_df[[j]]$`is resistant`
        sensitive <- abx_df[[j]]$`is sensitive`
        if (!is.null(resistant)) {
          if (resistant %in% "yes") {
            #you can not test both at once because R will error
            my_df[j, "value"] <- "resistant"
          }
        }
        if (!is.null(sensitive)) {
          if (sensitive %in% "yes") {
            my_df[j, "value"] <- "susceptible"
          }
        }
      }
    }
  }

  bacdive_susceptibility <- bind_rows(bacdive_susceptibility, my_df)

}

usethis::use_data(bacdive_susceptibility, overwrite = TRUE)

#pro-tip to search for a given bacdive ID: list.which(list_holder, General$`BacDive-ID` == "141146")

