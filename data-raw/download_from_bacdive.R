## code to prepare `bacdive_typestrains` dataset

library(BacDive)
library(tidyverse)
library(usethis)

credentials = Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))

bacdive <- open_bacdive(credentials[[1L]], credentials[[2L]])

full_list <- read_csv("full_list_of_bacteria_from_bacdive_20211027.csv", skip = 2)

type_strains <- full_list %>%
  filter(is_type_strain_header == 1)

# If you try to download more than 100 you get warnings and errors like this one:
# Warning message:
#   In download_json_with_retry(url, object) :
#   [API] title: BacDive API error; code: 400; message: You exceeded the maximum amount of 100 ids per request.

list_holder <- list()

i = 1

for (i in seq(1, length(type_strains$ID), 100)) {

  if (i+99 > length(type_strains$ID)) {
    k = length(type_strains$ID) #since the last block would include NA's otherwise
  } else {
    k = i+99
  }

  temp_list <-fetch(bacdive, type_strains$ID[i:k])

  list_holder <- c(list_holder, temp_list$results)

}

write_rds(list_holder, "type_strain_large_list.RData")

# now we want to extract the important bits (gram stain, oxygen tolerance, abx sensitivity / resistance, and urease activity)

bacdive_typestrains <- tibble()

i = 1

for (i in 1:length(list_holder)) {

  bacdive_typestrains[i,"ID"] <- list_holder[[i]]$General$`BacDive-ID`
  bacdive_typestrains[i,"species"] <- list_holder[[i]]$`Name and taxonomic classification`$species
  if (is.null(list_holder[[i]]$Morphology$`cell morphology`$`gram stain`)) {
    bacdive_typestrains[i,"gram_stain"] <- NA
  } else {
  bacdive_typestrains[i,"gram_stain"] <- list_holder[[i]]$Morphology$`cell morphology`$`gram stain`
  }

}

usethis::use_data(bacdive_typestrains, overwrite = TRUE)
