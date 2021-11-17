#bacdive_morphology

test_that("getMorphology", {

  # structure of list object gotten from bacdive
  # if it changes, it will break the code
  test_list <- readr::read_rds(file = "test_data.rda")

  morphology_df <- getMorphology(list_holder = test_list)

  expect_named(morphology_df, c("@ref", "gram stain", "cell length", "cell width", "cell shape", "motility", "flagellum arrangement", "ID", "taxon", "rank", "doi/url"), ignore.order = T)

})

test_that("getOxygen", {

  test_list <- readr::read_rds(file = "test_data.rda")

  oxygen_df <- getOxygen(list_holder = test_list)

  expect_named(oxygen_df, c("@ref", "oxygen tolerance", "ID", "taxon", "rank", "doi/url"), ignore.order = T)

})

test_that("getPhenotypes", {

  test_list <- readr::read_rds(file = "test_data.rda")
  morphology_df <- getMorphology(list_holder = test_list)
  oxygen_df <- getOxygen(list_holder = test_list)

  phenotypes_df <- getPhenotypes(morphology_df = morphology_df, oxygen_df = oxygen_df)

  expect_named(phenotypes_df, c("ID", "taxon", "rank", "gram_stain", "aerobic_status"), ignore.order = T)

})

# More like notes for future tests:
test_that("annotate microbiome data", {

  my_df <- data.frame(Taxa = c('E. coli', 'B. fragilis'),
                      Counts = c(100,200))

  # ... magic stuff ...

  my_vec <- c(Anaerobes = 0.5, Aerobes = 0.5)

})

test_that("get aerobic status 1", {

  my_input <- "Escherichia coli"

  my_answer <- data.frame(property = "aerobic_status", value = "facultative anaerobe", probability = 1.0)

})

test_that("get aerobic status 2", {

  my_input <- "Corynebacterium"

  my_answer <- data.frame(property = c("aerobic_status", "aerobic_status"), value = c("facultative anaerobe", "obligate aerobe"), probability = c(0.5, 0.5))

})

test_that("get aerobic status 3", {

  my_input <- "Bacteroidetes"

  my_answer <- data.frame(property = c("aerobic_status", "aerobic_status"), value = c("facultative anaerobe", "obligate anaerobe"), probability = c(0.2, 0.8))

  my_answer <- data.frame(property = c("aerobic_status", "aerobic_status", "gram_stain"), value = c("facultative anaerobe", "obligate anaerobe", "Gram negative"), probability = c(0.2, 0.8, 1.0))

})
