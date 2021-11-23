#bacdive_morphology

test_that("getMorphology", {

  # structure of list object gotten from bacdive
  # if it changes, it will break the code
  test_list <- readr::read_rds(file = "test_data.rda")

  morphology_df <- getMorphology(list_holder = test_list)

  expect_named(morphology_df, c('@ref', 'cell length', 'cell shape', 'cell width', 'doi/url', 'flagellum arrangement', 'gram stain', 'ID', 'motility', 'rank', 'taxon', 'type_strain'), ignore.order = T)

})

test_that("getOxygen", {

  test_list <- readr::read_rds(file = "test_data.rda")

  oxygen_df <- getOxygen(list_holder = test_list)

  expect_named(oxygen_df, c("@ref", "oxygen tolerance", "ID", "taxon", "rank", "doi/url", "type_strain"), ignore.order = T)

})

test_that("getPhenotypes", {

  test_list <- readr::read_rds(file = "test_data.rda")
  morphology_df <- getMorphology(list_holder = test_list)
  oxygen_df <- getOxygen(list_holder = test_list)

  phenotypes_df <- getPhenotypes(morphology_df = morphology_df, oxygen_df = oxygen_df)

  expect_named(phenotypes_df, c("taxon", "rank", "gram_stain", "aerobic_status"), ignore.order = T)

})

test_that("get multiple strains", {

  test_list <- readr::read_rds(file = "test_data.rda")
  bac_frag <- getStrainLocal(test_list, query = "Bacteroides fragilis", typestrain_only = F)

  expect_equal(bac_frag[[1]]$`Name and taxonomic classification`$species, "Bacteroides fragilis")
  expect_length(bac_frag, 2)

})

test_that("only get type strain", {

  test_list <- readr::read_rds(file = "test_data.rda")
  ent_fae <- getStrainLocal(test_list, query = "Bacteroides fragilis", typestrain_only = T)

  expect_equal(ent_fae[[1]]$`Name and taxonomic classification`$species, "Bacteroides fragilis")
  expect_equal(ent_fae[[1]]$`Name and taxonomic classification`$`type strain`, "yes")

})

test_that("getAbx", {

  test_list <- readr::read_rds(file = "test_data.rda")

  abx_df <- getAbx(test_list)

  expect_named(abx_df, c("@ref", "ChEBI", "metabolite", "is antibiotic", "is resistant",
                            "resistance conc.", "is sensitive", "sensitivity conc.", "ID",
                            "taxon", "rank", "type_strain", "doi/url", "medium", "incubation temperature",
                            "oxygen condition", "incubation time", "diameter", "group ID",
                            "is intermediate", "intermediate conc."), ignore.order = T)

})

test_that("consistent bacdive susceptibility", {

  test_list <- readr::read_rds(file = "test_data.rda")

  abx_df <- getAbx(test_list)

  suscept_df <-getSimplifiedAbx(data = abx_df, extra_info = T, most_common = F, remove_unknown = F)

  expect_equal(unique(suscept_df[suscept_df$`is sensitive` %in% "yes",]$value), c("sensitive", "unknown"))

  # or like this?
  # suscept_df %>% filter(`is sensitive` %in% "yes") %>% pull(value) %>% unique()

  expect_equal(unique(suscept_df[suscept_df$`is resistant` %in% "yes",]$value), c("resistant", "unknown"))
  expect_equal(unique(suscept_df[suscept_df$diameter %in% "0",]$value), c("resistant"))
  expect_equal(unique(suscept_df[suscept_df$diameter %in% "n.d.",]$value), c("unknown"))

  expect_equal(unique(suscept_df[is.na(suscept_df$diameter),]$value), c("resistant", "sensitive", "unknown"))

})

test_that("getEnzymes", {

  test_list <- readr::read_rds(file = "test_data.rda")

  enzymes_df <- getEnzymes(list_holder = test_list, most_common = T, remove_unknown = T)

  expect_named(enzymes_df, c("taxon", "rank", "value", "ec", "activity"), ignore.order = T)

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
