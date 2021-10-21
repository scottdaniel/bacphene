source("bacdive_credentials.R")

test_that("credentials stored", {

  expect_type(user, "character")

  expect_type(passwd, "character")

})

test_that("get strains", {

  strain_list <- getStrains(page = 1, genus = 'Bacteroides', species = 'xylanisolvens', userpassword = paste0(user,':',passwd))

  expect_named(strain_list, c("count","next","previous","results"))

})

test_that("get strain data", {

  strain_list <- getStrains(page = 1,
    genus = 'Bacteroides',
    species = 'xylanisolvens',
    userpassword = paste0(user,':',passwd))

  phenotypes_list <- getStrainData(strain_list,
    selection=1,
    userpassword = paste0(user,':',passwd))

  expect_named(phenotypes_list, c("taxonomy_name", "morphology_physiology", "culture_growth_condition",
                              "environment_sampling_isolation_source", "application_interaction",
                              "molecular_biology", "strain_availability", "references"))

})

test_that("gram stain", {

  strain_list <- getStrains(page = 1,
                            genus = 'Bacteroides',
                            species = 'xylanisolvens',
                            userpassword = paste0(user,':',passwd))

  phenotypes_list <- getStrainData(strain_list,
                                   selection=1,
                                   userpassword = paste0(user,':',passwd))

  gram_stain <- gramStain(phenotypes_list)

  expect_equal(gram_stain, "negative")

  })

test_that("oxygen tolerance", {

  strain_list <- getStrains(page = 1,
                            genus = 'Bacteroides',
                            species = 'xylanisolvens',
                            userpassword = paste0(user,':',passwd))

  phenotypes_list <- getStrainData(strain_list,
                                   selection=1,
                                   userpassword = paste0(user,':',passwd))

  oxygen_tolerance <- oxygenTolerance(phenotypes_list)

  expect_equal(oxygen_tolerance, "anaerobe")

})

test_that("vancomycin sensitive", {

  strain_list <- getStrains(page = 1,
                            genus = 'Actibacterium',
                            species = 'pelagium',
                            userpassword = paste0(user,':',passwd))

  phenotypes_list <- getStrainData(strain_list,
                                   selection=1,
                                   userpassword = paste0(user,':',passwd))

  expect_true(abxSensitive(phenotypes_list, abx = "vancomycin"))

})

test_that("gentamicin resistant", {

  strain_list <- getStrains(page = 1,
                            genus = 'Pseudomonas',
                            species = 'tarimensis',
                            userpassword = paste0(user,':',passwd))

  phenotypes_list <- getStrainData(strain_list,
                                   selection=1,
                                   userpassword = paste0(user,':',passwd))

  expect_true(abxResistant(phenotypes_list, abx = "gentamicin"))

})

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
