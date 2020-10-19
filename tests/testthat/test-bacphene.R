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

test_that("antibiotics sensitive or resistant", {

  strain_list <- getStrains(page = 1,
                            genus = 'Actibacterium',
                            species = 'pelagium',
                            userpassword = paste0(user,':',passwd))

  phenotypes_list <- getStrainData(strain_list,
                                   selection=1,
                                   userpassword = paste0(user,':',passwd))



})
