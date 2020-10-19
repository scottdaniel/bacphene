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
