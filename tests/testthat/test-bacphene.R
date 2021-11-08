
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
