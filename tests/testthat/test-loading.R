context('loading')

test_folder <- "../../inst/extdata/"
FIXATIONS_N_ROW <- 378

test_that("Can load preprocessed data", {
  expect_silent(eye <- load_eyer_data(test_folder))
  expect_s3_class(eye, "eyer")
  expect_equal(FIXATIONS_N_ROW, nrow(eye$data$fixations))
})

test_that("Can validate loaded data", {

})
