context('loading')

test_folder <- system.file("extdata", package = "eyer")
FIXATIONS_N_ROW <- 378

test_that("Can load preprocessed data", {
  expect_message(eye <- load_eyer_data(test_folder))
  expect_s3_class(eye, "eyer")
  expect_equal(FIXATIONS_N_ROW, nrow(eye$data$fixations))
})

test_that("Can validate loaded data", {

})
