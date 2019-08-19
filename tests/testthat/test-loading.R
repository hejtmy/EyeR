context('loading')

DIR <- system.file("extdata", package = "eyer")
FILEPATH <- paste0(DIR, "/test-file-SR1000.asc")
EYETRACKER <- "SR 1000"
FIXATIONS_N_ROW <- 797

test_that('can load asc data without error',{
  df <- read_eye_fixations(FILEPATH, EYETRACKER)
  expect_equal(nrow(df), FIXATIONS_N_ROW)
  df <- read_eye_events(FILEPATH, EYETRACKER)
  expect_equal(nrow(df), 4990)
})

test_that("can load from a directory", {
  eye <- load_eyetracker_data(DIR, F, EYETRACKER)
  expect_s3_class(eye$data$events, "data.frame")
  expect_s3_class(eye$data$fixations, "data.frame")
  expect_equal(nrow(eye$data$fixations), FIXATIONS_N_ROW)
})

test_that("Can load preprocessed data", {

})

test_that("Can override preprocessed data", {

})
