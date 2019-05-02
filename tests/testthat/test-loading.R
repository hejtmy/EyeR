context('loading')

DIR <- system.file("extdata", package = "eyer")
FILEPATH <- paste0(DIR, "/test-file-SR1000.asc")
EYETRACKER <- "SR 1000"

test_that('can load asc data without error',{
  df <- read_eye_fixations(FILEPATH, EYETRACKER)
  expect_equal(nrow(df), 797)
  df <- read_eye_events(FILEPATH, EYETRACKER)
  expect_equal(nrow(df), 4990)
})

test_that("can load from a directory", {

})

test_that("Can load preprocessed data", {

})

test_that("Can override preprocessed data", {

})
