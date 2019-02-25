context('loading')

FILEPATH <- "../../inst/extdata/test-file.asc"
EYETRACKER <- "SR 1000"
test_that('can load asc data without error',{
  df <- read_eye_fixations(FILEPATH, EYETRACKER)
  expect_equal(nrow(df), 795)
  df <- read_eye_events(FILEPATH, EYETRACKER)
  expect_equal(nrow(df), 795)
})
