context('preprocessing')

test_that('can load asc data without error',{
  df <- read_eye_fixations(FILEPATH, EYETRACKER)
  expect_equal(nrow(df), 797)
  df <- read_eye_events(FILEPATH, EYETRACKER)
  expect_equal(nrow(df), 4990)
})
