context("getters work")

obj <- eyer_data

test_that("filtering single time works", {
  START_TIME <- obj$info$start_time

  expect_silent(filtered <- filter_times(obj, c(0, 10)))
  expect_s3_class(filtered, "eyer")
  expect_equal(nrow(filtered$data$events), 0)

  expect_silent(filtered <- filter_times(obj, c(0, 10), data_fields = "gaze"))
  expect_equal(nrow(filtered$data$events), 1)

  expect_silent(filtered_raw <- filter_times(obj, c(START_TIME, START_TIME + 10), raw_times = T))
  expect_silent(filtered <- filter_times(obj, c(0, 10), raw_times = F))
  expect_equal(filtered_raw, filtered)
})

test_that("filtering dataframes works", {
  START_TIME <- obj$info$start_time

  times <- data.frame(start=c(0,50,100), end=c(10,70,110))

  expect_silent(filtered <- filter_times(obj, times))
  expect_s3_class(filtered, "eyer")
  expect_equal(nrow(filtered$data$events), 0)

  expect_silent(filtered <- filter_times(obj, c(0, 10), data_fields = "gaze"))
  expect_equal(nrow(filtered$data$events), 1)

  expect_silent(filtered_raw <- filter_times(obj, times+START_TIME , raw_times = T))
  expect_silent(filtered <- filter_times(obj, times, raw_times = F))
  expect_equal(filtered_raw, filtered)
})
