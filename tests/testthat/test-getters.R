context("getters work")


test_that("filtering works", {
  START_TIME <- eyer$info$start_time

  expect_silent(filtered <- filter_times(eyer, 0, 10))
  expect_s3_class(filtered, "eyer")
  expect_equal(nrow(filtered$data$events), 0)

  expect_silent(filtered <- filter_times(eyer, 0, 10, data_fields = "gaze"))
  expect_equal(nrow(filtered$data$events), 1)

  expect_silent(filtered_raw <- filter_times(eyer, START_TIME, START_TIME + 10, raw_times = T))
  expect_silent(filtered <- filter_times(eyer, 0, 10, raw_times = F))
  expect_equal(filtered_raw, filtered)
})
