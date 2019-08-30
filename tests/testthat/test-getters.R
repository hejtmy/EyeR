context("getters work")

test_that("filtering works", {
  expect_silent(filtered <- filter_times(eyer, 0, 10))
  expect_s3_class(filtered, "eyer")
  expect_equal(nrow(filtered$data$events), 0)

  expect_silent(filtered <- filter_times(eyer, 0, 10, data_fields = "gaze"))
  expect_equal(nrow(filtered$data$events), 1)
})
