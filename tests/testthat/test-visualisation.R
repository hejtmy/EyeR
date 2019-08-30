context('visualising')
obj <- eyer

test_that("test plotting gaze", {
  expect_s3_class(plot_gaze(eyer), "ggplot")
  expect_s3_class(plot_gaze(eyer, downsample = 100, color="red"), "ggplot")
})

test_that("test plotting fixations", {
  expect_s3_class(plot_fixations(eyer), "ggplot")
  expect_s3_class(plot_fixations(eyer, duration = F), "ggplot")
})

test_that("test plotting heatmaps", {
  expect_s3_class(plot_gaze_heatmap(eyer), "ggplot")
})
