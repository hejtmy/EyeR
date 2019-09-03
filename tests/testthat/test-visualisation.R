context('visualising')
obj <- eyer

test_that("test plotting gaze", {
  expect_s3_class(plot_gaze(obj), "ggplot")
  expect_s3_class(plot_gaze(obj, downsample = 100, color="red"), "ggplot")
})

test_that("test plotting fixations", {
  expect_s3_class(plot_fixations(obj), "ggplot")
  expect_s3_class(plot_fixations(obj, duration = F), "ggplot")
})

test_that("test plotting heatmaps", {
  expect_s3_class(plot_gaze_heatmap(obj), "ggplot")
})

test_that("test plotting monitor boundaries", {
  obj_empty <- EyerObject()
  expect_warning(g <- geom_eyer_monitor(obj_empty))
  expect_silent(g <- geom_eyer_monitor(obj))
  expect_silent(g <- geom_eyer_monitor(obj, alpha = 0.2, color= "red", size=1.5))
  plt <- plot_gaze_heatmap(obj)
  expect_silent(plt + g)
})
