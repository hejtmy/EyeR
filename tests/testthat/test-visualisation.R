context('visualising')
obj <- eyer_data

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

test_that("test plotting area boundaries", {
  area_left_bottom <- list(xmin=0,xmax=100, ymin=0,ymax=100)
  area_right_upper <- list(xmin=obj$info$resolution$width-100,xmax=obj$info$resolution$width,
                           ymin=obj$info$resolution$height-100,ymax=obj$info$resolution$height)
  expect_silent(ggplot() + geom_area_boundaries(area_right_upper) + geom_area_boundaries(area_left_bottom))
})


test_that("test plotting monitor boundaries", {
  obj_empty <- EyerObject()
  expect_warning(g <- geom_eyer_monitor(obj_empty))
  expect_silent(g <- geom_eyer_monitor(obj))
  expect_silent(g <- geom_eyer_monitor(obj, alpha = 0.2, color= "red", size=1.5))
  plt <- plot_gaze_heatmap(obj)
  expect_silent(plt + g)
})
