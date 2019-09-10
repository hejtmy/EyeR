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
  area_left_bottom <- AreaObject("left-bottom", c(0,100), c(0,100))
  area_right_upper <- AreaObject("upper", c(obj$info$resolution$width-100,obj$info$resolution$width),
                           c(obj$info$resolution$height-100,obj$info$resolution$height))
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
