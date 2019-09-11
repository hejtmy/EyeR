context('Area')

obj <- eyer_data

test_that("Creation of area works", {
  area <- AreaObject("hey", c(0,1), c(0,1))
  expect_s3_class(area, "area")
})

test_that("validations of area works", {
  area <- AreaObject("hey", c(0,1), c(0,1))
  expect_true(is_valid_area(area))
  area <- AreaObject("hey", c(0,0), c(0,1))
  expect_false(is_valid_area(area))
})

test_that("plotting of areas works", {
  area <- AreaObject("hey", c(0,400), c(0,400))
  expect_silent(plt <- ggplot() + geom_area_boundaries(area) + geom_eyer_monitor(obj))
  expect_s3_class(plt, "ggplot")
})

test_that("analysis of fixation areas works", {
  resolution <- obj$info$resolution
  center <- AreaObject("center", c(resolution$width/2 - 200, resolution$width/2 + 200),
                       c(resolution$height/2 - 200, resolution$height/2 + 200))
  expect_silent(df <- analyse_fixation_areas(obj, list(center)))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_gt(df$duration[1], df$duration[2])
})

test_that("analysis of gaze areas works", {
  resolution <- obj$info$resolution
  center <- AreaObject("center", c(resolution$width/2 - 200, resolution$width/2 + 200),
                       c(resolution$height/2 - 200, resolution$height/2 + 200))
  expect_silent(df <- analyse_gaze_areas(obj, list(center)))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_equal(df$ratio[1], 1)
  expect_gt(df$ratio[1], df$ratio[2])
})
