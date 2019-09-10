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

test_that("analysis of areas works", {
  resolution <- obj$info$resolution
  center <- AreaObject("hey", c(resolution$width/2 - 200, resolution$width/2 + 200),
                       c(resolution$height/2 - 200, resolution$height/2 + 200))

})
