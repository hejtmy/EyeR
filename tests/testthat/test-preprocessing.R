context('Preprocessing')

obj <- eyer_data

test_that("Tests changing resolution", {

})

test_that("Tests removing out of bounds data", {

})

test_that("Downsampling workds", {
  eyer_d <- downsample(obj, 10)
  original_n <- nrow(obj$data$gaze)
  expect_equal(nrow(eyer_d$data$gaze), original_n/10 - original_n %% 10)
})

test_that("flipping axis works", {
  expect_warning(flipped <- flip_axis(obj, "z", 5))
  expect_null(flipped)
  expect_silent(obj_flipped <- flip_axis(obj, "y", 1080))
  expect_s3_class(obj_flipped, "eyer")
  expect_silent(obj_flipped <- flip_axis(obj, "x", 1920))
  expect_s3_class(obj_flipped, "eyer")
})
