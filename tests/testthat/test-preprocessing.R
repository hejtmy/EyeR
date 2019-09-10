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
})
