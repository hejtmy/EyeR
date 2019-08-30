context('Preprocessing')

test_that("Tests changing resolution", {

})

test_that("Tests removing out of bounds data", {

})

test_that("Downsampling workds", {
  eyer_d <- downsample(eyer, 10)
  original_n <- nrow(eyer$data$gaze)
  expect_equal(nrow(eyer_d$data$gaze), original_n/10 - original_n %% 10)
})
