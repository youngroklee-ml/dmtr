test_that("binary logistic regression coefficient", {
  data(student, package = "dmtr")

  expect_equal(
    fit_binary_logistic_regression(student, y, x1:x3, .reflevel = "보통")[["betas"]],
    c("(Intercept)" = -30.51, x1 = 2.031, x2 = 3.471, x3 = 2.414),
    tolerance = 0.01
  )
})
