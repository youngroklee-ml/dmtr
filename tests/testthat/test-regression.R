test_that("multiple regression coefficient matches to book example", {
  expect_equal(
    {
      data(biometric, package = "dmtr")
      fit_linear_regression(biometric, weight, c(age, height))[["betas"]]
    },
    c(`(Intercept)` = -108.17, age = 0.3291, height = 0.9553),
    tolerance = 1e-3
  )
})
