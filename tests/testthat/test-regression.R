data(biometric, package = "dmtr")
fit <- fit_linear_regression(biometric, weight, c(age, height))
new_data <- tibble::tibble(age = 40, height = 170)

test_that("multiple regression coefficient matches to book example", {
  expect_equal(
    fit[["betas"]],
    c(`(Intercept)` = -108.17, age = 0.3291, height = 0.9553),
    tolerance = 1e-3
  )
})

test_that("multiple regression confidence interval", {
  expect_equal(
    predict_linear_regression(
      fit, .new_data = new_data, .xvar = c(age, height), .ci_interval = 0.95
    ) %>%
      dplyr::select(.ci_lower, .ci_upper) %>%
      unlist(),
    c(.ci_lower = 65.015, .ci_upper = 69.779),
    tolerance = 1e-3
  )
})

test_that("multiple regression prediction interval", {
  expect_equal(
    predict_linear_regression(
      fit, .new_data = new_data, .xvar = c(age, height), .pi_interval = 0.95
    ) %>%
      dplyr::select(.pi_lower, .pi_upper) %>%
      unlist(),
    c(.pi_lower = 60.686, .pi_upper = 74.108),
    tolerance = 1e-3
  )
})
