test_that("binary logistic regression coefficient matches to book example", {
  expect_equal(
    fit_binary_logistic_regression(student, y, x1:x3, .reflevel = "보통")[["betas"]],
    c("(Intercept)" = -30.51, x1 = 2.031, x2 = 3.471, x3 = 2.414),
    tolerance = 1e-3
  )
})

test_that("", {
  expect_equal(
    fit_binary_logistic_regression(student, y, x1:x3, .reflevel = "보통")[["betas"]] %>%
      posterior_binary_logistic_regression(student, x1:x3, .reflevel = "보통", .poslevel = "우수") %>%
      dplyr::pull(`.pred_우수`),
    c(0.8895, 0.1458, 0.6746, 0.8811, 0.9840, 0.7367, 0.0020, 0.0053,
      0.2002, 0.4187, 0.0002, 0.3294, 0.0800, 0.6561, 0.0007),
    tolerance = 1e-3
  )
})
