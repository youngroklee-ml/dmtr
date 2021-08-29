test_that("pooled variance-covariance matrix matches to book example", {
  expect_equal(
    pooled_variance(binaryclass2, class, c(x1, x2)) %>%
      as.numeric(),
    c(3.8857, 2.1143, 2.1143, 2.4571),
    tolerance = 1e-3
  )
})


test_that("group_mean returns correct length of list", {
  expect_equal(
    length(group_mean(binaryclass2, class, x1)),
    length(levels(binaryclass2$class))
  )
})
