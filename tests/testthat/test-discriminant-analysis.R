context("Discriminant analysis")

library(dplyr)

data(binaryclass2, package = "dmtr")

test_that("Discriminant function matches", {
  expect(
    all(
      dplyr::near(
        fisher_ld(binaryclass2, class, c(x1, x2)),
        c(x1 = -1.5080, x2 = 1.5418),
        tol = 0.0001
      )
    ),
    failure_message = "Estimated discriminant function does not match to book example"
  )
})
