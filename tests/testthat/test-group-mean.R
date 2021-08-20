context("Group mean")

library(dplyr)

data(binaryclass2, package = "dmtr")

test_that("group_mean returns correct length of list", {
  expect_equal(
    length(group_mean(binaryclass2, class, x1)),
    length(levels(binaryclass2$class))
  )
})


