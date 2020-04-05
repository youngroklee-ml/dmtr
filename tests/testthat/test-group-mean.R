context("Group mean")

library(dplyr)

binaryclass2 <- tribble(
  ~id, ~x1, ~x2, ~class,
  1, 5, 7, 1,
  2, 4, 3, 2,
  3, 7, 8, 2,
  4, 8, 6, 2,
  5, 3, 6, 1,
  6, 2, 5, 1,
  7, 6, 6, 1,
  8, 9, 6, 2,
  9, 5, 4, 2
) %>%
  mutate(class = factor(class, levels = c(1, 2)))

test_that("group_mean returns correct length of list", {
  expect_equal(
    length(group_mean(binaryclass2, class, x1)),
    length(levels(binaryclass2$class))
  )
})
