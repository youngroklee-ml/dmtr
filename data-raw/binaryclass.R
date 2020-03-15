## code to prepare `binaryclass` dataset goes here

library(dplyr)

binaryclass1_train <- tribble(
  ~id, ~x1, ~x2, ~y,
  1, 5, 7, 1,
  2, 4, 3, 2,
  3, 7, 8, 2,
  4, 8, 6, 2,
  5, 3, 6, 1,
  6, 2, 5, 1,
  7, 9, 6, 2
) %>%
  mutate(y = factor(y, levels = c(1, 2)))

binaryclass1_test <- tribble(
  ~id, ~x1, ~x2,
  8, 6, 7,
  9, 4, 2
)

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

binaryclass3_train <- tribble(
  ~x1, ~x2, ~class,
  1, 4, 1,
  2, 6, 1,
  2, 5, 1,
  2, 4, 2,
  2, 3, 2,
  3, 6, 1,
  4, 6, 1,
  4, 5, 2,
  4, 4, 2,
  5, 3, 2
) %>%
  mutate(class = factor(class, levels = c(1, 2)))


binaryclass3_test <- tribble(
  ~x1, ~x2, ~class,
  1, 5, 1,
  0, 5, 1,
  3, 4, 2,
  4, 3, 2,
  2, 7, 1,
  1, 4, 2
) %>%
  mutate(class = factor(class, levels = c(1, 2)))

binaryclass4_separable <- tribble(
  ~x1, ~x2, ~class,
  5, 7, 1,
  4, 3, -1,
  7, 8, 1,
  8, 6, 1,
  3, 6, -1,
  2, 5, -1,
  6, 6, 1,
  9, 6, 1,
  5, 4, -1
)

binaryclass4_inseparable <- bind_rows(
  binaryclass4_separable,
  tibble(x1 = 7, x2 = 6, class = -1)
)

binaryclass4_nonlinear <- tribble(
  ~x1, ~x2, ~class,
  5, 7, 1,
  4, 3, -1,
  7, 8, -1,
  8, 6, -1,
  3, 6, 1,
  2, 5, 1,
  6, 6, 1,
  9, 6, -1,
  5, 4, -1
)

binaryclass5 <- tribble(
  ~x, ~y,
  24, 0,
  35, 0,
  37, 1,
  42, 0,
  49, 1,
  54, 1,
  56, 0,
  68, 1,
  72, 1,
  73, 1
) %>%
  mutate(y = factor(y, levels = c(1, 0)))


usethis::use_data(
  binaryclass1_train,
  binaryclass1_test,
  binaryclass2,
  binaryclass3_train,
  binaryclass3_test,
  binaryclass4_separable,
  binaryclass4_inseparable,
  binaryclass4_nonlinear,
  binaryclass5,
  overwrite = TRUE
)


