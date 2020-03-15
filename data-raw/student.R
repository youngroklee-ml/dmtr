## code to prepare `student` dataset goes here

library(dplyr)

student <- tribble(
  ~id, ~x1, ~x2, ~x3, ~y,
  1, 0, 8, 2, "우수",
  2, 1, 7, 1, "우수",
  3, 0, 9, 0, "우수",
  4, 1, 6, 4, "우수",
  5, 1, 8, 2, "우수",
  6, 0, 7, 3, "우수",
  7, 0, 7, 0, "보통",
  8, 1, 6, 1, "보통",
  9, 0, 7, 2, "보통",
  10, 0, 8, 1, "보통",
  11, 0, 5, 2, "보통",
  12, 1, 8, 0, "보통",
  13, 0, 6, 3, "보통",
  14, 1, 7, 2, "보통",
  15, 0, 6, 1, "보통"
) %>%
  mutate(y = factor(y, levels = c("보통", "우수")))

usethis::use_data(student)
