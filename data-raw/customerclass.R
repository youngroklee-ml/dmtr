## code to prepare `customerclass` dataset goes here

library(dplyr)

customerclass_train <- tribble(
  ~id, ~x1, ~x2, ~y,
  1, "남", "20대", 1,
  2, "남", "20대", 2,
  3, "남", "30대", 1,
  4, "남", "40대", 1,
  5, "여", "10대", 1,
  6, "여", "20대", 2,
  7, "여", "20대", 1,
  8, "여", "30대", 2,
  9, "여", "40대", 2
) %>%
  mutate(y = factor(y, levels = c(1, 2)))

customerclass_test <- tibble(x1 = "남", x2 = "10대")

usethis::use_data(
  customerclass_train,
  customerclass_test,
  overwrite = TRUE
)
