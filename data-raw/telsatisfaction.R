## code to prepare `telsatisfaction` dataset goes here

library(dplyr)

telsatisfaction <- tribble(
  ~id, ~x1, ~x2, ~x3, ~x4, ~x5,
  1, "남", 46, "공무원", 35000, 2,
  2, "여", 28, "은행원", 51000, 3,
  3, "여", 32, "주부", 46000, 4
) %>%
  mutate(
    x1 = factor(x1, levels = c("남", "여")),
    x3 = factor(x3),
    x5 = factor(x5, levels = c(1:5), ordered = TRUE)
  )

telsatisfaction_range <- tibble(
  range = c("min", "max"),
  x2 = c(25, 70),
  x4 = c(0, 150000),
  x5 = factor(c(1, 5), levels = c(1:5), ordered = TRUE)
)

usethis::use_data(
  telsatisfaction,
  telsatisfaction_range,
  overwrite = TRUE
)
