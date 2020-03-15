## code to prepare `signalnoise` dataset goes here

library(dplyr)

telconnection <- tribble(
  ~N, ~L, ~y,
  25, 5, 3,
  25, 10, 3,
  25, 20, 2,
  25, 30, 1,
  32, 5, 3,
  32, 10, 3,
  32, 20, 2,
  32, 30, 1,
  42, 5, 1,
  42, 10, 3,
  42, 20, 1,
  42, 30, 1
) %>%
  mutate(y = as.ordered(y))

usethis::use_data(telconnection, overwrite = TRUE)
