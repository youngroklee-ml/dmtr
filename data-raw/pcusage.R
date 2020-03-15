## code to prepare `pcusage` dataset goes here

library(dplyr)

pcusage1 <- tribble(
  ~id, ~x1, ~x2, ~x3,
  1, 20, 6, 14,
  2, 28, 8, 13,
  3, 42, 14, 6,
  4, 35, 12, 7,
  5, 30, 15, 7,
  6, 30, 7, 15,
  7, 45, 13, 6,
  8, 46, 4, 2,
  9, 51, 3, 3,
  10, 41, 3, 2
)

pcusage2 <- tribble(
  ~id, ~x1, ~x2,
  1, 6, 14,
  2, 8, 13,
  3, 14, 6,
  4, 11, 8,
  5, 15, 7,
  6, 7, 15,
  7, 13, 6,
  8, 5, 4,
  9, 3, 3,
  10, 3, 2
)

usethis::use_data(
  pcusage1,
  pcusage2,
  overwrite = TRUE
)
