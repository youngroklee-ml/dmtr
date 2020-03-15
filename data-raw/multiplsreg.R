## code to prepare `multiplsreg` dataset goes here

library(dplyr)

multiplsreg <- tribble(
  ~x1, ~x2, ~x3, ~x4, ~y1, ~y2,
  -1, -0.5, -1, 1, 5.9, -10,
  1, 1.1, -6, -6, -3.7, -2,
  0, 0.3, -5, -2, 1, 11,
  -3, -3.2, -9, 19, 7.7, -22,
  4, 1.2, 14, -12, -7.5, 4,
  -2, -2.6, -2, 9, 2.8, 1,
  1, 3.7, 9, -9, -6.2, 18
)

usethis::use_data(multiplsreg, overwrite = TRUE)
