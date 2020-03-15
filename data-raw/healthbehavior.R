## code to prepare `healthbehavior` dataset goes here

library(dplyr)

healthbehavior <- tribble(
  ~id, ~x1, ~x2, ~x3, ~x4, ~x5,
  1, 1, 1, 1, 0, 1,
  2, 1, 0, 1, 0, 0,
  3, 0, 1, 0, 1, 0
)

usethis::use_data(healthbehavior, overwrite = TRUE)
