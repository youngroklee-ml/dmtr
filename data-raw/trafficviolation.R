## code to prepare `trafficviolation` dataset goes here

library(dplyr)

trafficviolation <- tribble(
  ~id, ~x1, ~x2,
  1, 4, 15,
  2, 20, 13,
  3, 3, 13,
  4, 19, 4,
  5, 17, 17,
  6, 8, 11,
  7, 19, 12,
  8, 18, 6
)

usethis::use_data(trafficviolation, overwrite = TRUE)
