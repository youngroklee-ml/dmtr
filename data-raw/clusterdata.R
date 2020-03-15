## code to prepare `clusterdata` dataset goes here

library(dplyr)

clusterdata1 <- tribble(
  ~id, ~x1, ~x2,
  1, 30, 15,
  2, 45, 22,
  3, 25, 12,
  4, 40, 24,
  5, 50, 25,
  6, 20, 10,
  7, 42, 9
)

clusterdata2 <- tribble(
  ~id, ~x1, ~x2,
  1, 3, 3,
  2, 5, 4,
  3, 11, 8,
  4, 13, 6,
  5, 14, 6,
  6, 15, 7
)

clusterdata3 <- tribble(
  ~id, ~x1, ~x2,
  1, 4, 12,
  2, 6, 13,
  3, 6, 15,
  4, 10, 4,
  5, 11, 3,
  6, 12, 2,
  7, 12, 5
)

clusterdata4 <- tribble(
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

usethis::use_data(
  clusterdata1,
  clusterdata2,
  clusterdata3,
  clusterdata4,
  overwrite = TRUE
)
