## code to prepare `rollsteel` dataset goes here

library(dplyr)

rollsteel <- tribble(
  ~ct, ~thickness, ~ts,
  540, 2, 52.5,
  660, 2, 50.2,
  610, 2, 51.3,
  710, 2, 49.1,
  570, 6, 50.8,
  700, 6, 48.7,
  560, 6, 51.2,
  600, 6, 50.8,
  680, 6, 49.3,
  530, 6, 51.5
) %>%
  mutate(thickness = factor(thickness, levels = c(6, 2)))

usethis::use_data(rollsteel, overwrite = TRUE)
