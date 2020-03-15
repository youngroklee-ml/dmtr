## code to prepare `gainchart` dataset goes here

library(dplyr)

gainchart_yfreq <- tribble(
  ~y, ~n,
  1, 437,
  2, 348,
  3, 215
) %>%
  mutate(y = factor(y, levels = c(1, 2, 3)))

gainchart_group1 <- tribble(
  ~k, ~n,
  1, 92,
  2, 78,
  3, 64,
  4, 57,
  5, 43,
  6, 35,
  7, 29,
  8, 22,
  9, 7,
  10, 10
)

testthat::expect_equal(
  gainchart_yfreq %>% filter(y == 1) %>% pull(n),
  sum(gainchart_group1 %>% pull(n))
)

usethis::use_data(
  gainchart_yfreq,
  gainchart_group1,
  overwrite = TRUE
)
