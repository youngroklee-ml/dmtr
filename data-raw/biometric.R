## code to prepare `biometric` dataset goes here

library(dplyr)

biometric <- tribble(
  ~age, ~height, ~weight,
  21, 170, 60,
  47, 167, 65,
  36, 173, 67,
  15, 165, 54,
  54, 168, 73,
  25, 177, 71,
  32, 169, 68,
  18, 172, 62,
  43, 171, 66,
  28, 175, 68
)

usethis::use_data(biometric, overwrite = TRUE)
