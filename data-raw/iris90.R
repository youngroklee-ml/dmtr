## code to prepare `iris` dataset goes here

library(dplyr)

iris90 <- datasets::iris %>%
  rename(
    x1 = Sepal.Length,
    x2 = Sepal.Width,
    x3 = Petal.Length,
    x4 = Petal.Width,
    class = Species
  ) %>%
  group_by(class) %>%
  slice(1:30) %>%
  ungroup() %>%
  mutate(id = row_number()) %>%
  select(id, everything())

usethis::use_data(iris90)
