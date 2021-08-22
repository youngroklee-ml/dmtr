## code to prepare `iris` dataset goes here

library(dplyr)

# iris90 <- datasets::iris %>%
#   rename(
#     x1 = Sepal.Length,
#     x2 = Sepal.Width,
#     x3 = Petal.Length,
#     x4 = Petal.Width,
#     class = Species
#   ) %>%
#   group_by(class) %>%
#   slice(1:30) %>%
#   ungroup() %>%
#   mutate(id = row_number()) %>%
#   select(id, everything())

iris90 <- read.csv("data-raw/iris.csv") %>%
  mutate(class = factor(class, levels = c("setosa", "versicolor", "virginica"))) %>%
  group_by(class) %>%
  slice(1L:30L) %>%
  ungroup()

usethis::use_data(iris90, overwrite = TRUE)
