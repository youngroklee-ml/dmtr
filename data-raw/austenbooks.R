## code to prepare `austenbooks` dataset goes here

library(janeaustenr)
austenbooks <- austen_books()

usethis::use_data(austenbooks, overwrite = TRUE)
