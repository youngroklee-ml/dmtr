## code to prepare `transactiondata` dataset goes here

library(dplyr)

transactiondata <- tribble(
  ~transaction_id, ~item,
  1, "b",
  1, "c",
  1, "g",
  2, "a",
  2, "b",
  2, "d",
  2, "e",
  2, "f",
  3, "a",
  3, "b",
  3, "c",
  3, "g",
  4, "b",
  4, "c",
  4, "e",
  4, "f",
  5, "b",
  5, "c",
  5, "e",
  5, "f",
  5, "g"
)

transactionseq <- tribble(
  ~customer_id, ~transaction_seq, ~item,
  1, 1, "a",
  1, 2, "b",
  2, 1, "c",
  2, 1, "d",
  2, 2, "a",
  2, 3, "e",
  2, 3, "f",
  2, 3, "g",
  3, 1, "a",
  3, 1, "h",
  3, 1, "g",
  4, 1, "a",
  4, 2, "e",
  4, 2, "g",
  4, 3, "b",
  5, 1, "b"
)

usethis::use_data(
  transactiondata,
  transactionseq,
  overwrite = TRUE
)
