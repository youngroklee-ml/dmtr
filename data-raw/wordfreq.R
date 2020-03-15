## code to prepare `wordfreq` dataset goes here

library(dplyr)

wordweight <- tribble(
  ~word, ~weight,
  "word1", 0.124,
  "word2", 0.275,
  "word3", 0.019,
  "word4", 0.182,
  "word5", 0.223
)

wordtfidf <- tribble(
  ~document, ~word, ~tfidf,
  "doc1", "word1", 0.0194,
  "doc1", "word2", 0.0043,
  "doc1", "word3", 0.0054,
  "doc1", "word4", 0.0155,
  "doc1", "word5", 0.0028,
  "doc2", "word1", 0.0082,
  "doc2", "word2", 0.0032,
  "doc2", "word3", 0.0007,
  "doc2", "word4", 0.0104,
  "doc2", "word5", 0.0073,
  "doc3", "word1", 0.0087,
  "doc3", "word2", 0.0174,
  "doc3", "word3", 0.0091,
  "doc3", "word4", 0.0086,
  "doc3", "word5", 0.0268,
  "doc4", "word1", 0.0093,
  "doc4", "word2", 0.0061,
  "doc4", "word3", 0.0172,
  "doc4", "word4", 0.0028,
  "doc4", "word5", 0.0009,
  "doc5", "word1", 0.0185,
  "doc5", "word2", 0.0249,
  "doc5", "word3", 0.0084,
  "doc5", "word4", 0.0167,
  "doc5", "word5", 0.0193,
  "doc6", "word1", 0.0028,
  "doc6", "word2", 0.0003,
  "doc6", "word3", 0.0202,
  "doc6", "word4", 0.0083,
  "doc6", "word5", 0.0054
)

usethis::use_data(
  wordweight,
  wordtfidf,
  overwrite = TRUE
)
