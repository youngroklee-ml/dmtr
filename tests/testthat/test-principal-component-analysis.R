data(financials, package = "dmtr")
fit <- fit_pca(financials, roa:turnover)

test_that("principal component eigen values matches to book example", {
  expect_equal(
    fit[["eig"]] %>% unname(),
    c(2.7615, 1.6057, 0.5506, 0.0641, 0.0183),
    tolerance = 1e-3
  )
})
