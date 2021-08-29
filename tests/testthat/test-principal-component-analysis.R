test_that("principal component eigen values matches to book example", {
  expect_equal(
    fit_pca(financials, roa:turnover)[["eig"]] %>% unname(),
    c(2.7615, 1.6057, 0.5506, 0.0641, 0.0183),
    tolerance = 1e-3
  )
})

test_that("principal component eigen values matches to book example", {
  expect_equal(
    fit_pca(pcrdata, x1:x3, .center = FALSE, .scale = FALSE)[["eig"]] %>% unname(),
    c(54.709, 2.769, 0.122),
    tolerance = 1e-3
  )
})
