test_that("principal component eigen values matches to book example", {
  expect_equal(
    fit_pca(financials, roa:turnover)[["eig"]] %>% unname(),
    c(2.7615, 1.6057, 0.5506, 0.0641, 0.0183),
    tolerance = 1e-3
  )
})

test_that("principal component eigen values matches to book example", {
  expect_equal(
    fit_pca(pcrdata, x1:x3, .scale = FALSE)[["eig"]] %>% unname(),
    c(54.709, 2.769, 0.122),
    tolerance = 1e-3
  )
})

test_that("principal component regression matches to book example", {
  expect_equal(
    fit_pcr(pcrdata, y, x1:x3, .scale = FALSE, .pc = 2L)[["org_betas"]][c("x1", "x2", "x3")],
    c(x1 = 2.1303, x2 = 2.7217, x3 = -1.7379),
    tolerance = 1e-3
  )
})


