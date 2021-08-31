test_that("PLSR results match to book example", {
  expect_equal(
    fit_plsr(pcrdata, y, x1:x3, .ncomp = 2L, .scale = FALSE)[["betas"]][c("LV1", "LV2")] %>%
      unname(),
    c(2.9246, 2.4647),
    tolerance = 1e-3
  )
})

test_that("Multivariate PLSR results match to book example", {
  expect_equal(
    nipals_plsr_n(
      multiplsreg %>% dplyr::select(x1:x4) %>% as.matrix(),
      multiplsreg %>% dplyr::select(y1:y2) %>% as.matrix(),
      ncomp = 3L
    )[["b"]] %>%
      unname(),
    c(0.8444, 0.4256, 3.2063),
    tolerance = 1e-3
  )
})
