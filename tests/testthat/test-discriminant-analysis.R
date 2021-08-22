context("Discriminant analysis")

library(dplyr)

data(binaryclass2, package = "dmtr")

test_that("Fisher discriminant function matches", {
  expect(
    all(
      dplyr::near(
        fisher_ld(binaryclass2, class, c(x1, x2)),
        c(x1 = -1.5080, x2 = 1.5418),
        tol = 0.0001
      )
    ),
    failure_message = "Estimated discriminant function does not match to book example"
  )
})

test_that("LDA score matches: when prior is not provided", {
  expect(
    all(
      dplyr::near(
        ld_fun(binaryclass2, class, c(x1, x2)) %>%
          score_da(binaryclass2, c(x1, x2)) %>%
          as.matrix(),
        matrix(
          data = c(
            9.2051470,	6.971538,
            -1.9363321,	0.489223,
            11.0057900,	10.246458,
            4.5909990,	8.423307,
            7.4045039,	3.696619,
            5.0411598,	1.367036,
            5.7164010,	6.532631,
            4.0282981,	9.368644,
            0.4270119,	2.818805
          ),
          nrow = 9, ncol = 2, byrow = TRUE
        ),
        tol = 1e-6
      )
    ),
    failure_message = "Estimated discriminant scores do not match to expected results"
  )
})

test_that("LDA score matches: when prior is provided", {
  expect(
    all(
      dplyr::near(
        ld_fun(binaryclass2, class, c(x1, x2), .prior = c(0.4, 0.6)) %>%
          score_da(binaryclass2, c(x1, x2)) %>%
          as.matrix(),
        matrix(
          data = c(
            9.100,	7.048,
            -2.042,	0.566,
            10.901,	10.323,
            4.486,	8.500,
            7.300,	3.773,
            4.936,	1.444,
            5.611,	6.609,
            3.923,	9.445,
            0.322,	2.895
          ),
          nrow = 9, ncol = 2, byrow = TRUE
        ),
        tol = 1e-3
      )
    ),
    failure_message = "Estimated discriminant scores do not match to expected results"
  )
})

test_that("LDA posterior matches: when prior is provided", {
  expect(
    all(
      dplyr::near(
        ld_fun(binaryclass2, class, c(x1, x2), .prior = c(0.5, 0.5)) %>%
          predict_da(binaryclass2, c(x1, x2),
                     .include_posterior = TRUE,
                     .include_class = FALSE) %>%
          as.matrix(),
        matrix(
          data = c(
            0.921,	0.079,
            0.100,	0.900,
            0.728,	0.272,
            0.026,	0.974,
            0.981,	0.019,
            0.980,	0.020,
            0.356,	0.644,
            0.006,	0.994,
            0.103,	0.897
          ),
          nrow = 9, ncol = 2, byrow = TRUE
        ),
        tol = 1e-3
      )
    ),
    failure_message = "Estimated posterior do not match to expected results"
  )
})

test_that("LDA classification matches: when prior is provided", {
  expect(identical(
    ld_fun(binaryclass2, class, c(x1, x2), .prior = c(0.5, 0.5)) %>%
      predict_da(binaryclass2, c(x1, x2)),
    tibble(.pred_class = factor(c(
      1, 2, 1, 2, 1, 1, 2, 2, 2
    ), levels = c(1, 2)))
  ),
  failure_message = "LDA classification results do not match to expected results")
})

test_that("QDA score matches: when prior is not provided", {
  expect(
    all(
      dplyr::near(
        qd_fun(binaryclass2, class, c(x1, x2)) %>%
          score_da(binaryclass2, c(x1, x2)) %>%
          as.matrix(),
        matrix(
          data = c(
            -1.729447,	-3.950639,
            -13.183993,	-2.497284,
            -3.911266,	-3.145402,
            -5.274902,	-1.868806,
            -1.183993,	-5.764060,
            -1.729447,	-6.202685,
            -2.002175,	-1.934273,
            -7.729447,	-2.582391,
            -8.274902,	-1.927726
          ),
          nrow = 9, ncol = 2, byrow = TRUE
        ),
        tol = 1e-6
      )
    ),
    failure_message = "Estimated discriminant scores do not match to expected results"
  )
})

test_that("QDA score matches: when prior is provided", {
  expect(
    all(
      dplyr::near(
        qd_fun(binaryclass2, class, c(x1, x2), .prior = c(0.5, 0.5)) %>%
          score_da(binaryclass2, c(x1, x2)) %>%
          as.matrix(),
        matrix(
          data = c(
            -0.9156,	-3.3617,
            -12.3722,	-1.9094,
            -3.0959,	-2.5561,
            -4.4603,	-1.2805,
            -0.3713,	-5.1755,
            -0.9174,	-5.6144,
            -1.1885,	-1.3457,
            -6.9143,	-1.9943,
            -7.4625,	-1.3397
          ) + log(0.5),
          nrow = 9, ncol = 2, byrow = TRUE
        ),
        tol = 1e-2
      )
    ),
    failure_message = "Estimated discriminant scores do not match to expected results"
  )
})

