context("Discriminant analysis")

library(dplyr)

data(binaryclass2, package = "dmtr")

test_that("Discriminant function matches", {
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

test_that("LDA score matches: w/o prior", {
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


test_that("LDA score matches: with prior", {
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

