#' 회귀 제곱합
#'
#' 주어진 회귀계수로부터 관측 데이터에 대한 회귀제곱합을 계산한다.
#'
#' @param .beta 계수 벡터.
#' @param .x 독립변수 행렬.
#' @param .y 종속변수 벡터.
#' @return 회귀제곱합.
fn_sum_of_squares_ols <- function(.beta, .x, .y) {
  beta <- matrix(.beta, ncol = 1)
  sum((.y - (.x %*% beta)) ^ 2)
}

#' 회귀 제곱합 미분값
#'
#' 회귀제곱합을 회귀계수에 대한 함수라 할 때, 각 회귀계수에 대하여 편미분한 함수의 값을 주어진 회귀계수에 대하여 계산한다.
#'
#' @param .beta 계수 벡터.
#' @param .x 독립변수 행렬.
#' @param .y 종속변수 벡터.
#' @return 회귀제곱합 편미분값.
gr_sum_of_squares_ols <- function(.beta, .x, .y) {
  beta <- matrix(.beta, ncol = 1)
  drop(-2 * t(.x) %*% (.y - (.x %*% beta)))
}


#' 다중회귀모형 선형 회귀계수 추정
#'
#' 주어진 데이터에 대하여 다중회귀모형을 추정한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .yvar 종속변수.
#' @param .xvar 독립변수. 독립변수가 여러 개일 때는 벡터 형태로 제공한다. (e.g. \code{c(age, height)})
#' @return 리스트. 최우추정 계수 벡터 \code{betas}, 헤시안 행렬 \code{hessian},
#'         평균잔차제곱 \code{mse}, 잔차자유도 \code{df}, 회귀계수 표준오차 \code{se},
#'         전체제곱합 \code{sst}, 결정계수 \code{rsq}, 수정결졍계수 \code{rsqadj}, 관측치개수 \code{n}.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit <- fit_linear_regression(biometric, weight, c(age, height))
#'
#' @export
fit_linear_regression <- function(.data, .yvar, .xvar) {
  .yvar <- rlang::enquo(.yvar)
  .xvar <- rlang::enquo(.xvar)

  y <- .data %>%
    dplyr::pull(!!.yvar) %>%
    matrix(ncol = 1L)

  X <- .data %>%
    dplyr::select(!!.xvar) %>%
    dplyr::mutate(`(Intercept)` = 1, .before = 1L) %>%
    as.matrix()

  fit <- stats::optim(
    rep(0, ncol(X)),
    fn = fn_sum_of_squares_ols,
    gr = gr_sum_of_squares_ols,
    .x = X,
    .y = y,
    method = "BFGS",
    hessian = TRUE
  )

  if (fit$convergence != 0) {
    warning("Optimization did not converge.")
  }

  betas <- fit$par
  names(betas) <- colnames(X)

  hessian <- fit$hessian
  rownames(hessian) <- colnames(hessian) <- colnames(X)

  df <- length(y) - length(betas)
  mse <- fit$value / df

  se <- sqrt(diag(mse * solve(hessian / 2)))

  sst <- sum((y - mean(y)) ^ 2)
  rsq <- 1 - (mse * df) / sst
  rsqadj <- 1 - (length(y) - 1) / df * (1 - rsq)

  return(list(betas = betas, hessian = hessian, mse = mse, df = df, se = se,
              sst = sst, rsq = rsq, rsqadj = rsqadj, n = length(y)))
}


#' 다중선형회귀모형 미래반응치 예측.
#'
#' 주어진 계수를 이용하여 새 데이터에 대해 종속변수값을 예측한다.
#'
#' @param .fit 회귀모형 추정 결과.
#' @param .new_data 새 관측 데이터 프레임.
#' @param .xvar 예측에 사용될 변수.
#' @param .ci_interval 평균반응치 신뢰구간. 0(default값)인 경우 신뢰구간을 구하지 않으며, 0 과 1 사이일 경우 \code{(100 * .interval)}\%의 신뢰구간을 구한다.
#' @param .pi_interval 미래반응치 예측구간. 0(default값)인 경우 예측구간을 구하지 않으며, 0 과 1 사이일 경우 \code{(100 * .interval)}\%의 예측구간을 구한다.
#' @return 애측값 데이터프레임.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit <- fit_linear_regression(biometric, weight, c(age, height))
#' predict_linear_regression(fit, biometric, c(age, height))
#' predict_linear_regression(fit, dplyr::tibble(age = 40, height = 170), c(age, height), .ci_interval = 0.95)
#' predict_linear_regression(fit, dplyr::tibble(age = 40, height = 170), c(age, height), .pi_interval = 0.95)
#'
#' @export
predict_linear_regression <- function(
  .fit,
  .new_data,
  .xvar,
  .ci_interval = 0,
  .pi_interval = 0) {
  .xvar <- rlang::enquo(.xvar)

  betas <- matrix(.fit$betas, ncol = 1L)

  X <- .new_data %>%
    dplyr::select(!!.xvar) %>%
    dplyr::mutate(`(Intercept)` = 1, .before = 1L) %>%
    as.matrix()

  res <- dplyr::tibble(
    .pred = as.vector(X %*% betas)
  )

  if (max(.ci_interval, .pi_interval) > 0 &&
      max(.ci_interval, .pi_interval) < 1) {
    xtx <- solve(.fit$hessian / 2)
    se <- sqrt(.fit$mse * apply(X, 1, function(x, xtx) {t(x) %*% xtx %*% x}, xtx = xtx))
    res <- res %>%
      dplyr::mutate(.se = se)
    if (.ci_interval > 0) {
      res <- res %>%
        dplyr::mutate(
          .ci_lower = .pred + qt(0.5 - .ci_interval / 2, .fit$df) * .se,
          .ci_upper = .pred + qt(0.5 + .ci_interval / 2, .fit$df) * .se
        )
    }
    if (.pi_interval > 0) {
      res <- res %>%
        dplyr::mutate(
          .pi_lower = .pred + qt(0.5 - .pi_interval / 2, .fit$df) * sqrt(.fit$mse + .se ^ 2),
          .pi_upper = .pred + qt(0.5 + .pi_interval / 2, .fit$df) * sqrt(.fit$mse + .se ^ 2)
        )
    }
  }

  return (res)
}


#' 다중선형회귀모형 분산분석표.
#'
#' 추정된 회귀모형을 이용하여 회귀성 검정을 위한 분산분석표를 생성한다.
#'
#' @param .fit 회귀모형 추정 결과.
#' @return 분산분석표 데이터프레임.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit <- fit_linear_regression(biometric, weight, c(age, height))
#' anova_linear_regression(fit)
#'
#' @export
anova_linear_regression <- function(.fit) {
  dplyr::tibble(
    source = c("Model", "Residuals", "Total"),
    df = c(.fit$n - 1 - .fit$df, .fit$df, .fit$n - 1),
    ss = c(.fit$sst - .fit$mse * .fit$df, .fit$mse * .fit$df, .fit$sst)
  ) %>%
    dplyr::mutate(
      ms = dplyr::if_else(
        dplyr::row_number() %in% c(1, 2), ss / df, NA_real_),
      F_statistic = dplyr::if_else(
        dplyr::row_number() == 1,
        ms / dplyr::lead(ms), NA_real_),
      p_value = 1 - stats::pf(F_statistic, .fit$n - 1 - .fit$df, .fit$df)
    )
}


#' 다중선형회귀모형 회귀계수 검정.
#'
#' 추정된 회귀모형을 이용하여 각 회귀계수의 유의성을 검정한다.
#'
#' @param .fit 회귀모형 추정 결과.
#' @return 회귀계수 검정 데이터프레임.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit <- fit_linear_regression(biometric, weight, c(age, height))
#' ttest_linear_regression(fit)
#'
#' @export
ttest_linear_regression <- function(.fit) {
  dplyr::tibble(
    term = names(.fit$betas),
    estimate = .fit$betas,
    std_error = .fit$se,
    t_statistic = estimate / std_error,
    p_value = stats::pt( - abs(t_statistic), .fit$df) * 2
  )
}


#' 맬로우즈 Cp 통계량
#'
#' 맬로우즈 Cp 통계량을 계산한다.
#'
#' @param .fit_reduced $p$항 회귀모형 추정 결과.
#' @param .fit_full 완전모형 추정 결과.
#' @return 맬로우즈 Cp 통계량.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit_full <- fit_linear_regression(biometric, weight, c(age, height))
#' fit_reduced <- fit_linear_regression(biometric, weight, height)
#' mallows_c(fit_reduced, fit_full)
#'
#' @export
mallows_c <- function(.fit_reduced, .fit_full) {
  (.fit_reduced$mse * .fit_reduced$df) / .fit_full$mse -
    .fit_reduced$n + 2 * (.fit_reduced$n - .fit_reduced$df)
}


#' 다중회귀모형 선택척도 산출
#'
#' 회귀모형의 각 변수조합에 대한 선택척도를 산출한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .yvar 종속변수.
#' @param .xvar 완전모형에 속할 독립변수. 독립변수가 여러 개일 때는 벡터 형태로 제공한다. (e.g. \code{c(age, height)})
#' @return \code{.xvar}의 각 변수조합에 대한 선택척도 데이터프레임.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' evaluate_linear_regression(biometric, weight, c(age, height))
#'
#' @export
evaluate_linear_regression <- function(.data, .yvar, .xvar) {
  variables <- tidyselect::eval_select(rlang::enquo(.xvar), .data) %>% names()
  .yvar <- rlang::enquo(.yvar)

  variables_in_model <- purrr::map(
    seq(from = 0L, to = length(variables)),
    ~ combn(variables, .x, simplify = FALSE)
  )
  variables_in_model <- unlist(variables_in_model, recursive = FALSE)

  fit_reduced <- purrr::map(
    variables_in_model,
    ~ fit_linear_regression(.data, !!.yvar, dplyr::all_of(.x))
  )

  fit_full <- fit_reduced[[length(fit_reduced)]]

  res <- purrr::map_dfr(
    fit_reduced,
    ~ dplyr::tibble(
      p = length(.x$betas),
      rsq = .x$rsq,
      rsqadj = .x$rsqadj,
      cp = mallows_c(.x, fit_full),
      mse = .x$mse,
      terms = paste(names(.x$betas), collapse = ", ")
    )
  )

  return(res)
}


#' 다중회귀모형 Type II 제곱합을 이용한 F 검정
#'
#' 회귀모형의 각 변수에 대해 Type II 제곱합을 산출하고 F 검정을 수행한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .yvar 종속변수.
#' @param .xvar 완전모형에 속할 독립변수. 독립변수가 여러 개일 때는 벡터 형태로 제공한다. (e.g. \code{c(age, height)})
#' @param .last_only .xvar 중 가장 마지막 변수에 대해서만 검정을 수행할 것인지 여부. Default: \code{FALSE}
#' @return \code{.xvar}의 각 변수에 대한 검정 결과 데이터프레임.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' test_type2_linear_regression(biometric, weight, c(age, height))
#'
#' @export
test_type2_linear_regression <- function(.data, .yvar, .xvar, .last_only = FALSE) {
  .xvar <- rlang::enquo(.xvar)
  .yvar <- rlang::enquo(.yvar)

  fit_full <- fit_linear_regression(.data, !!.yvar, !!.xvar)
  ssr_full <- fit_full$sst * fit_full$rsq
  mse_full <- fit_full$mse
  df_full <- fit_full$df

  variables <- tidyselect::eval_select(.xvar, .data) %>% names()
  if (.last_only) {
    candidates <- variables[length(variables)]
  } else {
    candidates <- variables
  }

  fit_reduced <- purrr::map(
    candidates,
    ~ fit_linear_regression(.data, !!.yvar, setdiff(variables, .x))
  )

  res <- purrr::map2_dfr(
    candidates,
    fit_reduced,
    ~ dplyr::tibble(
      terms = .x,
      ss = ssr_full - .y$sst * .y$rsq,
      F_statistic = ss / mse_full,
      p_value = 1 - stats::pf(F_statistic, 1, df_full)
    )
  )

  return(res)
}


#' 다중회귀모형 단계적 선택방법을 통한 회귀모형 입력변수 선택
#'
#' 단계적 선택방법을 통해 독립변수를 선택한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .yvar 종속변수.
#' @param .xvar 완전모형에 속할 독립변수. 독립변수가 여러 개일 때는 벡터 형태로 제공한다. (e.g. \code{c(age, height)})
#' @param .alpha_in 변수 선택 단계에서 적용할 유의수준. Default: 0.05
#' @param .alpha_out 변수 제거 단계에서 적용할 유의수준. \code{.alpha_in} 보다 크거나 같아야 한다. Default: 0.10
#' @param .maxit 최대 iteration 수. Default: 100.
#' @param .verbose 각 단계의 선택/제거 변수를 화면에 출력할 지 여부. Default: \code{TRUE}
#' @return 최종 선택된 독립변수 이름 벡터.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' select_variables_stepwise(biometric, weight, c(age, height))
#'
#' @export
select_variables_stepwise <- function(.data, .yvar, .xvar,
                             .alpha_in = 0.05, .alpha_out = 0.10, .maxit = 100L,
                             .verbose = TRUE) {
  if (.alpha_in > .alpha_out) {
    stop(".alpha_in must be less than or equal to .alpha_out")
  }

  .xvar <- rlang::enquo(.xvar)
  .yvar <- rlang::enquo(.yvar)
  variables <- tidyselect::eval_select(.xvar, .data) %>% names()

  res_addition <- purrr::map_dfr(
    variables,
    ~ test_type2_linear_regression(.data, !!.yvar, .x)
  ) %>%
    dplyr::slice_max(ss, n = 1L, with_ties = FALSE) %>%
    dplyr::filter(p_value < .alpha_in)

  if (nrow(res_addition) == 0L) {
    cat("No variable gives statistically significant improvement of the fit.\n")
    return(as.character())
  }

  if (.verbose) {
    cat(
      "Inital variable: ",
      res_addition[["terms"]],
      ", p-value = ",
      res_addition[["p_value"]],
      " < ",
      .alpha_in,
      "\n"
    )
  }

  variables_in_model <- res_addition[["terms"]]

  for (i in seq_len(.maxit)) {
    variables_not_in_model <- setdiff(variables, variables_in_model)

    if (.verbose) {
      cat("Iteration ", i, ": forward selection - ")
    }

    res_addition <- purrr::map_dfr(
      variables_not_in_model,
      ~ test_type2_linear_regression(.data, !!.yvar, c(variables_in_model, .x),
                                     .last_only = TRUE)
    ) %>%
      dplyr::slice_max(ss, n = 1L, with_ties = FALSE) %>%
      dplyr::filter(p_value < .alpha_in)

    if (nrow(res_addition) == 0) {
      if (.verbose) {
        cat("no additional variable gives statistically significant improvement of the fit.\n")
      }
      return(variables_in_model)
    }

    if (.verbose) {
      cat(res_addition[["terms"]], ", p-value = ", res_addition[["p_value"]], " < ", .alpha_in, "\n")
    }

    variables_in_model <- c(variables_in_model, res_addition[["terms"]])
    variables_not_in_model <- setdiff(variables_not_in_model, res_addition[["terms"]])

    if (.verbose) {
      cat("Iteration ", i, ": backward elimination - ")
    }

    res_elimination <- test_type2_linear_regression(.data, !!.yvar, variables_in_model) %>%
      dplyr::slice_min(ss, n = 1L, with_ties = FALSE) %>%
      dplyr::filter(p_value > .alpha_out)

    if (nrow(res_elimination) == 0) {
      if (.verbose) {
        cat("no variable can be deleted without a statistically significant loss of fit.\n")
      }
    } else {
      if (.verbose) {
        cat(res_elimination[["terms"]], ", p-value = ", res_elimination[["p_value"]], " > ", .alpha_out, "\n")
      }

      variables_in_model <- setdiff(variables_in_model, res_elimination[["terms"]])
      variables_not_in_model <- c(variables_not_in_model, res_elimination[["terms"]])
    }

    if (rlang::is_empty(variables_not_in_model)) {
      if (.verbose) {
        cat("Every variable provides statistically significant improvement of fit.\n")
      }
      return(variables_in_model)
    } else if (i == .maxit) {
      warning("Reached to maximum number of iterations. Please consider increasing a value of .maxit.\n")
      return(variables_in_model)
    }

  }

}


#' 다중회귀모형 분산팽창계수
#'
#' 다중공선성의 정도를 알아보기 위해 분산팽창계수를 계산한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .xvar 완전모형에 속할 독립변수. 독립변수가 여러 개일 때는 벡터 형태로 제공한다. (e.g. \code{c(age, height)})
#' @return \code{.xvar}의 각 변수조합에 대한 분산팽창계수.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' vif_linear_regression(biometric, c(age, height))
#'
#' @export
vif_linear_regression <- function(.data, .xvar) {
  .xvar <- rlang::enquo(.xvar)

  variables <- tidyselect::eval_select(.xvar, .data) %>% names()

  res <- purrr::map_dbl(
    variables,
    ~ 1 / (1 - fit_linear_regression(.data, .x, setdiff(variables, .x))[["rsq"]])
  ) %>%
    magrittr::set_names(variables) %>%
    tibble::enframe(name = "terms", value = "vif")

  return(res)
}
