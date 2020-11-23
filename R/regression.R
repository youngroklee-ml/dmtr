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
#'         평균잔차제곱 \code{mse}, 잔차자유도 \code{df}, 전체제곱합 \code{sst},
#'         결정계수 \code{rsq}, 관측치개수 \code{n}.
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

  sst <- sum((y - mean(y)) ^ 2)
  rsq <- 1 - (mse * df) / sst

  return(list(betas = betas, hessian = hessian, mse = mse, df = df,
              sst = sst, rsq = rsq, n = length(y)))
}


#' 다중선형회귀모형 미래반응치 예측.
#'
#' 주어진 계수를 이용하여 새 데이터에 대해 종속변수값을 예측한다.
#'
#' @param .fit 회귀모형 추정 결과.
#' @param .new_data 새 관측 데이터 프레임.
#' @param .xvar 예측에 사용될 변수.
#' @param .interval 예측구간. 0인 경우 예측구간을 구하지 않으며, 0 과 1 사이일 경우 \code{(100 * .interval)}\%의 예측구간을 구한다.
#' @return 애측값 데이터프레임.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit <- fit_linear_regression(biometric, weight, c(age, height))
#' predict_linear_regression(fit, biometric, c(age, height))
#' predict_linear_regression(fit, dplyr::tibble(age = 40, height = 170), c(age, height), 0.95)
#'
#' @export
predict_linear_regression <- function(
  .fit,
  .new_data,
  .xvar,
  .interval = 0) {
  .xvar <- rlang::enquo(.xvar)

  betas <- matrix(.fit$betas, ncol = 1L)

  X <- .new_data %>%
    dplyr::select(!!.xvar) %>%
    dplyr::mutate(`(Intercept)` = 1, .before = 1L) %>%
    as.matrix()

  res <- dplyr::tibble(
    .pred = as.vector(X %*% betas)
  )

  if (.interval > 0 && .interval < 1) {
    xtx <- solve(.fit$hessian / 2)
    se <- sqrt(.fit$mse * apply(X, 1, function(x, xtx) {t(x) %*% xtx %*% x}, xtx = xtx))
    res <- res %>%
      dplyr::mutate(
        .se = se,
        .pi_lower = .pred + qt(0.5 - .interval / 2, .fit$df) * sqrt(.fit$mse + se ^ 2),
        .pi_upper = .pred + qt(0.5 + .interval / 2, .fit$df) * sqrt(.fit$mse + se ^ 2)
      )
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
    source = c("회귀", "잔차", "전체"),
    df = c(.fit$n - 1 - .fit$df, .fit$df, .fit$n - 1),
    ss = c(.fit$sst - .fit$mse * .fit$df, .fit$mse * .fit$df, .fit$sst)
  ) %>%
    dplyr::mutate(
      ms = if_else(row_number() %in% c(1, 2), ss / df, NA_real_),
      F = if_else(row_number() == 1, ms / lead(ms), NA_real_),
      p = 1 - pf(F, .fit$n - 1 - .fit$df, .fit$df)
    )
}
