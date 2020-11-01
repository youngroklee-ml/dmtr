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
  drop(-2 * t(.x) %*% (.y - (.x %*% .beta)))
}


#' 다중회귀모형 선형 회귀계수 추정
#'
#' 주어진 데이터에 대하여 다중회귀모형을 추정한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .yvar 종속변수.
#' @param .xvar 독립변수. 독립변수가 여러 개일 때는 벡터 형태로 제공한다. (e.g. \code{c(age, height)})
#' @return 리스트. 최우추정 계수 벡터 \code{betas}. 헤시안 행렬 \code{hessian}.
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

  return(list(betas = betas, hessian = hessian))
}
