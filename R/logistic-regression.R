#' 이분 로지스틱 회귀분석 로그우도함수.
#'
#' 주어진 계수로부터 관측 데이터가 얻어질 우도함수값을 계산한다.
#'
#' @param .betas 계수 벡터.
#' @param .x 독립변수 행렬.
#' @param .y 종속변수 벡터.
#' @return 로그우도함수값.
loglik_binary_logistic_regression <- function(.betas, .x, .y) {
  .betas <- matrix(.betas, ncol = 1L)
  p <- 1 - (1 + exp(.x %*% .betas)) ^ (-1)
  res <- sum(.y * log(p) + (1L - .y) * log(1 - p))
  return (res)
}


#' 이분 로지스틱 회귀분석 계수 추정.
#'
#' 주어진 데이터에 대해 이분 로지스틱 회귀모형 계수를 추정한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .group_var 범주변수.
#' @param ... 범주 분류에 사용될 변수.
#' @param .reflevel 이분 범주에서 1(positive) 값을 지닐 범주. NULL값일 때는 \code{.group_var} 범주
#'   레벨에서 마지막 레벨을 사용한다.
#' @param .control \code{\link[stats]{optim}} 함수 수행 시 \code{control} 파라미터 리스트.
#' @return 리스트. 최우추정 계수 벡터 \code{betas}. 헤시안 행렬 \code{hessian}.
#'
#' @examples
#' data(student, package = "dmtr")
#' fit_binary_logistic_regression(student, y, x1, x2, x3, .reflevel = "우수")
#'
#' @export
fit_binary_logistic_regression <- function(
  .data, .group_var, ..., .reflevel = NULL,
  .control = list(fnscale = -1, maxit = 10000)
  ) {
  .group_var <- rlang::enquo(.group_var)
  .variables <- rlang::enquos(...)

  if (is.null(.reflevel)) {
    .reflevel <- .data %>%
      dplyr::pull(!!.group_var) %>%
      levels() %>%
      `[`(-1L)
  }

  n_betas <- length(.variables) + 1L
  betas <- stats::runif(n = n_betas)

  .x <- .data %>%
    dplyr::select(!!!.variables) %>%
    dplyr::mutate(.intercept = 1) %>%
    dplyr::select(.intercept, dplyr::everything()) %>%
    as.matrix()

  .y <- .data %>%
    dplyr::transmute(..y = dplyr::if_else(!!.group_var == .reflevel, 1L, 0L)) %>%
    dplyr::pull(..y)

  fit <- optim(betas, loglik_binary_logistic_regression,
    .x = .x,
    .y = .y,
    control = .control,
    hessian = TRUE
  )

  if (fit$convergence != 0) {
    warning("Optimization did not converge.")
  }

  betas <- fit$par
  names(betas) <- c("(Intercept)", purrr::map_chr(.variables, rlang::quo_name))

  hessian <- fit$hessian
  colnames(hessian) <- c("(Intercept)", purrr::map_chr(.variables, rlang::quo_name))
  rownames(hessian) <- colnames(hessian)

  return(list(betas = betas, hessian = hessian))
}


#' 이분 로지스틱 회귀분석 사후확률 추정.
#'
#' 주어진 계수를 이용하여 새 데이터에 대해 사후확률을 추정한다.
#'
#' @param .betas 이분 로지스틱 회귀분석 계수
#' @param .new_data 새 관측 데이터 프레임.
#' @param ... 범주 분류에 사용될 변수.
#' @param .reflevel 이분 범주에서 1(positive) 값을 지닐 범주.
#' @return 사후확률.
#'
#' @examples
#' data(student, package = "dmtr")
#' fit <- fit_binary_logistic_regression(student, y, x1, x2, x3, .reflevel = "우수")
#' posterior_binary_logistic_regression(fit$betas, student, x1, x2, x3, .reflevel = "우수")
#'
#' @export
posterior_binary_logistic_regression <- function(.betas, .new_data, ..., .reflevel) {
  .variables <- rlang::enquos(...)

  .betas <- matrix(.betas, ncol = 1L)

  .x <- .new_data %>%
    dplyr::select(!!!.variables) %>%
    dplyr::mutate(.intercept = 1) %>%
    dplyr::select(.intercept, dplyr::everything()) %>%
    as.matrix()

  p <- 1 - (1 + exp(.x %*% .betas)) ^ (-1)
  colnames(p) <- stringr::str_c(".pred", .reflevel, sep = "_")
  res <- dplyr::as_tibble(p)

  return (res)
}


