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
#' @param .reflevel 이분 범주에서 0(negative) 값을 지닐 범주. NULL값일 때는 \code{.group_var} 범주
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

  all_levels <- .data %>%
    dplyr::pull(!!.group_var) %>%
    levels()

  if (is.null(.reflevel)) {
    .reflevel <- all_levels[-1L]
  }

  n_betas <- length(.variables) + 1L
  betas <- stats::runif(n = n_betas)

  .x <- .data %>%
    dplyr::select(!!!.variables) %>%
    dplyr::mutate(.intercept = 1) %>%
    dplyr::select(.intercept, dplyr::everything()) %>%
    as.matrix()

  .y <- .data %>%
    dplyr::transmute(..y = dplyr::if_else(!!.group_var == .reflevel, 0L, 1L)) %>%
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
#' @param .reflevel 이분 범주에서 0(negative) 값을 지닐 범주값. Default: 0.
#' @param .poslevel 이분 범주에서 1(positive) 값을 지닐 범주값. Default: 1.
#' @return 각 범주별 사후확률.
#'
#' @examples
#' data(student, package = "dmtr")
#' fit <- fit_binary_logistic_regression(student, y, x1, x2, x3, .reflevel = "보통")
#' posterior_binary_logistic_regression(fit$betas, student, x1, x2, x3,
#'   .reflevel = "보통", .poslevel = "우수")
#'
#' @export
posterior_binary_logistic_regression <- function(.betas, .new_data, ...,
  .reflevel = 0L, .poslevel = 1L) {
  .variables <- rlang::enquos(...)

  .betas <- matrix(.betas, ncol = 1L)

  .x <- .new_data %>%
    dplyr::select(!!!.variables) %>%
    dplyr::mutate(.intercept = 1) %>%
    dplyr::select(.intercept, dplyr::everything()) %>%
    as.matrix()

  res <- dplyr::tibble(
    positive = 1 - (1 + exp(.x %*% .betas)) ^ (-1),
    negative = 1 - positive
  )

  names(res) <- stringr::str_c(".pred", c(.poslevel, .reflevel), sep = "_")

  return (res)
}


#' 명목형 로지스틱 회귀분석 로그우도함수.
#'
#' 주어진 계수로부터 관측 데이터가 얻어질 우도함수값을 계산한다.
#'
#' @param .betas 계수 행렬. 각 열은 범주를 나타내고 각 행은 변수를 나타낸다.
#'   기준범주 열은 생략되어, 범주 개수보다 하나 적은 열을 지닌다.
#' @param .x 독립변수 행렬.
#' @param .y 종속변수 벡터.
#' @param .reflevel 기준범주 값.
#' @return 로그우도함수값.
loglik_multinom_logistic_regression <- function(.betas, .x, .y, .reflevel) {
  all_levels <- levels(.y)
  n_levels <- length(all_levels)
  if (n_levels < 2L) stop("Output variable must be factor w/ 2 or more levels.")

  .y_matrix <- factor_to_matrix(.y)
  .betas <- matrix(.betas, ncol = n_levels - 1L)

  idx_reflevel <- which(all_levels == .reflevel)

  .betas <- cbind(
    .betas[, seq_len(idx_reflevel - 1L)],
    rep(0, nrow(.betas)),
    .betas[, rlang::seq2(idx_reflevel, n_levels - 1L)]
  )

  score_mat <- exp(.x %*% .betas)
  denominator <- rowSums(score_mat)
  p <- score_mat / denominator

  res <- sum(log(p[.y_matrix == 1L]))

  return (res)
}


#' 명목형 로지스틱 회귀분석 계수 추정.
#'
#' 주어진 데이터에 대해 명목형 로지스틱 회귀모형 계수를 추정한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .group_var 범주변수.
#' @param ... 범주 분류에 사용될 변수.
#' @param .reflevel 기준 범주. NULL값일 때는 \code{.group_var} 범주
#'   레벨에서 마지막 레벨을 사용한다.
#' @param .control \code{\link[stats]{optim}} 함수 수행 시 \code{control} 파라미터 리스트.
#' @return 리스트. 최우추정 계수 행렬 \code{betas}. 헤시안 행렬 \code{hessian}.
#'
#' @examples
#' data(defecttype, package = "dmtr")
#' fit <- fit_multinom_logistic_regression(defecttype, y, x1, x2, .reflevel = 3L,
#'   .control = list(fnscale = -1, reltol = 1e-10, maxit = 100000))
#'
#' @export
fit_multinom_logistic_regression <- function(
  .data, .group_var, ..., .reflevel = NULL,
  .control = list(fnscale = -1, maxit = 10000)
) {
  .group_var <- rlang::enquo(.group_var)
  .variables <- rlang::enquos(...)

  all_levels <- .data %>% dplyr::pull(!!.group_var) %>% levels()
  n_levels <- length(all_levels)

  if (is.null(.reflevel)) {
    .reflevel <- all_levels[n_levels]
  }

  n_betas <- length(.variables) + 1L
  betas <- matrix(
    stats::runif(n = n_betas * (n_levels - 1L)),
    nrow = n_betas,
    ncol = n_levels - 1L
  )

  .x <- .data %>%
    dplyr::select(!!!.variables) %>%
    dplyr::mutate(.intercept = 1) %>%
    dplyr::select(.intercept, dplyr::everything()) %>%
    as.matrix()

  .y <- .data %>%
    dplyr::pull(!!.group_var)

  fit <- optim(betas, loglik_multinom_logistic_regression,
    .x = .x,
    .y = .y,
    .reflevel = .reflevel,
    control = .control,
    hessian = TRUE,
    method = "BFGS"
  )

  if (fit$convergence != 0) {
    warning("Optimization did not converge.")
  }

  betas <- matrix(fit$par, ncol = n_levels - 1L)


  rownames(betas) <- c("(Intercept)", purrr::map_chr(.variables, rlang::quo_name))
  colnames(betas) <- setdiff(all_levels, .reflevel)

  hessian <- fit$hessian
  colnames(hessian) <- stringr::str_c(
    rep(rownames(betas), ncol(betas)),
    rep(colnames(betas), each = nrow(betas)),
    sep = ":"
  )
  rownames(hessian) <- colnames(hessian)

  return(list(betas = betas, hessian = hessian))
}


