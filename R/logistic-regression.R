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
  p <- drop(p)
  res <- sum(.y * log(p) + (1L - .y) * log(1 - p))
  return (res)
}

#' 이분 로지스틱 회귀분석 로그우도함수의 gradient.
#'
#' 주어진 계수에서 우도함수의 편미분값을 계산한다.
#'
#' @param .betas 계수 벡터.
#' @param .x 독립변수 행렬.
#' @param .y 종속변수 벡터.
#' @return 편미분값 벡터.
grad_binary_logistic_regression <- function(.betas, .x, .y) {
  .betas <- matrix(.betas, ncol = 1L)
  p <- 1 - (1 + exp(.x %*% .betas)) ^ (-1)
  p <- drop(p)
  q <- exp(.x %*% .betas)
  q <- drop(q)
  dp <- (q / ((1 + q) ^ 2)) * .x
  res <- colSums((.y / p - (1 - .y) / (1 - p)) * dp)
  return (res)
}



#' 이분 로지스틱 회귀분석 계수 추정.
#'
#' 주어진 데이터에 대해 이분 로지스틱 회귀모형 계수를 추정한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .group_var 범주변수.
#' @param .xvar 범주 분류에 사용될 변수.
#' @param .reflevel 이분 범주에서 0(negative) 값을 지닐 범주. NULL값일 때는 \code{.group_var} 범주
#'   레벨에서 마지막 레벨을 사용한다.
#' @param .control \code{\link[stats]{optim}} 함수 수행 시 \code{control} 파라미터 리스트.
#' @return 리스트. 최우추정 계수 벡터 \code{betas}. 헤시안 행렬 \code{hessian}.
#'
#' @examples
#' data(student, package = "dmtr")
#' fit_binary_logistic_regression(student, y, c(x1, x2, x3), .reflevel = "우수")
#'
#' @export
fit_binary_logistic_regression <- function(
  .data, .group_var, .xvar, .reflevel = NULL,
  .control = list(fnscale = -1, maxit = 10000)
  ) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  all_levels <- .data %>%
    dplyr::pull(!!.group_var) %>%
    levels()

  if (is.null(.reflevel)) {
    .reflevel <- all_levels[-1L]
  }


  X <- .data %>%
    dplyr::select(!!.xvar) %>%
    dplyr::mutate(`(Intercept)` = 1, .before = 1L) %>%
    as.matrix()

  n_betas <- ncol(X)
  betas <- stats::runif(n = n_betas)

  y <- .data %>%
    dplyr::transmute(..y = dplyr::if_else(!!.group_var == .reflevel, 0L, 1L)) %>%
    dplyr::pull(..y)

  fit <- stats::optim(
    betas,
    fn = loglik_binary_logistic_regression,
    gr = grad_binary_logistic_regression,
    .x = X,
    .y = y,
    control = .control,
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


#' 이분 로지스틱 회귀분석 사후확률 추정.
#'
#' 주어진 계수를 이용하여 새 데이터에 대해 사후확률을 추정한다.
#'
#' @param .betas 이분 로지스틱 회귀분석 계수
#' @param .new_data 새 관측 데이터 프레임.
#' @param .xvar 범주 분류에 사용될 변수.
#' @param .reflevel 이분 범주에서 0(negative) 값을 지닐 범주값. Default: 0.
#' @param .poslevel 이분 범주에서 1(positive) 값을 지닐 범주값. Default: 1.
#' @return 각 범주별 사후확률.
#'
#' @examples
#' data(student, package = "dmtr")
#' fit <- fit_binary_logistic_regression(student, y, c(x1, x2, x3), .reflevel = "보통")
#' posterior_binary_logistic_regression(fit$betas, student, c(x1, x2, x3),
#'   .reflevel = "보통", .poslevel = "우수")
#'
#' @export
posterior_binary_logistic_regression <- function(.betas, .new_data, .xvar,
  .reflevel = 0L, .poslevel = 1L) {
  .xvar <- rlang::enquo(.xvar)

  betas <- matrix(.betas, ncol = 1L)

  X <- .new_data %>%
    dplyr::select(!!.xvar) %>%
    dplyr::mutate(`(Intercept)` = 1, .before = 1L) %>%
    as.matrix()

  res <- dplyr::tibble(
    positive = 1 - (1 + exp(drop(X %*% betas))) ^ (-1),
    negative = 1 - positive
  )

  names(res) <- stringr::str_c(".pred", c(.poslevel, .reflevel), sep = "_")

  return(res)
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

  y_matrix <- factor_to_matrix(.y)
  betas <- matrix(.betas, ncol = n_levels - 1L)

  idx_reflevel <- which(all_levels == .reflevel)

  betas <- cbind(
    betas[, seq_len(idx_reflevel - 1L)],
    rep(0, nrow(betas)),
    betas[, rlang::seq2(idx_reflevel, n_levels - 1L)]
  )

  score_mat <- exp(.x %*% betas)
  denominator <- rowSums(score_mat)
  p <- score_mat / denominator
  p[is.nan(p)] <- 1
  p <- normalize_to_prob(p)

  res <- sum(log(p[y_matrix == 1L]))

  return (res)
}


#' 명목형 로지스틱 회귀분석 계수 추정.
#'
#' 주어진 데이터에 대해 명목형 로지스틱 회귀모형 계수를 추정한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .group_var 범주변수.
#' @param .xvar 범주 분류에 사용될 변수.
#' @param .reflevel 기준 범주. NULL값일 때는 \code{.group_var} 범주
#'   레벨에서 마지막 레벨을 사용한다.
#' @param .control \code{\link[stats]{optim}} 함수 수행 시 \code{control} 파라미터 리스트.
#' @return 리스트. 최우추정 계수 행렬 \code{betas}. 헤시안 행렬 \code{hessian}.
#'
#' @examples
#' data(defecttype, package = "dmtr")
#' fit <- fit_multinom_logistic_regression(defecttype, y, c(x1, x2), .reflevel = 3L,
#'   .control = list(fnscale = -1, reltol = 1e-10, maxit = 100000))
#'
#' @export
fit_multinom_logistic_regression <- function(
  .data, .group_var, .xvar, .reflevel = NULL,
  .control = list(fnscale = -1, maxit = 1000)
) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  all_levels <- .data %>% dplyr::pull(!!.group_var) %>% levels()
  n_levels <- length(all_levels)

  if (is.null(.reflevel)) {
    .reflevel <- all_levels[n_levels]
  }

  X <- .data %>%
    dplyr::select(!!.xvar) %>%
    dplyr::mutate(`(Intercept)` = 1, .before = 1L) %>%
    as.matrix()

  n_betas <- ncol(X)
  betas <- matrix(
    stats::runif(n = n_betas * (n_levels - 1L)),
    nrow = n_betas,
    ncol = n_levels - 1L
  )

  y <- .data %>%
    dplyr::pull(!!.group_var)

  fit <- stats::optim(betas, loglik_multinom_logistic_regression,
    .x = X,
    .y = y,
    .reflevel = .reflevel,
    control = .control,
    hessian = TRUE,
    method = "BFGS"
  )

  if (fit$convergence != 0) {
    warning("Optimization did not converge.")
  }

  betas <- matrix(fit$par, ncol = n_levels - 1L)

  rownames(betas) <- colnames(X)
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


#' 서열형 로지스틱 회귀분석 로그우도함수.
#'
#' 주어진 계수로부터 관측 데이터가 얻어질 우도함수값을 계산한다.
#'
#' @param .betas 계수 벡터. 각 열은 범주를 나타내고 각 행은 변수를 나타낸다.
#'   기준범주 열은 생략되어, 범주 개수보다 하나 적은 열을 지닌다.
#' @param .x 독립변수 행렬.
#' @param .y 종속변수 벡터.
#' @param .type 로짓모형의 종류. "cumulative": 누적 로짓모형, "adjacent": 인근범주 로짓모형
#' @return 로그우도함수값.
loglik_ordinal_logistic_regression <- function(.betas, .x, .y,
  .type = "cumulative") {
  all_levels <- levels(.y)
  n_levels <- length(all_levels)
  if (n_levels < 3L) stop("Output variable must be factor w/ three or more levels.")

  n_obs <- length(.y)
  y_matrix <- factor_to_matrix(.y)

  n_betas <- ncol(.x)
  betas_mat <- matrix(0, nrow = n_betas, ncol = n_levels - 1L)
  betas_mat[1, ] <- .betas[seq_len(n_levels - 1L)]
  if (n_betas > 1) {
    betas_mat[2:n_betas, ] <- .betas[n_levels - 1 + seq_len(n_betas - 1)]
  }

  if (.type == "cumulative") {
    kappa <- cbind(
      rep(0, n_obs),
      1 / (exp(- .x %*% betas_mat) + 1),
      rep(1, n_obs)
    )
    p <- kappa[, -1] - kappa[, -(n_levels + 1)]
  } else if (.type == "adjacent") {
    score_mat <- cbind(
      exp(t(apply(.x %*% betas_mat, 1, function(x) rev(cumsum(rev(x)))))),
      rep(1, nrow(.x))
    )
    denominator <- rowSums(score_mat)
    p <- score_mat / denominator
  }

  # 모든 p가 0보다 큰 확률값을 갖도록 처리
  p <- pmax(p, 0)
  p[is.nan(p)] <- 1
  p <- normalize_to_prob(p)

  res <- sum(log(p[y_matrix == 1L]))

  return (res)
}


#' 서열형 로지스틱 회귀분석 계수 추정.
#'
#' 주어진 데이터에 대해 서열형 로지스틱 회귀모형 계수를 추정한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .group_var 범주변수.
#' @param .xvar 범주 분류에 사용될 변수.
#' @param .type 로짓모형의 종류. "cumulative": 누적 로짓모형, "adjacent": 인근범주 로짓모형
#' @param .control \code{\link[stats]{optim}} 함수 수행 시 \code{control} 파라미터 리스트.
#' @return 리스트. 최우추정 계수 행렬 \code{betas}. 헤시안 행렬 \code{hessian}.
#'
#' @examples
#' data(telconnection, package = "dmtr")
#' fit_ordinal_logistic_regression(
#'   telconnection, y, c(N, L)
#' )
#' fit_ordinal_logistic_regression(
#'   telconnection, y, c(N, L), .type = "adjacent"
#' )
#'
#' @export
fit_ordinal_logistic_regression <- function(
  .data, .group_var, .xvar, .type = "cumulative",
  .control = list(fnscale = -1, maxit = 1000)
) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  all_levels <- .data %>%
    dplyr::pull(!!.group_var) %>%
    levels()
  n_levels <- length(all_levels)

  if (n_levels < 3L)
    stop(".group_var needs to have three or more levels.")

  X <- .data %>%
    dplyr::select(!!.xvar) %>%
    dplyr::mutate(`(Intercept)` = 1, .before = 1L) %>%
    as.matrix()

  n_betas <- ncol(X)
  betas <- c(
    sort(stats::runif(n = n_levels - 1L)),
    stats::runif(n = n_betas - 1L)
  )

  y <- .data %>%
    dplyr::pull(!!.group_var)

  if (!is.ordered(y))
    stop(".group_var must be an ordinal variable.")

  fit <- stats::optim(betas,
    loglik_ordinal_logistic_regression,
    .x = X,
    .y = y,
    .type = .type,
    control = c(.control),
    hessian = TRUE
  )

  return (fit)
}








