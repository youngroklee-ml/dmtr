#' NIPALS 알고리즘 - 부분최소자승 회귀분석
#'
#' NIPALS 알고리즘을 이용하여 부분최소자승 회귀모형을 구한다.
#'
#' @param X 독립변수 행렬.
#' @param y 종속변수 벡터.
#' @param ncomp 추출할 잠재변수 개수.
#' @return 리스트. \code{T}: 스코어 행렬, \code{W}: 가중치 행렬, \code{P}: 로딩행렬, \code{b} 회귀계수.
#'
#' @export
nipals_plsr <- function(X, y, ncomp = NULL) {
  if (rlang::is_empty(ncomp) || (ncomp > min(dim(X)))) {
    ncomp <- min(dim(X))
  }

  Tmat <- matrix(NA, nrow = nrow(X), ncol = ncomp)
  colnames(Tmat) <- stringr::str_c("LV", seq_len(ncomp))

  Wmat <- matrix(NA, nrow = ncol(X), ncol = ncomp)
  rownames(Wmat) <- colnames(X)
  colnames(Wmat) <- colnames(Tmat)

  Pmat <- matrix(NA, nrow = ncol(X), ncol = ncomp)
  rownames(Pmat) <- colnames(X)
  colnames(Pmat) <- colnames(Tmat)

  b <- vector("numeric", length = ncomp)
  names(b) <- colnames(Tmat)

  for (a in seq_len(ncomp)) {
    Wmat[, a] <- 1 / sum(y ^ 2) * (t(X) %*% y)
    Wmat[, a] <- Wmat[, a] / sqrt(sum(Wmat[, a] ^ 2))

    Tmat[, a] <- X %*% Wmat[, a]

    Pmat[, a] <- 1 / sum(Tmat[, a] ^ 2) * (t(X) %*% Tmat[, a])

    p_size <- sqrt(sum(Pmat[, a] ^ 2))
    Tmat[, a] <- Tmat[, a] * p_size
    Wmat[, a] <- Wmat[, a] * p_size

    Pmat[, a] <- Pmat[, a] / p_size

    b[a] <- 1 / sum(Tmat[, a] ^ 2) * sum(y * Tmat[, a])

    X <- X - Tmat[, a] %*% t(Pmat[, a])
    y <- y - Tmat[, a] %*% t(b[a])
  }

  return(list(T = Tmat, W = Wmat, P = Pmat, b = b, ncomp = ncomp))
}


#' NIPALS 알고리즘 - 부분최소자승 회귀분석
#'
#' NIPALS 알고리즘을 이용하여 부분최소자승 회귀모형을 구한다.
#'
#' @param X 독립변수 행렬.
#' @param Y 종속변수 행렬.
#' @param ncomp 추출할 잠재변수 개수.
#' @return 리스트.
#'
#' @export
nipals_plsr_n <- function(X, Y, ncomp = NULL) {
  if (nrow(X) != nrow(Y)) stop("X and Y must have the same numbers of observations.")

  if (rlang::is_empty(ncomp) || (ncomp > min(dim(X)))) {
    ncomp <- min(dim(X))
  }

  Tmat <- matrix(NA, nrow = nrow(X), ncol = ncomp)
  colnames(Tmat) <- stringr::str_c("LV", seq_len(ncomp))

  Umat <- matrix(NA, nrow = nrow(X), ncol = ncomp)
  colnames(Umat) <- colnames(Tmat)

  Wmat <- matrix(NA, nrow = ncol(X), ncol = ncomp)
  colnames(Wmat) <- colnames(Tmat)
  rownames(Wmat) <- colnames(X)

  Pmat <- matrix(NA, nrow = ncol(X), ncol = ncomp)
  colnames(Pmat) <- colnames(Tmat)
  rownames(Pmat) <- colnames(X)

  Qmat <- matrix(NA, nrow = ncol(Y), ncol = ncomp)
  colnames(Qmat) <- colnames(Tmat)
  rownames(Qmat) <- colnames(Y)

  b <- vector("numeric", length = ncomp)
  names(b) <- colnames(Tmat)

  for (a in seq_len(ncomp)) {
    j <- sample.int(ncol(Y), size = 1L)
    Umat[, a] <- Y[, j]

    while (TRUE) {
      Wmat[, a] <- 1 / sum(Umat[, a] ^ 2) * (t(X) %*% Umat[, a])
      Wmat[, a] <- Wmat[, a] / sqrt(sum(Wmat[, a] ^ 2))

      Tmat[, a] <- X %*% Wmat[, a]

      Qmat[, a] <- 1 / sum(Tmat[, a] ^ 2) * (t(Y) %*% Tmat[, a])
      Qmat[, a] <- Qmat[, a] / sqrt(sum(Qmat[, a] ^ 2))

      u_new <- Y %*% Qmat[, a]

      if (all(dplyr::near(u_new, Umat[, a]))) break

      Umat[, a] <- u_new
    }

    Pmat[, a] <- 1 / sum(Tmat[, a] ^ 2) * (t(X) %*% Tmat[, a])

    p_size <- sqrt(sum(Pmat[, a] ^ 2))
    Tmat[, a] <- Tmat[, a] * p_size
    Wmat[, a] <- Wmat[, a] * p_size

    Pmat[, a] <- Pmat[, a] / p_size

    b[a] <- 1 / sum(Tmat[, a] ^ 2) * sum(Umat[, a] * Tmat[, a])

    X <- X - Tmat[, a] %*% t(Pmat[, a])
    Y <- Y - b[a] * Tmat[, a] %*% t(Qmat[, a])
  }

  return(list(T = Tmat, W = Wmat, P = Pmat,
              U = Umat, Q = Qmat, b = b, ncomp = ncomp))
}



#' 부분최소자승 회귀분석
#'
#' 주어진 데이터에 대하여 부분최소자승 회귀모형을 추정한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .yvar 종속변수.
#' @param .xvar 독립변수. 독립변수가 여러 개일 때는 벡터 형태로 제공한다. (e.g. \code{c(age, height)}).
#' @param .ncomp 잠재변수의 수.
#' @param .center 평균조정 여부. TRUE이면 각 독립변수에 대해 평균조정을 수행한다. 현재 구현상 TRUE만 가능.
#' @param .scale 분산조정 여부. TRUE이면 각 독립변수에 대해 분산조정을 수행한다.
#' @return 리스트.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit <- fit_plsr(biometric, weight, c(age, height), .ncomp = 1L)
#'
#' @export
fit_plsr <- function(.data, .yvar, .xvar, .ncomp = NULL, .center = TRUE, .scale = TRUE) {
  .xvar <- rlang::enquo(.xvar)
  .yvar <- rlang::enquo(.yvar)
  variables <- tidyselect::eval_select(.xvar, .data) %>% names()
  y_variable <- tidyselect::eval_select(.yvar, .data) %>% names()

  if (!.center) stop("Must set parameter .center to be TRUE.")

  X <- .data %>%
    dplyr::select(!!.xvar) %>%
    as.matrix() %>%
    scale(center = .center, scale = .scale)

  x_center <- attr(X, "scaled:center")
  if (is.null(x_center)) {
    x_center <- rep(0, length.out = ncol(X)) %>%
      rlang::set_names(variables)
  }
  x_scale <- attr(X, "scaled:scale")
  if (is.null(x_scale)) {
    x_scale <- rep(1, length.out = ncol(X)) %>%
      rlang::set_names(variables)
  }

  y <- .data %>%
    dplyr::select(!!.yvar) %>%
    as.matrix() %>%
    scale(center = .center, scale = .scale)

  y_center <- attr(y, "scaled:center")
  if (is.null(y_center)) {
    y_center <- rep(0, length.out = ncol(y)) %>%
      rlang::set_names(y_variable)
  }
  y_scale <- attr(y, "scaled:scale")
  if (is.null(y_scale)) {
    y_scale <- rep(1, length.out = ncol(y)) %>%
      rlang::set_names(y_variable)
  }

  prep <- list(
    x_center = x_center,
    x_scale = x_scale,
    y_center = y_center,
    y_scale = y_scale
  )

  plsr_fit <- nipals_plsr(X, y, .ncomp)

  X <- dplyr::as_tibble(plsr_fit[["T"]])

  reg_data <- .data %>%
    dplyr::select(!!.yvar) %>%
    dplyr::bind_cols(X)

  lm_fit <- fit_linear_regression(reg_data, !!.yvar, -!!.yvar)

  res <- c(plsr_fit, lm_fit, prep)

  return(res)
}

