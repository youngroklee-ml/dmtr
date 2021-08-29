
#' NIPALS 알고리즘 - 주성분분해
#'
#' NIPALS 알고리즘을 이용하여 주성분 스코어 행렬과 고유벡터행렬을 구한다.
#'
#' @param X 독립변수 행렬.
#' @param .pc 추출할 주성분의 수.
#' @return 리스트. \code{T}: 스코어 행렬, \code{V}: 고유벡터 행렬.
#'
#' @keywords principal-component-analysis
#' @export
nipals_pca <- function(X, .pc = NULL) {
  if (rlang::is_empty(.pc) || (.pc > min(dim(X)))) {
    .pc <- min(dim(X))
  }

  Th <- matrix(NA, nrow = nrow(X), ncol = .pc)
  Vh <- matrix(NA, nrow = ncol(X), ncol = .pc)

  for (h in seq_len(.pc)) {
    j <- sample(ncol(X), 1L)
    Th[, h] <- X[, j]
    while (TRUE) {
      Vh[, h] <- t(t(Th[, h]) %*% X / (norm(Th[, h], "2") ^ 2))
      Vh[, h] <- Vh[, h] / norm(Vh[, h], "2")
      th <- X %*% Vh[, h]
      if (all(dplyr::near(Th[, h], th))) break
      Th[, h] <- th
    }
    X <- X - Th[, h] %*% t(Vh[, h])
  }

  return(list(T = Th, V = Vh))
}



#' 주성분 분해
#'
#' 주어진 데이터에 대하여 주성분을 분해한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .xvar 독립변수. 독립변수가 여러 개일 때는 벡터 형태로 제공한다. (e.g. \code{c(age, height)}).
#'   Default: \code{everthing()}, \code{.data}의 모든 변수에 대한 주성분 분해를 수행한다.
#' @param .pc 추출할 주성분의 수.
#' @param .center 평균조정 여부. TRUE이면 각 독립변수에 대해 평균조정을 수행한다.
#' @param .scale 분산조정 여부. TRUE이면 각 독립변수에 대해 분산조정을 수행한다.
#' @return 리스트. 고유치 \code{eig}, 주성분행렬 \code{score},
#'         상관로딩 \code{loading}, 주성분 수 \code{ncomp}, 주성분 변동 기여율 \code{R2},
#'         평균조정 시 평균벡터 \code{center}, 분산조정 시 표본표준편차 벡터 \code{scale}.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit1 <- fit_pca(biometric, c(age, height, weight), .pc = 2L)
#' fit2 <- fit_pca(biometric, .pc = 2L)
#'
#' @keywords principal-component-analysis
#' @export
fit_pca <- function(.data, .xvar = everything(), .pc = NULL, .center = TRUE, .scale = TRUE) {
  .xvar <- rlang::enquo(.xvar)
  variables <- tidyselect::eval_select(.xvar, .data) %>% names()

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

  col_ss <- colSums(X ** 2)
  total_ss <- sum(col_ss)

  Xpc <- nipals_pca(X, .pc)
  ncomp <- ncol(Xpc[["V"]])

  pc_eigen <- colSums(Xpc[["T"]] ** 2) / col_ss[seq_len(ncomp)]
  pc_col_ss <- colSums(Xpc[["T"]] ** 2)
  pc_total_ss <- sum(pc_col_ss)

  res <- list(
    eig = pc_eigen,
    score = Xpc[["T"]] %>%
      magrittr::set_colnames(paste0("PC", seq_len(ncomp))),
    loadings = Xpc[["V"]] %>%
      magrittr::set_colnames(paste0("PC", seq_len(ncomp))) %>%
      magrittr::set_rownames(variables),
    ncomp = ncomp,
    R2 = pc_col_ss / total_ss,
    center = x_center,
    scale = x_scale
  )

  return(res)
}


#' 주성분 스코어 계산.
#'
#' 주어진 주성분 분해 결과를 이용하여 새 데이터에 대해 주성분 스코어를 계산한다.
#'
#' @param .fit 주성분 분해 결과.
#' @param .new_data 새 관측 데이터 프레임.
#' @return 주성분 스코어 데이터프레임.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit <- fit_pca(biometric, .pc = 2L)
#' predict_pca(fit, biometric)
#'
#' @keywords principal-component-analysis
#' @export
predict_pca <- function(.fit, .new_data) {
  .xvar <- rownames(.fit$loadings)

  X <- .new_data %>%
    dplyr::select(!!.xvar) %>%
    as.matrix() %>%
    scale(center = .fit$center, scale = .fit$scale)

  res <- tibble::as_tibble(X %*% .fit$loadings)

  return(res)
}



#' 주성분 회귀분석
#'
#' 주어진 데이터에 대하여 주성분 회귀모형을 추정한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .yvar 종속변수.
#' @param .xvar 독립변수. 독립변수가 여러 개일 때는 벡터 형태로 제공한다. (e.g. \code{c(age, height)}).
#' @param .pc 추출할 주성분의 수.
#' @param .center 평균조정 여부. TRUE이면 각 독립변수에 대해 평균조정을 수행한다.
#' @param .scale 분산조정 여부. TRUE이면 각 독립변수에 대해 분산조정을 수행한다.
#' @return 리스트. 주성분을 독립변수로 하여 수행한 \code{fit_linear_regression()}의 결과값.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit <- fit_pcr(biometric, weight, c(age, height), .pc = 1L)
#'
#' @keywords principal-component-analysis
#' @export
fit_pcr <- function(.data, .yvar, .xvar, .pc = NULL, .center = TRUE, .scale = TRUE) {
  .xvar <- rlang::enquo(.xvar)
  .yvar <- rlang::enquo(.yvar)

  pc_fit <- fit_pca(.data, !!.xvar, .pc, .center, .scale)

  X <- dplyr::as_tibble(pc_fit[["score"]])

  reg_data <- .data %>%
    dplyr::select(!!.yvar) %>%
    dplyr::bind_cols(X)

  lm_fit <- fit_linear_regression(reg_data, !!.yvar, -!!.yvar)

  res <- c(pc_fit, lm_fit)

  intercept <- ((-t(pc_fit$center / pc_fit$scale) %*% pc_fit$loadings) %*%
    lm_fit$betas[colnames(pc_fit$loadings)]) %>%
    `+`(lm_fit$betas["(Intercept)"]) %>%
    drop()

  betas <- (1 / pc_fit$scale) *
    drop(pc_fit$loadings %*% lm_fit$betas[colnames(pc_fit$loadings)])

  res[["org_betas"]] <- c("(Intercept)" = intercept, betas)

  return(res)
}


#' 주성분 회귀분석 반응치 예측.
#'
#' 주성분 회귀모형 추정 이후 새로운 데이터에 대한 반응치를 예측한다.
#'
#' @param .fit 주성분 회귀모형 추정 결과.
#' @param .new_data 새 관측 데이터 프레임.
#' @param ... 함수 \code{predict_linear_regression()} 수행 시 사용할 파라미터 값.
#' @return 예측값 데이터 프레임.
#'
#' @examples
#' data(biometric, package = "dmtr")
#' fit <- fit_pcr(biometric, weight, c(age, height), .pc = 1L)
#' predict_pcr(fit, biometric)
#'
#' @keywords principal-component-analysis
#' @export
predict_pcr <- function(.fit, .new_data, ...) {
  predicted_score <- predict_pca(.fit, .new_data)
  res <- predict_linear_regression(.fit, predicted_score, everything(), ...)

  return(res)
}

