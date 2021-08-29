#' 피셔 선형 판별 함수.
#'
#' 두 범주 데이터를 구분하는 피셔 선형 판별함수의 계수를 추정한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .group_var 범주변수.
#' @param .xvar 범주 분류에 사용될 변수.
#' @return 선형 함수의 계수 벡터.
#'
#' @examples
#' data(binaryclass2, package = "dmtr")
#' fisher_ld(binaryclass2, class, c(x1, x2))
#'
#' @keywords discriminant-functions
#' @export
fisher_ld <- function(.data, .group_var, .xvar) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  mu_hat <- group_mean(.data, !!.group_var, !!.xvar)

  if (length(mu_hat) != 2L) {
    stop("number of levels of .group_var must be 2.")
  }

  sigma_hat <- pooled_variance(.data, !!.group_var, !!.xvar)

  res <- solve(sigma_hat) %*%
    as.matrix(mu_hat[[1]] - mu_hat[[2]], ncol = 1L) %>%
    as.vector()

  names(res) <- names(mu_hat[[1]])
  attr(res, "group") <- attr(mu_hat, "group")

  return(res)
}


#' 피셔 선형 판별 함수 분류 경계값.
#'
#' 두 범주 데이터를 구분하는 피셔 선형 판별함수의 경계값을 구한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .group_var 범주변수.
#' @param .xvar 범주 분류에 사용될 변수.
#' @return 선형 함수의 분류 경계값.
#'
#' @examples
#' data(binaryclass2, package = "dmtr")
#' fisher_ld_threshold(binaryclass2, class, c(x1, x2))
#'
#' @keywords discriminant-functions
#' @export
fisher_ld_threshold <- function(.data, .group_var, .xvar) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  w_hat <- fisher_ld(.data, !!.group_var, !!.xvar)
  mean_vec <- .data %>% dplyr::select(!!.xvar) %>% colMeans()

  res <- sum(w_hat * mean_vec)

  return(res)
}


#' 피셔 선형 판별 함수 예측값.
#'
#' 새로운 데이터에 대해 피셔 선형 판별함수를 이용하여 범주를 분류한다.
#'
#' @param .w 선형 함수 계수.
#' @param .z 선형 함수 분류 경계치.
#' @param .newdata 새 데이터.
#' @param .xvar 범주 분류에 사용될 변수.
#' @param .levels 범주.
#' @return 분류 예측값을 포함한 데이터 프레임.
#'
#' @keywords discriminant-functions
#' @examples
#' data(binaryclass2, package = "dmtr")
#' w <- fisher_ld(binaryclass2, class, c(x1, x2))
#' z <- fisher_ld_threshold(binaryclass2, class, c(x1, x2))
#' pred <- fisher_ld_prediction(w, z, binaryclass2, c(x1, x2), .levels = attr(w, "group"))
#'
#' @export
fisher_ld_prediction <- function(.w, .z, .newdata, .xvar, .levels = c(1L, 2L)) {
  .xvar <- rlang::enquo(.xvar)

  .newdata %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      z = crossprod(c(!!.xvar), .w),
      .pred_class = dplyr::if_else(z > .z, .levels[1], .levels[2])
    )
}


#' 범주별 판별함수 - 선형 판별 분석.
#'
#' 범주별 판별함수를 범주별 평균벡터와 합동 분산-공분산행렬을 이용하여 정의한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .group_var 범주변수.
#' @param .xvar 범주 분류에 사용될 변수.
#' @param .prior 범주 사전 확률. NULL일 때는 데이터 프레임 \code{.data}내의 각 범주의 비율을 사전확률로 추정.  default: NULL
#' @return 범주별 판별 함수.
#'
#' @keywords discriminant-functions
#' @examples
#' data(binaryclass2, package = "dmtr")
#' f <- ld_fun(binaryclass2, class, c(x1, x2))
#'
#' @export
ld_fun <- function(.data, .group_var, .xvar, .prior = NULL) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  summ <- group_summary(.data, !!.group_var, !!.xvar)

  if (is.null(.prior)) {
    .prior <- purrr::map_dbl(summ, ~ .x[["n"]] / nrow(.data))
  }

  stopifnot(length(attr(summ, "group")) == length(.prior))

  sigma_hat <- pooled_variance(.data, !!.group_var, !!.xvar)
  sigma_hat_inv <- solve(sigma_hat)

  fn <- purrr::map2(
    summ, .prior,
    ~ function(x) {
    if (is.list(x)) x <- unlist(x)
    if (is.vector(x)) x <- matrix(x, ncol = 1L)

    mu_hat <- matrix(.x[["mean"]], ncol = 1L)

    res <- t(mu_hat) %*% sigma_hat_inv %*% x -
      1 / 2 * t(mu_hat) %*% sigma_hat_inv %*%  mu_hat +
      log(.y)

    drop(res)
  })

  attr(fn, "group") <- attr(summ, "group")

  return(fn)
}

#' 범주별 판별함수값 계산.
#'
#' 새 데이터에 대해 범주별 판별함수값을 계산한다.
#'
#' @param .f 판별함수 리스트.
#' @param .new_data 새 관측 데이터 프레임.
#' @param .xvar 범주 함수에 사용될 입력 변수.
#' @return 새 관측 데이터에 대한 판별함수 값을 포함한 데이터 프레임.
#'
#' @examples
#' data(binaryclass2, package = "dmtr")
#' f <- ld_fun(binaryclass2, class, c(x1, x2))
#' score_da(f, binaryclass2, c(x1, x2))
#'
#' @keywords discriminant-functions
#' @export
score_da <- function(.f, .new_data, .xvar) {
  .xvar <- rlang::enquo(.xvar)

  u_df <- purrr::map2_dfc(.f, attr(.f, "group"),
    function(f, class, xvar, data) {
      xvar <- rlang::enquo(xvar)
      uvar <- rlang::sym(stringr::str_c("u", class))

      data %>%
        dplyr::select(!!xvar) %>%
        dplyr::transmute(!!uvar := apply(., 1, f))
    },
    xvar = !!.xvar,
    data = .new_data
  )

  names(u_df) <- stringr::str_c(".score", attr(.f, "group"), sep = "_")

  return(u_df)
}

#' 새 데이터에 대한 범주 예측.
#'
#' 범주별 판별함수를 이용하여 새 데이터에 대한 범주를 예측한다.
#'
#' @param .f 판별함수 리스트.
#' @param .new_data 새 관측 데이터 프레임.
#' @param .xvar 범주 분류에 사용될 변수.
#' @param .include_score TRUE이면 범주별 판별함수값을 결과에 저장. default = FALSE.
#' @param .include_posterior TRUE이면 사후확률값을 결과에 저장. default = FALSE.
#' @param .include_class TRUE이면 추정범주값을 결과에 저장. default = TRUE.
#' @return 데이터 프레임: 범주 추정.
#'
#' @examples
#' data(binaryclass2, package = "dmtr")
#' f <- ld_fun(binaryclass2, class, c(x1, x2))
#' predict_da(f, binaryclass2, c(x1, x2), .include_posterior = TRUE)
#'
#' @keywords discriminant-functions
#' @export
predict_da <- function(.f, .new_data, .xvar,
  .include_score = FALSE,
  .include_posterior = FALSE,
  .include_class = TRUE
) {
  .xvar <- rlang::enquo(.xvar)

  if (!any(.include_score, .include_posterior, .include_class)) {
    stop('at least one of followings needs to be TRUE: \n
      .include_score, .include_posterior, .include_class')
  }

  u_df <- score_da(.f, .new_data, !!.xvar)

  p_df <- u_df %>%
    dplyr::mutate_all(exp) %>%
    dplyr::mutate_all(function(x) x / rowSums(.))

  names(p_df) <- stringr::str_c(".pred", attr(.f, "group"), sep = "_")

  yhat_df <- dplyr::tibble(
    .pred_class = attr(.f, "group")[apply(p_df, 1, which.max)]
  )

  res <- NULL
  if (.include_score) res <- dplyr::bind_cols(res, u_df)
  if (.include_posterior) res <- dplyr::bind_cols(res, p_df)
  if (.include_class) res <- dplyr::bind_cols(res, yhat_df)

  return(res)
}


#' 범주별 판별함수 - 이차 판별 분석.
#'
#' 범주별 판별함수를 범주별 평균벡터와 범주별 분산-공분산행렬을 이용하여 정의한다.
#'
#' @param .data 관측 데이터 프레임.
#' @param .group_var 범주변수.
#' @param .xvar 범주 분류에 사용될 변수.
#' @param .prior 범주 사전 확률. NULL일 때는 데이터 프레임 \code{.data}내의 각 범주의 비율을 사전확률로 추정.  default: NULL
#' @return 범주별 판별 함수.
#'
#' @examples
#' data(binaryclass2, package = "dmtr")
#' f <- qd_fun(binaryclass2, class, c(x1, x2))
#'
#' @keywords discriminant-functions
#' @export
qd_fun <- function(.data, .group_var, .xvar, .prior = NULL) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  summ <- group_summary(.data, !!.group_var, !!.xvar)

  if (is.null(.prior)) {
    .prior <- purrr::map_dbl(summ, ~ .x[["n"]] / nrow(.data))
  }

  stopifnot(length(attr(summ, "group")) == length(.prior))

  fn <- purrr::map2(summ, .prior, ~ function(x) {
    if (is.list(x)) x <- unlist(x)
    if (is.vector(x)) x <- matrix(x, ncol = 1L)

    mu_hat <- matrix(.x[["mean"]], ncol = 1L)
    sigma_hat <- .x[["sigma"]]
    sigma_hat_inv <- solve(sigma_hat)

    res <- - 1 / 2 * t(mu_hat - x) %*% sigma_hat_inv %*% (mu_hat - x) -
      1 /2 * log(det(sigma_hat)) + log(.y)

    drop(res)
  })

  attr(fn, "group") <- attr(summ, "group")

  return(fn)
}

