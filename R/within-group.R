#' 범주별 데이터 프레임. Group nest.
#'
#' 각 범주별 데이터 분석을 위한 nested data frame을 만든다. \code{group_nest} returns nested
#' data frame that each row represents variable within each group.
#'
#' @param .data 관측 데이터 프레임. A raw data frame.
#' @param .group_var 범주변수. A variable to group by.
#' @param .xvar 범주별 데이터 프레임에 포함될 변수. One or more variables separated by commas within \code{c()}.
#'   Each variable needs to be a numeric column of \code{.data}.
#' @return 두 개의 컬럼을 지닌 데이터 프레임으로, 첫 번째 컬럼은 범주변수 \code{.group_var}이며, 두 번째 컬럼
#'   \code{.new_col}는 각 범주 내의 관측객체들에 대한 데이터 프레임으로, \code{.xvar}에 속한 변수들을 컬럼으로 지닌다.
#'
#' @keywords within-group
group_nest <- function(.data, .group_var, .xvar) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  res <- .data %>%
    dplyr::select(!!.group_var, !!.xvar) %>%
    tidyr::nest(.new_col = c(!!.xvar))

  return(res)
}


#' 범주별 평균벡터. Group mean.
#'
#' 각 범주별 평균벡터를 추정한다. \code{group_mean} returns mean of each variable within each
#' group.
#'
#' 이 함수는 데이터 프레임 \code{.data} 내의 범주변수 \code{.group_var}의 범주별로 \code{.xvar}에 주어진
#' 변수들에 대한 관측 데이터의 평균을 범주별 평균벡터로 추정한다. This function aggregated \code{.data}
#' into \code{.group_var} level to have a column \code{.group_var} as group
#' variable and columns \code{.xvar} that represent group mean value of
#' \code{.xvar} in the original data \code{.data}.
#'
#' @param .data 관측 데이터 프레임. A raw data frame.
#' @param .group_var 범주변수. A variable to group by.
#' @param .xvar 평균벡터에 포함될 변수. One or more variables separated by commas within \code{c()}. Each variable needs to
#'   be a numeric column of \code{.data}.
#' @return 리스트 형태로, 각 리스트의 원소는 각 범주의 평균벡터를 나타낸다. A list of mean vector, that each list element represent each group,
#'   and has a vector of mean of \code{.xvar}.
#'
#' @examples
#' data(binaryclass2, package = "dmtr")
#' group_mean(binaryclass2, class, c(x1, x2))
#'
#' @keywords within-group
#' @export
group_mean <- function(.data, .group_var, .xvar) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  nested_df <- group_nest(.data, !!.group_var, !!.xvar)

  res <- nested_df %>%
    dplyr::pull(.data$.new_col) %>%
    purrr::map(~ purrr::map_dbl(.x, base::mean))

  attr(res, "group") <- nested_df %>% dplyr::pull(!!.group_var)

  return(res)
}

#' 범주별 표본 분산-공분산행렬. Within-group variance-covariance matrix.
#'
#' 각 범주에 속하는 객체들에 대한 표분 분산-공분산행렬을 구한다. \code{group_variance} returns a list of
#' covariance matrices each of which represents covariance matrix within each
#' group.
#'
#' 이 함수는 데이터 프레임 \code{.data} 내의 범주변수 \code{.group_var}의 범주별로 \code{.xvar}에 주어진
#' 변수들에 대한 관측 데이터의 표본 분산-공분산 행렬을 범주별 분산-공분산행렬로 추정한다. This function aggregated
#' \code{.data} into \code{.group_var} level to have a column \code{.group_var}
#' as group variable and columns \code{.xvar} that represent group mean value of
#' \code{.xvar} in the original data \code{.data}.
#'
#' @param .data 관측 데이터 프레임. A raw data frame.
#' @param .group_var 범주변수. A variable to group by.
#' @param .xvar 분산-공분산 행렬에 포함될 변수. One or more variables separated by commas within \code{c()}. Each
#'   variable needs to be a numeric column of \code{.data}.
#' @return 리스트 형태로, 각 리스트의 원소는 각 범주의 표본 분산-공분산 행렬을 나타낸다. A list of covariance
#'   matrix
#'
#' @examples
#' data(binaryclass2, package = "dmtr")
#' group_variance(binaryclass2, class, c(x1, x2))
#'
#' @keywords within-group
#' @export
group_variance <- function(.data, .group_var, .xvar) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  nested_df <- group_nest(.data, !!.group_var, !!.xvar)

  res <- nested_df %>%
    dplyr::pull(.data$.new_col) %>%
    purrr::map(stats::var)

  attr(res, "group") <- nested_df %>% dplyr::pull(!!.group_var)

  return(res)
}

#' 범주별 통계치. Group summary.
#'
#' 범주별 관측객체수, 평균벡터, 분산-공분산행렬 등 통계치를 계산한다. \code{group_summary} returns a list of statistics for each group.
#'
#' 이 함수는 데이터 프레임 \code{.data} 내의 범주변수 \code{.group_var}의 범주별로 \code{.xvar}에 주어진
#' 변수들에 대한 통계치를 계산하여 리스트로 제공한다.
#'
#' @param .data 관측 데이터 프레임. A raw data frame.
#' @param .group_var 범주변수. A variable to group by.
#' @param .xvar 관심 변수. One or more variables separated by commas within \code{c()}. Each variable
#'   needs to be a numeric column of \code{.data}.
#' @return 리스트 형태로, 각 리스트의 원소는 각 범주에 대한 리스트를 지닌다.
#'
#' @examples
#' data(binaryclass2, package = "dmtr")
#' group_summary(binaryclass2, class, c(x1, x2))
#'
#' @keywords within-group
#' @export
group_summary <- function(.data, .group_var, .xvar) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  nested_df <- group_nest(.data, !!.group_var, !!.xvar)

  res <- purrr::map(
    nested_df %>% dplyr::pull(.data$.new_col),
    ~ list(
      n = base::nrow(.x),
      mean = purrr::map_dbl(.x, base::mean),
      sigma = stats::var(.x)
    )
  )

  attr(res, "group") <- nested_df %>% dplyr::pull(!!.group_var)

  return(res)
}

#' 합동 분산-공분산행렬. Pooled variance.
#'
#' 분산-공분산행렬이 범주에 관계없이 동일하다고 가정하고, 합동 분산-공분산행렬을 범주별로 주어진 관측치를 이용하여 추정한다.
#' \code{pooled_variance} returns common variance.
#'
#' @param .data 관측 데이터 프레임. A raw data frame.
#' @param .group_var 범주변수. A variable to group by.
#' @param .xvar 분산-공분산 행렬에 포함될 변수. One or more variables separated by commas
#'   within \code{c()}. Each variable needs to be a numeric column of
#'   \code{.data}.
#' @return 하나의 분산-공분산 행렬. A covariance matrix.
#'
#' @examples
#' data(binaryclass2, package = "dmtr")
#' pooled_variance(binaryclass2, class, c(x1, x2))
#'
#' @keywords within-group
#' @export
pooled_variance <- function(.data, .group_var, .xvar) {
  .group_var <- rlang::enquo(.group_var)
  .xvar <- rlang::enquo(.xvar)

  summ <- group_summary(.data, !!.group_var, !!.xvar)

  numerator <- purrr::map(summ, ~ (.[["n"]] - 1) * (.[["sigma"]])) %>%
    purrr::reduce(`+`)

  denominator <- purrr::map_int(summ, ~ (.[["n"]] - 1L)) %>% base::sum()

  res <- numerator / denominator

  return(res)
}


