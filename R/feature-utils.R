#' 범주(factor)형 벡터를 행렬로 변환.
#'
#' 범주(factor)형 벡터를 범주 수만큼의 열을 지닌 행렬로 변환한다.
#'
#' @param x 범주형 벡터.
#' @param .reflevel 기준 범주값. NULL일 때는 one-hot encoding. NULL이 아닌 값일 때는 해당 범주는
#'   모든 열에 대해 0의 값을 가지도록 dummy variable encoding.
#' @return 행렬.
#'
#' @examples
#' x <- factor(c(1L, 2L, 3L, 1L), levels = c(1L, 2L, 3L))
#' factor_to_matrix(x)
#' factor_to_matrix(x, .reflevel = 3L)
#'
#' @keywords feature-utils
#' @export
factor_to_matrix <- function(x, .reflevel = NULL) {
  n_obs <- length(x)
  n_levels <- nlevels(x)

  mat <- purrr::map(levels(x),
    ~ vector("integer", length = n_obs) %>%
      `[<-`(which(x == .x), 1L)
  ) %>%
    unlist()

  res <- matrix(mat, nrow = n_obs, ncol = n_levels)
  colnames(res) <- levels(x)

  if (!is.null(.reflevel)) {
    res <- res[, levels(x) != .reflevel]
  }

  return(res)
}


#' 비음행렬의 값을 확률로 변환.
#'
#' 행렬의 각 행이 합계 1의 값을 갖도록 값을 변환.
#'
#' @param p 실수형 행렬. 모든 값은 0 이상이어야 한다.
#' @param small_p 값 변환 전 행렬 \code{p}의 모든 원소값에 더할 작은 양의 실수.
#' @return 행렬.
#'
#' @examples
#' x <- matrix(runif(15), nrow = 5)
#' normalize_to_prob(x)
#' normalize_to_prob(x, small_p = 0)
#'
#' @keywords feature-utils
#' @export
normalize_to_prob <- function(p, small_p = NULL) {
  if (any(p < 0)) stop("All elements of p must be non-negative.")
  if (is.null(small_p)) small_p <- sqrt(.Machine$double.eps)
  if (small_p < 0) stop("A value of small_p must be either NULL or non-negative.")

  p <- p + small_p
  p <- p / rowSums(p)

  return(p)
}




