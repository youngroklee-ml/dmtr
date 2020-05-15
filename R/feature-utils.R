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
