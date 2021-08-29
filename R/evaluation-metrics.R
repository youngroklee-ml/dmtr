#' 평균절대오차
#'
#' 예측값의 평균절대오차를 계산한다.
#'
#' @param .y 관측 종속변수 값. 숫자형 벡터.
#' @param .y_hat 예측 종속변수 값. 숫자형 벡터.
#' @return 평균절대오차.
#'
#' @keywords evaluation-metrics
#' @export
eval_mad <- function(.y, .y_hat) {
  mean(abs(.y - .y_hat))
}

#' 평균제곱오차
#'
#' 예측값의 평균제곱오차를 계산한다.
#'
#' @param .y 관측 종속변수 값. 숫자형 벡터.
#' @param .y_hat 예측 종속변수 값. 숫자형 벡터.
#' @return 평균제곱오차.
#'
#' @keywords evaluation-metrics
#' @export
eval_mse <- function(.y, .y_hat) {
  mean((.y - .y_hat) ^ 2)
}

#' 평균제곱오차의 제곱근
#'
#' 예측값의 평균제곱오차의 제곱근을 계산한다.
#'
#' @param .y 관측 종속변수 값. 숫자형 벡터.
#' @param .y_hat 예측 종속변수 값. 숫자형 벡터.
#' @return 평균제곱오차의 제곱근.
#'
#' @keywords evaluation-metrics
#' @export
eval_rmse <- function(.y, .y_hat) {
  sqrt(eval_mse(.y, .y_hat))
}
