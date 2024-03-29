% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/principal-component-analysis.R
\name{predict_pcr}
\alias{predict_pcr}
\title{주성분 회귀분석 반응치 예측.}
\usage{
predict_pcr(.fit, .new_data, ...)
}
\arguments{
\item{.fit}{주성분 회귀모형 추정 결과.}

\item{.new_data}{새 관측 데이터 프레임.}

\item{...}{함수 \code{predict_linear_regression()} 수행 시 사용할 파라미터 값.}
}
\value{
예측값 데이터 프레임.
}
\description{
주성분 회귀모형 추정 이후 새로운 데이터에 대한 반응치를 예측한다.
}
\examples{
data(biometric, package = "dmtr")
fit <- fit_pcr(biometric, weight, c(age, height), .pc = 1L)
predict_pcr(fit, biometric)

}
\keyword{principal-component-analysis}
