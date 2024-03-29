% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic-regression.R
\name{loglik_multinom_logistic_regression}
\alias{loglik_multinom_logistic_regression}
\title{명목형 로지스틱 회귀분석 로그우도함수.}
\usage{
loglik_multinom_logistic_regression(.betas, .x, .y, .reflevel)
}
\arguments{
\item{.betas}{계수 행렬. 각 열은 범주를 나타내고 각 행은 변수를 나타낸다.
기준범주 열은 생략되어, 범주 개수보다 하나 적은 열을 지닌다.}

\item{.x}{독립변수 행렬.}

\item{.y}{종속변수 벡터.}

\item{.reflevel}{기준범주 값.}
}
\value{
로그우도함수값.
}
\description{
주어진 계수로부터 관측 데이터가 얻어질 우도함수값을 계산한다.
}
\keyword{logistic-regression}
