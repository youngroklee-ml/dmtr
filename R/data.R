#' Jane Austen Books.
#'
#' Jane Austen의 6개의 소설의 텍스트를 저장한 데이터 프레임.
#' 각 소설 텍스트는 약 70개의 문자 단위로 나뉘어 행을 구성한다.
#'
#' @format 2개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{text}{텍스트}
#'   \item{book}{소설 제목}
#' }
#'
#' @source \code{\link[janeaustenr]{austen_books}}
"austenbooks"


#' 두 범주 테스트 표본 1.
#'
#' 두 범주 학습표본 1 (\code{\link{binaryclass1_train}})을 이용해 학습된 분류규칙을 적용하기 위한 새 표본
#'
#' @format 2개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{id}{객체 고유번호}
#'   \item{x1, x2}{두 변수}
#' }
"binaryclass1_test"


#' 두 범주 학습표본 표본 1.
#'
#' 두 개의 범주값과 두 개의 변수를 지닌 학습표본.
#'
#' @format 7개의 행과 4개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{id}{객체 고유번호}
#'   \item{x1, x2}{두 변수}
#'   \item{class}{범주 (1 혹은 2)}
#' }
"binaryclass1_train"


#' 두 범주 학습표본 2.
#'
#' 두 개의 범주값과 두 개의 변수를 지닌 학습표본.
#'
#' @format 9개의 행과 4개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{id}{객체 고유번호}
#'   \item{x1, x2}{두 변수}
#'   \item{class}{범주 (1 혹은 2)}
#' }
"binaryclass2"


#' 두 범주 테스트 표본 3.
#'
#' 두 범주 학습표본 3 (\code{\link{binaryclass3_train}})을 이용해 학습된 분류규칙을 적용하기 위한 새 표본
#'
#' @format 6개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{x1, x2}{두 변수}
#'   \item{class}{범주 (1 혹은 2)}
#' }
"binaryclass3_test"


#' 두 범주 학습표본 표본 3.
#'
#' 두 개의 범주값과 두 개의 변수를 지닌 학습표본.
#'
#' @format 10개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{x1, x2}{두 변수}
#'   \item{class}{범주 (1 혹은 2)}
#' }
"binaryclass3_train"


#' 두 범주 학습표본 표본 4 - 선형분리불가능.
#'
#' 두 개의 범주값과 두 개의 변수를 지닌 학습표본. 해당 표본은 선형 분류기법으로는 두 범주가 완전히 분리되지 않는다.
#'
#' @format 10개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{x1, x2}{두 변수}
#'   \item{class}{범주 (1 혹은 -1)}
#' }
"binaryclass4_inseparable"


#' 두 범주 학습표본 표본 4 - 비선형분리.
#'
#' 두 개의 범주값과 두 개의 변수를 지닌 비선형분리기법을 위한 학습표본.
#'
#' @format 9개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{x1, x2}{두 변수}
#'   \item{class}{범주 (1 혹은 -1)}
#' }
"binaryclass4_nonlinear"


#' 두 범주 학습표본 표본 4 - 선형분리가능.
#'
#' 두 개의 범주값과 두 개의 변수를 지닌 학습표본. 해당 표본은 선형 분류기법으로 두 범주가 완전히 분리된다.
#'
#' @format 9개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{x1, x2}{두 변수}
#'   \item{class}{범주 (1 혹은 -1)}
#' }
"binaryclass4_separable"


#' 두 범주 학습표본 표본 5.
#'
#' 두 개의 범주값과 하나의 변수를 지닌 학습표본.
#'
#' @format 10개의 행과 2개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{x}{변수}
#'   \item{y}{범주 (0 혹은 1)}
#' }
"binaryclass5"


#' 나이, 키, 몸무게 학습표본.
#'
#' 나이와 키로써 몸무게를 설명하는 회귀모형을 추정하기 위한 학습표본.
#'
#' @format 10개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{age}{나이}
#'   \item{height}{키(cm)}
#'   \item{weight}{몸무게(kg)}
#' }
"biometric"


#' 군집분석 데이터 1.
#'
#' 두 개의 실수형 변수로 이루어진 객체 데이터
#'
#' @format 7개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{id}{객체 고유번호}
#'   \item{x1, x2}{두 변수}
#' }
"clusterdata1"


#' 군집분석 데이터 2.
#'
#' 두 개의 실수형 변수로 이루어진 객체 데이터
#'
#' @format 6개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{id}{객체 고유번호}
#'   \item{x1, x2}{두 변수}
#' }
"clusterdata2"


#' 군집분석 데이터 3.
#'
#' 두 개의 실수형 변수로 이루어진 객체 데이터
#'
#' @format 7개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{id}{객체 고유번호}
#'   \item{x1, x2}{두 변수}
#' }
"clusterdata3"


#' 군집분석 데이터 4.
#'
#' 두 개의 실수형 변수로 이루어진 객체 데이터
#'
#' @format 8개의 행과 3개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{id}{객체 고유번호}
#'   \item{x1, x2}{두 변수}
#' }
"clusterdata4"


#' 고객 새 표본.
#'
#' 고객 학습표본 \code{\link{customerclass_train}}을 이용해 학습된 분류규칙을 적용하여 분류를 예측하고자 하는 새 고객 표본.
#'
#' @format 1개의 행과 2개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{x1}{성별 (남 혹은 여)}
#'   \item{x2}{나이 (10대, 20대, 30대, 40대)}
#' }
"customerclass_test"


#' 고객 학습표본.
#'
#' 두 개의 범주값과 두 개의 변수를 지닌 학습표본.
#'
#' @format 9개의 행과 4개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{id}{고객 고유번호}
#'   \item{x1}{성별 (남 혹은 여)}
#'   \item{x2}{나이 (10대, 20대, 30대, 40대)}
#'   \item{y}{범주 (1 혹은 2)}
#' }
"customerclass_train"


#' 공정변수-불량 종류 데이터
#'
#' 3종류의 불량을 2개의 공정변수로 예측하는 모형을 학습하기 위한 데이터.
#'
#' @format 18개의 행과 4개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{id}{관측 고유번호}
#'   \item{x1, x2}{두 공정변수}
#'   \item{y}{불량 종류 (1, 2, 3)}
#' }
"defecttype"


#' 국내 증권회사의 주요 재무제표
#'
#' 국내 18개 증권회사의 주요 재무제표
#'
#' @format 18개의 행과 6개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{company}{증권회사명}
#'   \item{roa}{총자본 순이익율}
#'   \item{roe}{자기자본 순이익율}
#'   \item{bis}{자기자본비율}
#'   \item{de_ratio}{부채비율}
#'   \item{turnover}{자기자본 회전율}
#' }
"financials"


#' 집단별 관측빈도.
#'
#' \code{\link{gainchart_yfreq}}의 1000개의 객체들에 대해 어떤 분류모형을 이용하여 범주 1(특정범주)에 대한
#' 사후확률을 산출한 후 그 확률의 내림차순으로 전체 객체를 정렬하여 매 100개의 객체를 한 집단으로 구분하였을 때, 각 집단에 속하는 범주
#' 1의 빈도.
#'
#' @format 10개의 행과 2개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{k}{집단 번호 (1, 2, ..., 10)}
#'   \item{n}{범주 1의 빈도}
#' }
"gainchart_group1"


#' 실제범주별 관측빈도.
#'
#' 전체 객체 수가 1,000인 어떤 데이터의 실제 범주별 빈도.
#'
#' @format 3개의 행과 2개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{y}{범주 (1, 2, 3)}
#'   \item{n}{관측수}
#' }
"gainchart_yfreq"


#' 건강 문진
#'
#' 3명에 대한 건강 관련 문진에 대한 답을 나타낸 자료
#'
#' @format 3개의 행과 6개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{id}{환자 고유번호}
#'   \item{x1}{운동여부 (예: 1, 아니오: 0)}
#'   \item{x2}{음주여부 (예: 1, 아니오: 0)}
#'   \item{x3}{흡연여부 (예: 1, 아니오: 0)}
#'   \item{x4}{가족력여부 (예: 1, 아니오: 0)}
#'   \item{x5}{고혈압여부 (예: 1, 아니오: 0)}
#' }
"healthbehavior"


#' 붓꽃 데이터.
#'
#' \code{\link[datasets]{iris}}의 150개 데이터 중 각 범주 별 30개의 관측치를 모아 총 90개의 관측치로 구성한
#' 데이터.
#'
#' @format 90개의 행과 6개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{id}{관측치 고유번호}
#'   \item{x1}{Sepal.Length}
#'   \item{x2}{Sepal.Width}
#'   \item{x3}{Petal.Length}
#'   \item{x4}{Petal.Width}
#'   \item{class}{범주 (setosa, versicolor, virginica)}
#' }
#'
#' @source \code{\link[datasets]{iris}}
"iris90"


#' 다종속변수 학습표본.
#'
#' 두 개의 종속변수를 지닌 학습표본.
#'
#' @format 7개의 행과 6개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{x1, x2, x3, x4}{4개의 독립변수}
#'   \item{y1, y2}{2개의 종속변수}
#' }
"multiplsreg"


#' 주성분 회귀분석 학습표본.
#'
#' 하나의 종속변수와 세 개의 독립변수를 가진 학습표본.
#'
#' @format 6개의 행과 4개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{x1, x2, x3}{3개의 독립변수}
#'   \item{y}{종속변수}
#' }
"pcrdata"


#' PC 사용 데이터 1.
#'
#' 가정에서 PC를 사용하는 10명에 대한 나이, PC 경험연수, 주당 사용시간을 나타낸 데이터.
#'
#' @format 10개의 행과 4개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{id}{사용자 고유번호}
#'   \item{x1}{나이}
#'   \item{x2}{PC 경험연수}
#'   \item{x3}{주당 사용시간}
#' }
"pcusage1"


#' PC 사용 데이터 2.
#'
#' 10명의 사람(객체)에 대한 PC 사용경력과 주당 사용시간을 나타낸 데이터.
#'
#' @format 10개의 행과 3개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{id}{사용자 고유번호}
#'   \item{x1}{PC 사용경력}
#'   \item{x2}{주당 사용시간}
#' }
"pcusage2"


#' 시장바구니 구매 데이터.
#'
#' 5명의 고객 각각이 7개의 상품 중 구매한 상품을 나타내는 데이터.
#'
#' @format 3개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{customer}{고객 고유번호}
#'   \item{item}{상품 고유번호}
#'   \item{purchase}{구매여부 (1: 구매)}
#' }
"purchasedata_reference"


#' 시장바구니 목표고객 구매 데이터.
#'
#' \code{\link{purchasedata_reference}}의 시장바구니 데이터를 이용하여 상품 추천을 제공할 목표고객의 기존 구매 데이터.
#'
#' @format 3개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{customer}{고객 고유번호}
#'   \item{item}{상품 고유번호}
#'   \item{purchase}{기존구매여부 (1: 구매)}
#' }
"purchasedata_target"


#' 시장바구니 평점 데이터.
#'
#' 6명의 고객 각각이 7개의 상품 중 구매한 상품에 대해 매긴 평점을 나타내는 데이터.
#'
#' @format 3개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{customer}{고객 고유번호}
#'   \item{item}{상품 고유번호}
#'   \item{rating}{평점}
#' }
"ratingdata_reference"


#' 시장바구니 목표고객 평점 데이터.
#'
#' \code{\link{ratingdata_reference}}의 시장바구니 데이터를 이용하여 상품 추천을 제공할 목표고객의 기존 평점 데이터.
#'
#' @format 3개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{customer}{고객 고유번호}
#'   \item{item}{상품 고유번호}
#'   \item{rating}{평점}
#' }
"ratingdata_target"


#' 열연코일 인장강도 데이터.
#'
#' 열연코일의 인장강도(TS)에 권취온도(CT)가 어떤 영향을 미치는가를 조사하기 위한 학습표본.
#'
#' @format 10개의 행과 3개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{ct}{권취온도}
#'   \item{thickness}{두께 (2 혹은 6, 단위: mm) - 범주형(factor) 변수}
#'   \item{ts}{인장강도}
#' }
"rollsteel"


#' 학생 평점 학습표본.
#'
#' 기숙사에 거주하는 대학생 15명을 대상으로 얻은 생활습관에 대한 설문조사 결과와 평점(이분형 범주 - 우수/보통).
#'
#' @format 15개의 행과 5개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{id}{학생 고유번호}
#'   \item{x1}{아침식사 여부 (예: 1, 아니오: 0)}
#'   \item{x2}{수면시간}
#'   \item{x3}{서클활동시간}
#'   \item{y}{평점범주 (우수 혹은 보통)}
#' }
"student"


#' 성능변수에 따른 통신 만족도.
#'
#' 벨 연구소에서 한 통신잔치에 대하여 실시한 조사 결과. 주요 성능변수인 회선잡음(circuit noise: N)과 소리크기
#' 손실(loudness loss: L)이 이용자의 주관적인 만족도에 미치는 영향을 분석하기 위한 것이다. 만족도는 원결과(Cavanaugh,
#' 1976)를 가공하여 세 범주로 분류하였다.
#'
#' @format 12개의 행과 3개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{N}{회선잡음}
#'   \item{L}{소리크기 손실}
#'   \item{y}{만족도 범주 - 서열형(ordinal) (나쁨: 1, 보통: 2, 좋음: 3)}
#' }
#'
#' @source \url{https://doi.org/10.1002/j.1538-7305.1976.tb02939.x}
"telconnection"


#' 통신서비스 만족도.
#'
#' 3명의 고객별 통신서비스 만족도. 혼합형 객체 데이터의 예.
#'
#' @format 3개의 행과 6개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{id}{고객 고유번호}
#'   \item{x1}{성별 - 이분형 (여 혹은 남)}
#'   \item{x2}{나이 - 연속형}
#'   \item{x3}{직업 - 명목형}
#'   \item{x4}{월통신요금 - 연속형}
#'   \item{x5}{통신서비스 만족도 - 서열형 (1: 매우만족, 2: 만족, 3: 보통, 4: 불만족, 5: 매우불만족)}
#' }
"telsatisfaction"


#' 통신서비스 만족도 변수 범위.
#'
#' \code{\link{telsatisfaction}}의 연속형 및 서열형 변수들에 대한 값의 범위 (최소/최대).
#'
#' @format 2개의 행과 4개의 열을 지닌 데이터 프레임:
#' \describe{
#'   \item{range}{"min": 최소, "max": 최대}
#'   \item{x2, x4, x5}{각 변수에 해당하는 최소/최대값}
#' }
"telsatisfaction_range"


#' 운전경력에 따른 사고횟수.
#'
#' 8명에 대한 운전경력과 교통위반 횟수를 나타낸 표본.
#'
#' @format 8개의 행과 3개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{id}{운전자 고유번호}
#'   \item{x1}{운전경력}
#'   \item{x2}{위반횟수}
#' }
"trafficviolation"


#' 트랜잭션.
#'
#' 총 5개의 트랜잭션에서 각 트랜잭션에 포함된 항목들을 나타낸 long-form 데이터 프레임.
#'
#'  @format 21개의 행과 2개의 열로 이루어진 데이터 프레임:
#'  \describe{
#'    \item{transaction_id}{트랜잭션 고유번호}
#'    \item{item}{항목}
#'  }
"transactiondata"


#' 고객 시퀀스.
#'
#' 5명의 고객들의 시간에 따른 구매 행태를 나타낸 long-form 데이터 프레임.
#'
#' @format 16개의 행과 3개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{customer_id}{고객 고유번호}
#'   \item{transaction_seq}{고객별 트랜잭션 순서}
#'   \item{item}{항목}
#' }
"transactionseq"


#' 문서별 단어 TF-IDF 가중치.
#'
#' 5개 중요 단어의 문서 6건에 대한 TF-IDF 가중치.
#'
#' @format 30개의 행과 3개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{document}{문서 고유번호}
#'   \item{word}{단어 고유번호}
#'   \item{tfidf}{TF-IDF 가중치}
#' }
"wordtfidf"


#' 사용자 단어 가중치.
#'
#' \code{\link{wordtfidf}}의 5개 중요 단어에 대하여 평가된 목표사용자의 가중치. 본 가중치와
#' \code{\link{wordtfidf}}의 각 문서별 가중치를 비교하여 목표사용자에게 문서를 추천하고자 하는 목적이다.
#'
#' @format 5개의 행과 2개의 열로 이루어진 데이터 프레임:
#' \describe{
#'   \item{word}{단어 고유번호}
#'   \item{weight}{목표사용자 가중치}
#' }
"wordweight"
