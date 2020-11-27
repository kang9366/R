library(knitr)
#Distribution

#1. 정규분포(normal distribution)
x <- seq(-5, 5, length = 101)

#정규분포의 확률 밀도 함수
y <- (1/sqrt(2*pi))*exp(-x^2/2)
plot(x, y, type = "l", col = "red")

y1 <- dnorm(x, mean = 2, sd = 1)
plot(x, y1, type = "l", col = "red")

y2 <- dnorm(x, mean=0, sd=4)
plot(x, y2, type="l", col="red")

#평균 : 정규분포 확률밀도함수의 최대값의 x좌표
#표준편차가 커지면 정규분포 확률밀도 함수의 첨도가 작아지고 표준편차가 작아지면 첨도가 커진다(반비례 관계)

pnorm(1, mean=2, sd=1)

#n이 5일때
y1 <- rbinom(5, size=5, prob=0.5)
hist(y1)

#n이 10일 때
y2 <- rbinom(10, size=10, prob=0.5)
hist(y2)

#n이 30일 때
y3 <- rbinom(0:30, size=30, prob=0.5)
hist(y3)

#n이 100일 때
y4 <- rbinom(100, size=100, prob=0.5)
hist(y4)

#n이 5일때
y1 <- rbinom(5, size=5, prob=0.1)
hist(y1)

#n이 10일 때
y2 <- rbinom(10, size=10, prob=0.1)
hist(y2)

#n이 30일 때
y3 <- rbinom(0:30, size=30, prob=0.1)
hist(y3)

#n이 100일 때
y4 <- rbinom(100, size=100, prob=0.1)
hist(y4)

#n이 증가할 수록 히스토그램의 형태가 정규분포에 가까워진다.
#prob가 증가하면 이항분포의 기댓값의 x좌표(그래프의 최고점의 x좌표)가 x축의 양의방향으로 이동한다.

#2. t분포(t-distribution)
#모평균과 모표준편차를 모르는 정규모집단에서 표본크기가 작은 경우에 모평균에 대한 추정과 검정에 t통계량을 이용한다.
#정규모집단으로부터 크기가 n인 표본을 무작위로 추출했을 때 표본통계량 t는 자유도 n-1인 t분포를 따른다.
t <- seq(-5, 5, length.out = 101)
v <- 10
y <-(1/sqrt(pi*v))*(factorial((v+1)/2-1)/factorial(v/2-1))*(1+t^2/v)^(-(v+1)/2)
plot(t, y, type = "l", col = "red")

y1 <- dt(t, v)
plot(t, y1, type = "l", col = "red")

min_value <- 2
me <- min_value %>% as.integer(runif(10, . ,10))
