library(knitr)
#Distribution

#1. 정규분포(normal distribution)
x <- seq(-5, 5, length = 101)

#정규분포의 확률 밀도 함수
y <- (1/sqrt(2*pi))*exp(-x^2/2)
plot(x, y, type = "l", col = "red")

y1 <- dnorm(x, mean = 0, sd = 1)
plot(x, y1, type = "l", col = "red")

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
