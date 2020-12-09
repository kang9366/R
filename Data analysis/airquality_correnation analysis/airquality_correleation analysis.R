install.packages("PerformanceAnalytics")
install.packages("corrplot")

library(PerformanceAnalytics)
library(corrplot)

#airquality data : 1973년 뉴욕의 공기의 질을 측정한 데이터로 Ozon, Solar.R, Wind, Temp 4개의 변수로 구성되어 있다.
head(airquality)

#month와 day를 제외한 변수들을 aq1에 대입
aq1 <- airquality[,c(1:4)]

#na값들로 인해 상관분석의 결과가 유의미하지 않다.
cor(aq1)

#aq1의 결측치들을 제거한 뒤 aq2에 대입
aq2 <- na.omit(aq1)
aq.cor <- cor(aq2)
aq.cor

#coefficient of determination : 결정계수
#-1.0과 -0.7 사이이면, 강한 음적 선형관계,
#-0.7과 -0.3 사이이면, 뚜렷한 음적 선형관계,
#-0.3과 -0.1 사이이면, 약한 음적 선형관계,
#-0.1과 +0.1 사이이면, 거의 무시될 수 있는 선형관계,
#+0.1과 +0.3 사이이면, 약한 양적 선형관계,
#+0.3과 +0.7 사이이면, 뚜렷한 양적 선형관계,
#+0.7과 +1.0 사이이면, 강한 양적 선형관계

plot(aq2)
pairs(aq2, panel = panel.smooth)
chart.Correlation(aq2, histogram = TRUE, pch = 19)

corrplot(aq.cor, method = "number")

summary(aq2$Ozone)
summary(aq2$Solar.R)
summary(aq2$Wind)
summarry(aq2$Temp)
