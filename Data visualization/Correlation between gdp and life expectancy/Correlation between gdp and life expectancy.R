knitr::opts_chunk$set(echo = TRUE)
library(extrafont)
library(knitr)

## GDP 데이터 불러오기
gdp <- read.csv("GDP with continent.csv", stringsAsFactors = FALSE)
names(gdp) <- c("Country_Name", "Country_Code", "Continent", "GDP", "OECD")
gdp$GDP <- as.double(gdp$GDP)
gdp$GDP <- round(gdp$GDP, digits = 2)
str(gdp)

## 기대수명 데이터 불러오기
life_exp <- read.csv("life expectancy with continent.csv", stringsAsFactors = FALSE)
names(life_exp) <- c("Country_Name", "Country_Code", "continent", "Life_Expectation", "OECD")
life_exp$Life_Expectation <- round(life_exp$Life_Expectation, digits = 1)
str(life_exp)

## 기대수명과 GDP 데이터 합치기
gdp_life <- data.frame(Country_name = gdp$Country_Name,
                       Continent = gdp$Continent,
                       GDP = gdp$GDP,
                       Life_Expectation = life_exp$Life_Expectation,
                       OECD = life_exp$OECD)
str(gdp_life)
par(family = "KoPubWorldDotum Medium")
kable(gdp_life, align = c(rep('c', 5)))


## GDP와 기대수명의 평균 계산
gdp_mean <- mean(gdp_life$GDP)
gdp_mean
life_mean <- round(mean(gdp_life$Life_Expectation), digit = 2)
life_mean

## GDP와 기대수명의 상관계수 계산
cor <- round(cor(gdp_life$GDP, gdp_life$Life_Expectation), digit = 4)
cor

## 산점도의 점 색깔 벡터 만들기
point_color <- c("#2B3A1C", "#0080FF", "#FA605A", "#A901DB", "#848484", "#3ADF00", "#DBA901")

## 아프리카 국가들의 plot
par(family = "KoPubWorldDotum Medium")
gdp_life_Africa <- subset(gdp_life, gdp_life$Continent == "Africa")
gdp_mean_africa <- mean(gdp_life_Africa$GDP)
life_mean_africa <- mean(gdp_life_Africa$Life_Expectation)
plot(x = gdp_life_Africa$GDP, y = gdp_life_Africa$Life_Expectation,
     main = "아프리카 국가별 1인당 GDP와 기대수명의 상관관계",
     xlab = "국가별 1인당 GDP (단위 : $)", ylab = "국가별 기대수명",
     pch = 16, cex = 1, col = point_color[1], grid())
abline(h=life_mean, lty =4, col = "red")
text(12500, life_mean-1, labels="↑ 전 세계 평균 기대수명", cex = 0.7)
abline(h=life_mean_africa, lty =4, col = "blue")
text(12500, 65.6, labels = "↓ 아프리카 평균 기대수명", cex = 0.7 )
abline(v=gdp_mean, lty=4, col="red")
text(gdp_mean-1600, 60, labels="전 세계 평균 GDP →", cex = 0.7)
abline(v=gdp_mean_africa, lty=4, col="blue")
text(gdp_mean_africa + 1700, 57 , labels = " ← 아프리카 평균 GDP", cex = 0.7)

## 아시아 국가들의 plot
par(family = "KoPubWorldDotum Medium")
gdp_life_Asia <- subset(gdp_life, gdp_life$Continent == "Asia")
gdp_mean_asia <- mean(gdp_life_Asia$GDP)
life_mean_asia <- mean(gdp_life_Asia$Life_Expectation)
plot(x = gdp_life_Asia$GDP, y = gdp_life_Asia$Life_Expectation,
     main = "아시아 국가별 1인당 GDP와 기대수명의 상관관계",
     xlab = "국가별 1인당 GDP (단위 : $)", ylab = "국가별 기대수명",
     pch = 16, cex = 1, col = point_color[2], grid())
abline(h=life_mean, lty =4, col = "red")
text(65000, life_mean-1, labels="↑ 전 세계 평균 기대수명", cex = 0.7)
abline(h=life_mean_asia, lty =4, col = "blue")
text(65000, life_mean+2.65, labels = "↓ 아시아 평균 기대수명", cex = 0.7)
abline(v=gdp_mean, lty=4, col="red")
text(gdp_mean+9000, 69, labels="← 전 세계 평균 GDP", cex = 0.7)
abline(v=gdp_mean_asia, lty=4, col="blue")
text(6000, 82.5, labels = "아시아 평균 GDP →", cex = 0.7)

## 유럽 국가들의 plot
par(family = "KoPubWorldDotum Medium")
gdp_life_Europe <- subset(gdp_life, gdp_life$Continent == "Europe")
gdp_mean_europe <- mean(gdp_life_Europe$GDP)
life_mean_europe <- mean(gdp_life_Europe$Life_Expectation)
plot(x = gdp_life_Europe$GDP, y = gdp_life_Europe$Life_Expectation,
     main = "유럽 국가별 1인당 GDP와 기대수명의 상관관계",
     xlab = "국가별 1인당 GDP (단위 : $)", ylab = "국가별 기대수명",
     pch = 16, cex = 1, col = point_color[3], grid())
abline(h=life_mean, lty =4, col = "red")
text(140000, life_mean+0.7, labels="↓ 전 세계 평균 기대수명", cex = 0.7)
abline(h=life_mean_europe, lty =4, col = "blue")
text(140000, life_mean_europe - 0.7, labels = "↑ 유럽 평균 기대수명", cex = 0.7)
abline(v=gdp_mean, lty=4, col="red")
text(gdp_mean+16000, 73.5, labels="← 전 세계 평균GDP", cex = 0.7)
abline(v=gdp_mean_europe, lty=4, col="blue")
text(49000, 76.5, labels = "← 유렵 평균 GDP", cex = 0.7)

## 중동 국가들의 plot
par(family = "KoPubWorldDotum Medium")
gdp_life_MiddleEast <- subset(gdp_life, gdp_life$Continent == "Middle East")
gdp_mean_middleeast <- mean(gdp_life_MiddleEast$GDP)
life_mean_middleeast <- mean(gdp_life_MiddleEast$Life_Expectation)
plot(x = gdp_life_MiddleEast$GDP, y = gdp_life_MiddleEast$Life_Expectation,
     main = "중동 국가별 1인당 GDP와 기대수명의 상관관계",
     xlab = "국가별 1인당 GDP (단위 : $)", ylab = "국가별 기대수명",
     pch = 16, cex = 1, col = point_color[4], grid())
abline(h=life_mean, lty =4, col = "red")
text(55000, life_mean-1, labels="↑ 전 세계 평균 기대수명", cex = 0.7)
abline(h=life_mean_middleeast, lty =4, col = "blue")
text(55000, life_mean_middleeast + 1, labels = "↓ 중동 평균 기대수명", cex = 0.7)
abline(v=gdp_mean, lty=4, col="red")
text(gdp_mean-7000, 68, labels="전 세계 평균 GDP →", cex = 0.7)
abline(v=gdp_mean_middleeast, lty=4, col="blue")
text(gdp_mean_middleeast+6400, 68, labels = "← 중동 평균 GDP", cex = 0.7)

## 북미 국가들의 plot
par(family = "KoPubWorldDotum Medium")
gdp_life_NorthAmerica <- subset(gdp_life, gdp_life$Continent == "North America")
gdp_mean_NorthAmerica <- mean(gdp_life_NorthAmerica$GDP)
life_mean_NorthAmerica <- mean(gdp_life_NorthAmerica$Life_Expectation)
plot(x = gdp_life_NorthAmerica$GDP, y = gdp_life_NorthAmerica$Life_Expectation,
     main = "북미 국가별 1인당 GDP와 기대수명의 상관관계",
     xlab = "국가별 1인당 GDP (단위 : $)", ylab = "국가별 기대수명",
     pch = 16, cex = 1, col = point_color[5], grid())
abline(h=life_mean, lty =4, col = "red")
abline(h=life_mean_NorthAmerica, lty =4, col = "blue")
text(65000, life_mean_NorthAmerica - 0.5, labels = "↑ 북미 평균 기대수명", cex = 0.7)
abline(v=gdp_mean, lty=4, col="red")
text(gdp_mean-7800, 76.5, labels="전 세계 평균 GDP →", cex = 0.7)
abline(v=gdp_mean_NorthAmerica, lty=4, col="blue")
text(38000, 75, labels = "← 북미 평균 GDP", cex = 0.7)

## 오세아니아 국가들의 plot
par(family = "KoPubWorldDotum Medium")
gdp_life_Oceania <- subset(gdp_life, gdp_life$Continent == "Oceania")
gdp_mean_Oceania <- mean(gdp_life_Oceania$GDP)
life_mean_Oceania <- mean(gdp_life_Oceania$Life_Expectation)
plot(x = gdp_life_Oceania$GDP, y = gdp_life_Oceania$Life_Expectation,
     main = "오세아니아 국가별 1인당 GDP와 기대수명의 상관관계",
     xlab = "국가별 1인당 GDP (단위 : $)", ylab = "국가별 기대수명",
     pch = 16, cex = 1, col = point_color[6], grid())
abline(h=life_mean, lty =4, col = "red")
text(40000, life_mean+1, labels="↓ 전 세계 평균 기대수명", cex = 0.7)
abline(h=life_mean_Oceania, lty =4, col = "blue")
text(40000, life_mean_Oceania-1, labels = "↑ 오세아니아 평균 기대수명", cex = 0.7)
abline(v=gdp_mean, lty=4, col="red")
text(gdp_mean+5500, 66, labels="← 전 세계 평균 GDP", cex = 0.7)
abline(v=gdp_mean_Oceania, lty=4, col="blue")
text(6400, 77.5, labels = "오세아니아 평균 GDP →", cex = 0.7)

## 남미 국가들의 plot
par(family = "KoPubWorldDotum Medium")
gdp_life_SouthAmerica <- subset(gdp_life, gdp_life$Continent == "South America")
gdp_mean_SouthAmerica <- mean(gdp_life_SouthAmerica$GDP)
life_mean_SouthAmerica <- mean(gdp_life_SouthAmerica$Life_Expectation)
plot(x = gdp_life_SouthAmerica$GDP, y = gdp_life_SouthAmerica$Life_Expectation,
     main = "남미 국가별 1인당 GDP와 기대수명의 상관관계",
     xlab = "국가별 1인당 GDP", ylab = "국가별 기대수명",
     pch = 16, cex = 1, col = point_color[7], grid())
abline(h=life_mean, lty =4, col = "red")
text(22000, life_mean-0.8, labels="↑ 전 세계 평균 기대수명", cex = 0.7)
abline(h = life_mean_SouthAmerica, lty = 4, col = "blue")
text(22000, 75.3, labels = "↓ 남미 평균 기대수명", cex = 0.7)
abline(v=gdp_mean, lty=4, col="red")
text(gdp_mean+3350, 66, labels="← 전 세계 평균 GDP", cex = 0.7)
abline(v=gdp_mean_SouthAmerica, lty=4, col="blue")
text(7000, 66, labels = "남미 평균 GDP →", cex = 0.7)

## OECD 국가들의 plot
par(family = "KoPubWorldDotum Medium")
gdp_life_OECD <- subset(gdp_life, gdp_life$OECD == TRUE)
gdp_mean_OECD <- mean(gdp_life_OECD$GDP)
life_mean_OECD <- mean(gdp_life_OECD$Life_Expectation)
plot(x = gdp_life_OECD$GDP, y = gdp_life_OECD$Life_Expectation,
     main = "OECD 국가별 1인당 GDP와 기대수명의 상관관계",
     xlab = "국가별 1인당 GDP (단위 : $)", ylab = "국가별 기대수명",
     pch = 16, cex = 1, col = "#F7786B", grid())
abline(h=life_mean, lty =4, col = "red")
abline(h=life_mean_OECD, lty =4, col = "blue")
text(88000, 80.15, labels = "↑ OECD 평균 기대수명", cex = 0.7)
abline(v=gdp_mean, lty=4, col="red")
text(gdp_mean+11600, 75.5, labels="← 전 세계 평균 GDP", cex = 0.7)
abline(v=gdp_mean_OECD, lty=4, col="blue")
text(51000, 75.5, labels = "← OECD 평균 GDP", cex = 0.7)

## plot을 이용해 전체 국가들의 산점도 그리기
par(family = "KoPubWorldDotum Medium")
plot(x=gdp_life$GDP, y=gdp_life$Life_Expectation, grid(), 
     main = "국가별 1인당 GDP와 기대수명의 상관관계",
     xlab = "국가별 1인당 GDP (단위 : $)", ylab = "국가별 기대수명",
     col = point_color[gdp_life$Continent], pch = 16, cex=0.7)

#x축의 범위를 0부터 150,000까지 2,5000 씩 증가시켜 표시
axis(side = 1, at=c(seq(0, 150000, by=25000), 170000))

#기대수명의 평균을 가로선으로 표시
abline(h=life_mean, lty =4, col = "red")
text(140000, life_mean+2, labels="↓ 평균 기대수명", cex = 0.7)

# GDP의 평균을 세로선으로 표시
abline(v=gdp_mean, lty=4, col="red")
text(gdp_mean+11000, 55, labels="← 평균 GDP", cex = 0.7)

# 범례 표시
continent <- c("아프리카", "아시아", "유럽", "중동", "북아메리카", "오세아니아", "남아메리카") 
legend("bottomright",
       continent,
       col = point_color, pch = 16, cex=0.75)

#그래프 하단에 상관계수 표시
text(80000, 58, labels="상관 계수 cor = 0.6466", cex = 1.2)

## log화 그래프를 그리기 위해 데이터 전처리
gdp_life$GDP <- log10(gdp_life$GDP)
gdp_mean_log <- round(mean(gdp_life$GDP), digit = 2)

## ggplot으로 로그화된 그래프 그리기
library(ggplot2)
#데이터를 불러와 산점도 그리기
g1 <- ggplot(data = gdp_life, mapping = aes(x = GDP, y = Life_Expectation)) + 
  geom_point()

#배경 추가
g2 <- g1 + theme_bw()

#그래프의 제목, x축 제목, y축 제목, 주석 추가
g3 <- g2 + labs(title = "국가별 1인당 GDP와 기대수명의 상관관계 (log화)", 
                x = "log화된 국가별 1인당 GDP", y = "국가별 기대수명", 
                caption = "자료 출처 : WorldBank")

#위에서 설정한 텍스트의 크기, 글씨체, 위치 설정
g4 <- g3 + theme(plot.title = element_text(family = "KoPubWorldDotum Medium", size = 15, 
                                           color =  "navy", hjust = 0.5, vjust = 2.5, face = "bold"),
                 axis.title.x = element_text(family = "KoPubWorldDotum Medium", size = 10),
                 axis.title.y = element_text(family = "KoPubWorldDotum Medium", size = 10, 
                                             vjust = 3))

#GDP와 기대수명의 평균을 나타내는 수직선, 수평선 추가
g5 <- g4 + geom_vline(xintercept = gdp_mean_log, linetype = 2, col = "red", size = 0.6) +
  geom_hline(yintercept = life_mean, linetype = 2, 
             col = "red", size = 0.6)

#X축과 Y축의 범위 설정
g6 <- g5 + 
  scale_x_continuous(breaks = c(seq(2, 5.5, by = 0.5), gdp_mean_log)) +
  scale_y_continuous(breaks = c(seq(55, 85, by = 5), life_mean))

#대륙별로 점의 색깔을 구분하여 표시
g7 <- g6 + geom_point(aes(col = Continent), size = 1.5) + scale_color_manual(values = point_color)

#범례와 텍스트 표시
g8 <- g7 + geom_text(aes(x = 4.25, y = 57, label = "상관계수 cor = 0.6466", 
                         family = "KoPubWorldDotum Medium")) +
  geom_text(aes(x = 3.45, y = 81.7, label = "log화 된 평균 GDP →", 
                family = "KoPubWorldDotum Medium")) +
  geom_text(aes(x = 2.65, y = 74.4, label = "↓ 평균 기대수명", 
                family = "KoPubWorldDotum Medium"))

#범례의 위치 조정
g9 <- g8 + theme(legend.background = element_rect(linetype = "solid", color = "black"),
                 legend.title = element_blank(),
                 legend.position = c(0.901, 0.275))
g9
