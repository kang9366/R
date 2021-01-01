library(leaflet)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(dygraphs)
library(plotly)


data <- read.csv("./data/강원도CCTV.csv", fileEncoding = "cp949")
str(data)
data$시군구명 %<>% as.factor()
data$설치목적구분 %<>% as.factor()
data$설치년월 %<>% as.factor()
data$촬영방면정보 %<>% as.factor()

#시군 별 cctv 설치 현황
data1 <- data %>% 
  group_by(시군구명) %>% 
  summarise(num = sum(카메라대수))a

ggplot(data1, aes(x=시군구명, y=num)) + 
  geom_bar(stat="identity", fill='#BAD252') +
  theme_wsj(base_family='NanumGothic') +
  geom_text(aes(y=num, label = num), colour="black", size=4, vjust=0.5)

#설치 목적 별 cctv 개수
data2 <- data %>% 
  group_by(설치목적구분) %>% 
  summarise(num = sum(카메라대수))

ggplot(data2, aes(x=설치목적구분, y=num)) + 
  geom_bar(stat="identity", fill='#BAD252') +
  theme_wsj(base_family='NanumGothic') +
  geom_text(aes(y=num, label = num), colour="black", size=4, vjust=0.5)


#촬영 방면 별 cctv 개수
data4 <- data %>% 
  group_by(촬영방면정보) %>% 
  summarise(num = sum(카메라대수)) %>% 
  as.data.frame() %>% 
  arrange(., desc(num)) %>% 
  head(n=14)
data4 <- data4[-3,]

ggplot(data4, aes(x=촬영방면정보, y=num)) + 
  geom_bar(stat="identity", fill='#F29661') +
  theme_wsj(base_family='NanumGothic') +
  geom_text(aes(y=num, label = num), colour="black", size=4, vjust=0.5) +
  theme(axis.text.x=element_text(size=9, angle=-10)) +
  labs(x="")

#시계열 그래프
dygraph(ts(data3$s1)) %>% 
  dyRangeSelector()

#범죄율과 cctv 설치 수의 관계
y <- c()
for(i in 2011:2018){
  year <- data3 %>% 
    filter(substr(설치년월, 1, 4)==i) %>% 
    summarise(s = sum(s1))
  y <- c(y, year$s)
}

crime <- read.csv("./data/crime data.csv", fileEncoding = "cp949")
crime <- crime[2,c(4:11)] %>% 
  t() %>% 
  as.integer()
data5 <- data.frame(year = c(2011:2018),
                    cctv = y,
                    crime = crime)
ggplot() +
  geom_line(data5, mapping = aes(x=year, y=cctv, group=1, colour="#ED5D47")) +
  geom_point(data5, mapping = aes(x=year, y=cctv)) +
  geom_line(data5, mapping = aes(x=year, y=crime/10, group=1, colour="#BAD252")) +
  geom_point(data5, mapping = aes(x=year, y=crime/10)) +
  labs(colour = "", x="", y="") +
  scale_color_hue(labels = c("범죄 횟수", "CCTV 설치 수")) +
  theme_wsj(base_family = "NanumGothic") +
  theme(legend.title = element_text()) +
  scale_x_continuous(breaks = c(2011:2018))

#지도 시각화
leaflet(data) %>% 
  setView(lng=128.150, lat=37.450, zoom = 8) %>% 
  addTiles() %>% 
  addCircles(lng=~경도, lat=~위도)
