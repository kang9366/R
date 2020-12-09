library(ggplot2)
library(magrittr)
library(dplyr)
library(ggthemes)

data1 <- read.csv("./data/movie2.csv", fileEncoding = "cp949")
data2 <- read.csv("./data/movie1.csv", fileEncoding = "cp949")

#1
genre <- table(data2$genre) %>% 
  as.data.frame() %>% 
  arrange(., desc(Freq)) %>% 
  head(., n = 10)

ggplot(genre, aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity") + 
  ggtitle("2000개 영화 순위 중 상위 10개 장르") + 
  labs(x="", y="") +
  theme_solarized() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5, size = 20))


#2
data1_na <- data1 %>% 
  na.omit()
data1_na$audience_female %<>% 
  gsub("%", "", .) %>% 
  as.integer()
data1_na$audience_male %<>% 
  gsub("%", "", .) %>% 
  as.integer()

par(mfrow=c(1,2))
boxplot(data1_na$audience_male, xlab = "남성 비율", col = "#CAB388")
boxplot(data1_na$audience_female, xlab="여성 비율", col = "#B7A6AD")

#3
actor_num <- data2$actor %>% 
  strsplit(.,",")

num <- c()
for(i in 1:length(actor_num)){
  num <- c(num, length(actor_num[[i]]))
}

actor <- data.frame(movie = data2$title,
                    actorNum = num)

actor <- table(actor$actorNum) %>% 
  as.data.frame()

ggplot(actor, aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") +
  ggtitle("주연 배우의 수") +
  labs(x="배우의 수", y="") +
  theme_solarized() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=20))

#4
runtime <- data2$runtime %>% 
  gsub("분", "", .) %>% 
  as.integer()

par(mfrow=c(1,1))
boxplot(runtime, col="#F1AB00",main="상영시간의 분포")

#5
score <- data.frame(netizen = data2$netizen_score,
                    audience = data2$audience_score) %>% 
  na.omit()

random <- sample(c(1:nrow(score)), 100)
score <- score[random,]
cor(score$netizen, score$audience)
ggplot(score, aes(x=netizen, y=audience)) + 
  geom_point() +
  geom_smooth(method=lm, fullrange=TRUE) + 
  theme_solarized() + 
  labs(x="네티즌 평점", y="관람객 평점") + 
  ggtitle("네티즌 평점과 관람객 평점의 관계") + 
  theme(plot.title = element_text(hjust = 0.5, size=20))