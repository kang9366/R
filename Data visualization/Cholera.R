library(HistData)
library(ggplot2)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(gridExtra)

#Cholera$district %<>% as.factor(.)
#Cholera$region %<>% as.factor(.)
str(Cholera)

state <- levels(Cholera$region)

west <- Cholera %>% 
  filter(region==state[1])
sum_w <- sum(west$cholera_deaths)
sum_pw <- sum(west$poor_rate)
mean_hpw <- mean(west$house_val)

north <- Cholera %>% 
  filter(region==state[2])
sum_n <- sum(north$cholera_deaths)
sum_pn <- sum(north$poor_rate)
mean_hpn <- mean(north$house_val)

central <- Cholera %>% 
  filter(region==state[3])
sum_c <- sum(central$cholera_deaths)
sum_pc <- sum(central$poor_rate)
mean_hpc <- mean(central$house_val)

south <- Cholera %>% 
  filter(region==state[4])
sum_s <- sum(south$cholera_deaths)
sum_ps <- sum(south$poor_rate)
mean_hps <- mean(south$house_val)

kent <- Cholera %>%
  filter(region==state[5])
sum_k <- sum(kent$cholera_deaths)
sum_pk <- sum(kent$poor_rate)
mean_hpk <- mean(kent$house_val)

Cholera <- data.frame(region=as.factor(state), 
                      death=c(sum_w, sum_n, sum_c, sum_s, sum_k), 
                      poor.rate = c(sum_pw, sum_pn, sum_pc, sum_ps, sum_pk),
                      mean.housePrice = c(mean_hpw, mean_hpn, mean_hpc, mean_hps, mean_hpk))

g1 <- ggplot(Cholera, aes(region,death,fill=region)) + 
  geom_bar(stat="identity")
g2 <- ggplot(Cholera, aes(region, poor.rate, fill=region)) + 
  geom_bar(stat = "identity") + 
  theme(legend.position = "none")
grid.arrange(g1, g2, nrow=1, ncol=2)


coordinate_hp <- function(i){
  x <- Cholera$mean.housePrice[i]
  y <- Cholera$poor.rate[i] + 0.05
  if(i==1){
    y <- Cholera$poor.rate[i] - 0.05
  }  
  return(c(x,y))
}
coordinate_d <- function(i){
  x <- Cholera$death[i]
  y <- Cholera$poor.rate[i] + 0.05
  if(i==5){
    y <- Cholera$poor.rate[i] - 0.05
  }  
  return(c(x,y))
}

par(mfrow=c(1,2))
color <- c("skyblue", "black", "red", "brown", "pink") 
color <- color[as.numeric(Cholera$region)]

plot(Cholera$poor.rate~Cholera$mean.housePrice, pch=19, cex=2.3, col=color,
     main = "지역에 따른 집의 평균 가격과 가난한 사람의 비율의 관계",
     xlab="평균 집 가격", ylab="가난한 사람의 비율")
text(coordinate_hp(1)[1], coordinate_hp(1)[2], label="West", cex=0.8)
text(coordinate_hp(2)[1], coordinate_hp(2)[2], label="North", cex=0.8)
text(coordinate_hp(3)[1], coordinate_hp(3)[2], label="Central", cex=0.8)
text(coordinate_hp(4)[1], coordinate_hp(4)[2], label="South", cex=0.8)
text(coordinate_hp(5)[1], coordinate_hp(5)[2], label="Kent", cex=0.8)
#abline(v=mean(Cholera$mean.housePrice), lty=4)
#abline(h=mean(Cholera$poor.rate), lty=4)

plot(Cholera$poor.rate~Cholera$death, pch=19, cex=2.3, col=color, 
     main = "지역에 따른 콜레라 사망자 수와 가난한 사람의 비율의 관계",
     xlab="사망자 수", ylab="가난한 사람의 비율")
text(coordinate_d(1)[1], coordinate_d(1)[2], label="West", cex=0.8)
text(coordinate_d(2)[1], coordinate_d(2)[2], label="North", cex=0.8)
text(coordinate_d(3)[1], coordinate_d(3)[2], label = "Central", cex=0.8)
text(coordinate_d(4)[1], coordinate_d(4)[2], label = "South", cex=0.8)
text(coordinate_d(5)[1], coordinate_d(5)[2], label = "Kent", cex=0.8)
#abline(v=mean(Cholera$death), lty=4)
#abline(h=mean(Cholera$poor.rate), lty=4)

ggplot(HistData::Cholera, aes(x=cholera_drate, y=poor_rate, color=region)) + 
  geom_point(size=4)#, pch=c(0, 2, 4, 8, 10)[HistData::Cholera$region])

proportion <- Cholera %>% 
  arrange(desc(region)) %>%
  mutate(prop = death / sum(Cholera$death) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

ggplot(proportion, aes(x="", y=prop, fill=region)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start = 0) + 
  theme_void()+
  labs(fill="") + 
  geom_text(aes(y=ypos, label=paste0(round(prop, digit=2), "%", "\n", "(", death, "명)")))

