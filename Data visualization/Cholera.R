library(HistData)
library(ggplot2)
library(magrittr)
library(dplyr)
library(RColorBrewer)

cholera <- Cholera
str(cholera)


cs <- read.csv("Cholera.csv")
cholera$district %<>% as.factor(.)
barplot(c(cholera$district, cholera$cholera_deaths))
mosaicplot(cholera)
levels(cholera$region)
cholera <- cholera[,c(3,6,12)]
barplot(c(cholera$cholera_deaths, cholera$region))

levels(cholera$region)

z <- function(x, q, a = min(x), b = max(x)) {
  (q - a) / (b - a)
}
china <- subset(cs, cs[1] == 'China')
usa <- subset(cs, cs[1] == 'United States of America')
germany <- subset(cs, cs[1] == 'Germany')
france <- subset(cs, cs[1] == 'France')
japan <- subset(cs, cs[1] == 'Japan')

china_z <- china$Number.of.reported.cases.of.cholera %>% z(.,.)
usa_z <- usa$Number.of.reported.cases.of.cholera %>% z(.,.)
germany_z <- germany$Number.of.reported.cases.of.cholera %>% z(.,.)
france_z <- france$Number.of.reported.cases.of.cholera %>% z(.,.)
japan_z <- japan$Number.of.reported.cases.of.cholera %>% z(.,.)

plot(china$Year, china_z, type='l', col="red")
plot(france$Year, france_z, type='l', col='purple', xlab="year", ylab="death")
lines(usa$Year, usa_z, type='l', col="blue")
lines(germany$Year, germany_z, type='l', col='brown')
lines(japan$Year, japan_z, type='l', col='orange')

sum_k <- 0
sum_w <- 0
sum_c <- 0
sum_s <- 0
sum_n <- 0
sum_pk <- 0
sum_pw <- 0
sum_pn <- 0
sum_pc <- 0
sum_ps <- 0
for(i in 1:nrow(cholera)){
  if(cholera[i,2]=="Kent"){
    sum_k <- sum_k + cholera[i,1]
    sum_pk <- sum_pk + cholera[i,3] 
  }else if(cholera[i,2]=="West"){
    sum_w <- sum_w + cholera[i,1]
    sum_pw <- sum_pw + cholera[i,3]
  }else if(cholera[i,2] == "North"){
    sum_n <- sum_n + cholera[i,1]
    sum_pn <- sum_pn + cholera[i,3]
  }else if(cholera[i,2] == "Central"){
    sum_c <- sum_c + cholera[i,1]
    sum_pc <- sum_pc + cholera[i,3]
  }else{
    sum_s <- sum_s + cholera[i,1]
    sum_ps <- sum_ps + cholera[i,3]
  }
}

cholera <- data.frame(region=levels(cholera$region), death=c(sum_w, sum_n, sum_c, sum_s, sum_k), 
                      poor.rate = c(sum_pw, sum_pn, sum_pc, sum_ps, sum_pk))

r <- function(i){
  return (cholera[i,2]/sum(cholera$death))
}
cholera[,4] <- c(r(1), r(2), r(3), r(4), r(5))

ggplot(cholera, aes(region,death)) + geom_bar(stat="identity")
ggplot(cholera, aes(region, poor.rate)) + geom_bar(stat = "identity")

plot(cholera$poor.rate~cholera$death, pch=19, cex=3)
text(950, 0.3, label="North")
text(1200, 0.35, label="West")
text(1500 ,0.55, label = "Central")
text(3100, 0.55, label = "South")
text(7200, 0.85, label = "Kent")

plot(cholera$cholera_deaths~cholera$poor_rate)

ggplot(cholera, aes(x=cholera_drate, y=poor_rate, color=region)) + geom_point(size=5)
