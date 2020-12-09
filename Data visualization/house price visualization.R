library(magrittr)
library(dplyr)
options("scipen" = 100)

data <- read.csv("./data/report2.csv") %>% 
  arrange(date)

for(i in 2:ncol(data)){
  data[,i] %<>%
    gsub("[[:punct:]]", "", .) %>% 
    as.numeric()
}

par(mfrow=c(1,2))
barplot(data$sj3.sold, names=data$date, ylim=c(0,50000), main="sj3 sold", col = "red")
barplot(data$sj3.lease, names=data$date, ylim=c(0,50000), main="sj3 lease", col = "blue")

par(mfrow=c(1,2))
barplot(data$sj4.sold, names=data$date, ylim=c(0,40000), main="sj4 sold", col = "red")
barplot(data$sj4.lease, names=data$date, ylim=c(0,40000), main="sj4 lease", col = "blue")

par(mfrow=c(1,2))
barplot(data$sj5.sold, names=data$date, ylim=c(0,60000), main="sj5 sold", col = "red")
barplot(data$sj5.lease, names=data$date, ylim=c(0,60000), main="sj5 lease", col = "blue")

par(mfrow=c(1,2))
barplot(data$sj7.sold, names=data$date, ylim=c(0,60000), main="sj7 sold", col = "red")
barplot(data$sj7.lease, names=data$date, ylim=c(0,60000), main="sj7 lease", col = "blue")

par(mfrow=c(1,2))
barplot(data$sj10.sold, names=data$date, ylim=c(0,50000), main="sj10 sold", col = "red")
barplot(data$sj10.lease, names=data$date, ylim=c(0,50000), main="sj10 lease", col = "blue")

par(mfrow=c(1,2))
barplot(data$dg.sold, names=data$date, ylim=c(0,60000), main="dg sold", col = "red")
barplot(data$dg.lease, names=data$date, ylim=c(0,60000), main="dg lease", col = "blue")

par(mfrow=c(1,2))
barplot(data$cs1.sold, names=data$date, ylim=c(0,50000), main="cs1 sold", col = "red")
barplot(data$cs1.lease, names=data$date, ylim=c(0,50000), main="cs2 lease", col = "blue")

par(mfrow=c(1,2))
barplot(data$cs2.sold, names=data$date, ylim=c(0,50000), main="cs2 sold", col = "red")
barplot(data$cs2.lease, names=data$date, ylim=c(0,50000), main="cs2 lease", col = "blue")

par(mfrow=c(1,2))
barplot(data$cbi.sold, names=data$date, ylim=c(0,100000), main="cbi sold", col = "red")
barplot(data$cbi.lease, names=data$date, ylim=c(0,100000), main="cbi lease", col = "blue")

par(mfrow=c(1,2))
barplot(data$dc.sold, names=data$date, ylim=c(0,80000), main="dc sold", col = "red")
barplot(data$dc.lease, names=data$date, ylim=c(0,80000), main="dc lease", col = "blue")