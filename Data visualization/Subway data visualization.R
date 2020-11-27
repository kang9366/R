library(dplyr)

subway1 <- read.csv("./data/subway.csv", header=T, encoding="UTF-8", fileEncoding="CP949" )
subway2 <- read.csv("./data/subway_latlong.csv", header=T, encoding="UTF-8", fileEncoding="CP949")
names(subway2)[1] <- names(subway1[1])

subway.tot <- merge(subway1, subway2, by="station")

