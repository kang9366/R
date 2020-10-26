library(readxl)
dat <- read_xlsx("Seoul restaurant.xlsx")
View(dat)

address <- dat$`업소 주소`

#글자 수 계산
nchar(address[1])

#문자열 나누기
tmp <- "010-9804-1304"
tmp2 <- unlist(strsplit(tmp, "-"))

#문자열 합치기
paste(tmp2, collapse = "")

#구 정보 가져오기
temp <- strsplit(address[1], " ")
unlist(temp)[2]
gu <- c(1:length(address))
for(i in 1:length(address)){
  gu[i] <- unlist(strsplit(address[i], " "))[2]
}
gu <- as.factor(gu)
str(gu)
barplot(gu)
