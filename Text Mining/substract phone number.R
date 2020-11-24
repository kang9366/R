library(readxl)
library(magrittr)

seoul <- read_excel("./data/seoul.xlsx")
phone <- seoul$`업소 전화번호`

#2-1
number_02 <- grep("^02", phone)

number <- strsplit(phone, "-")
number_null <- c()
number_etc <- c()
number_a <- c()

#2-2, 2-4
for(i in 1:length(number)){
  if(length(number[[i]])==3){
    if(number[[i]][1] != "02"){
      number_a %<>%  c(., i)
    }
  }else if(length(number[[i]])==2){
    number_etc %<>% c(., i)
  }else{
    number_null %<>%  c(., i)
  }
}

data_02 <- seoul[number_02,]
data_null <- seoul[number_null,]
data_etc <- seoul[number_etc,]
data_a <- seoul[number_a,]

View(data_02)
View(data_null)
View(data_etc)
View(data_a)

dat <- seoul
dat <- subset(dat, 분류코드=="001" | 분류코드=="002" |분류코드=="003")
info <- dat$업소정보
View(info)
info %<>% 
  gsub("\r", "", .) %>%
  gsub("\n", "", .) %>%
  gsub(" ", "", ., fixed=TRUE)

sum(is.na(info))
info %<>% na.omit(.)
View(info)
temp <- c()
for(i in 1:length(info)){
  if(info[i] == "." | info[i] == "null"){
    temp %<>% c(., i)
  }
}

info <- info[-temp]

substract <- function(x, y){
  for(i in 1:length(x)){
    y <- c(y, x[[i]][1])
  }
}
temp <- c()
substract(info, temp)


info %<>% strsplit(., "좌석수")
for(i in 1:length(info1)){
  info %<>%  c(info2, info1[[i]][1])
}

info3 <- info2 %>% 
  strsplit("휴무일")

info4 <- c()
for(i in 1:length(info3)){
  info4 <- c(info4, info3[[i]][1])
}

