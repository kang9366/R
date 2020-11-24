library(readxl)
library(magrittr)

seoul <- read_excel("seoul.xlsx")
phone <- seoul$`업소 전화번호`

#2-1
number_02 <- grep("^02", phone)

number <- strsplit(phone, "-")

number_null <- c()
number_etc <- c()
number_ <- c()

#2-2, 2-4
for(i in 1:length(number)){
  if(length(number[[i]])==3){
    if(number[[i]][1] != "02"){
      number_a <- c(number_a, i)
    }
  }else if(length(number[[i]])==2){
    number_etc <- c(number_etc, i)
  }else{
    number_null <- c(number_null, i)
  }
}

data_02 <- seoul[number_02,]
data_null <- seoul[number_null,]
data_etc <- seoul[number_etc,]
data_a <- seoul[number_a,]

data_etc$`업소 전화번호` %<>% paste0("02-", .)

View(data_02)
View(data_null)
View(data_etc)
View(data_a)

data$'업소 전화번호' <- paste0("02-", data$'업소 전화번호')

dat <- seoul
dat <- subset(dat, 분류코드=="001" | 분류코드=="002" |분류코드=="003")
info <- dat$업소정보
View(info)

info1 <-
  info %>% 
  gsub("\r", "", .) %>%
  gsub("\n", "", .) %>%
  gsub(" ", "", ., fixed=TRUE) %>% 
  strsplit(., "좌석수") %>% 
  strsplit(., "휴무일") %>% 
  strsplit(., "예약") %>% 
  strsplit(., "배달")

View(info1)

info2 <- c()
for(i in 1:length(info1)){
  info2 <- c(info2, info1[[i]][1])
}

info3 <- info2 %>% 
  strsplit("휴무일")

info4 <- c()
for(i in 1:length(info3)){
  info4 <- c(info4, info3[[i]][1])
}

