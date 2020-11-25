library(readxl)
library(magrittr)

seoul <- read_excel("seoul.xlsx")
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
data_etc$`업소 전화번호` %<>% paste0("02-", .)

View(data_02)
View(data_null)
View(data_etc)
View(data_a)

dat <- seoul
dat <- subset(dat, 분류코드=="001" | 분류코드=="002" |분류코드=="003")
info <- dat$업소정보

info %<>% 
  gsub("\r", "", .) %>%
  gsub("\n", "", .) %>%
  gsub("\t", "", .) %>% 
  gsub(" ", "", ., fixed=TRUE)

sum(is.na(info))
info %<>% na.omit(.)

temp <- c()
for(i in 1:length(info)){
  if(info[i] == "." | info[i] == "null"){
    temp %<>% c(., i)
  }
}
info <- info[-temp]

substract <- function(x, y){
  y <- c()
  for(i in 1:length(x)){
    y %<>% c(., x[[i]][1])
  }
  return (y)
}
View(info)
info <- substract(strsplit(info ,"좌석수"), temp) 
info <- substract(strsplit(info, "휴무"), temp)
info <- substract(strsplit(info, "예약"), temp)
info <- substract(strsplit(info, "배달"), temp)
info %<>% gsub("영업시간:", "", .) 

temp <- c()
for(i in 1:length(info)){
  if(info[i] == ""){
    print(i)
    temp %<>% c(.,i)
  }
}

info <- info[-temp]