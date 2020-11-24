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
table(gu)
barplot(gu)

# appply
square <- function(x){
  x^2
}
sapply(c(2,3,4,5), square)

chicken <- readLines("chicken.txt", encoding = "UTF-8")
View(chicken)
for(i in 1:length(chicken)){
  print(strsplit(chicken[i], ":")[1])
}

f1 <- function(x){
  unlist(strsplit(x, ": "))[1]
}
f2 <- function(x){
  unlist(strsplit(x, ": "))[2]
}

brand <- sapply(chicken, f1)
n <- sapply(chicken, f2)

n <- as.numeric(n)
df <- data.frame(brand, n)
install.packages("wordcloud")

country <- dget("country.txt")

grep("Korea", country, value=T)
grep("ee", country, value=T)
grep("'", country, value=T)
grep("Rep", country, value=T)
grep(" ", country, value=T)
country[nchar(country) == 7]
grep("^Co", country, value=T)
grep("land$", country, value=T)

strings <- c("abcd", "cdab", "cabd", "c abd", "^ab")
grep("ab", strings, value=T)

#ab로 시작하는 벡터 찾기
grep("^ab", strings, value=T) 

#ab로 끝나는 벡터찾기
grep("ab$", strings, value=T) 
grep("ab[c-e]", strings, value=T)
grep("ab[^c]", strings, value=T)

#^ 찾기 (\\을 붙이면 됨)
grep("\\^ab", strings, value=T) 
grep("^ab", strings, value=T, fixed = T)

# abc, abd찾기
grep("abc|abd", strings, value=T) 

# ab뒤에 공백이나 문자가 1개 이상
grep("ab.", strings, value=T) 

# ab뒤에 공백이나 문자가 0개 이상
grep("ab*", strings, value=T) 

id <- "kang9366@naver.com"
name <- strsplit("@", id)
sub("p", "*", "apple")
gsub("p", "*", "apple")

