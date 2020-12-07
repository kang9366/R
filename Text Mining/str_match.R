library(stringr)

x <- c("교촌치킨", "BBQ")
str_match(x, "[A-Z]+")

y <- c("동해물과/N+과/J", "백두산/N+이/J")
str_match(y, "([가-힣]+)/N")
