library(stringr)
library(KoNLP)
library(wordcloud)

x <- c("교촌치킨", "BBQ")
str_match(x, "[A-Z]+")

y <- c("동해물과/N+과/J", "백두산/N+이/J")
str_match(y, "([가-힣]+)/N")

speech <- readLines("./data/speech.txt")
speech <- speech[nchar(speech)!=0]

ps <- SimplePos09(speech) %>% 
  unlist()

result <- str_match(ps, "([가-힣]+)/N")[,2]
result <- result[!is.na(result)] %>% 
  table()

#빈도수가 2개 이상인 단어 추출
result <- result[result>1]
wordcloud(names(result), result)


