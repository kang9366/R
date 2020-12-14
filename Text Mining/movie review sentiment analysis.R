library(rvest)
library(magrittr)
library(plyr)
library(stringr)

eliminate <- function(data){
  data %>% 
    gsub("\r", "", .) %>% 
    gsub("\t", "", .) %>% 
    gsub("\n", "", .)
}
base_url <- "https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=66834&target=&page="

reviews <- c()
for(i in 1:100){
  url <- paste(base_url, i, sep = "")
  html <- read_html(url, encoding = 'CP949')
  table <- html_nodes(html, '.list_netizen')
  text <- html_nodes(table, '.title')
  review <- html_text(text) %>% 
    eliminate()
  for(j in 1:10){
    review[j] <- strsplit( review[j], "중")[[1]][2] %>% 
      gsub('\\d+', '', .) %>% 
      gsub('[[:punct:]]', '', .) %>% 
      gsub('[[:cntrl:]]', '', .)
  }
  reviews <- c(reviews, review)
}

reviews <- reviews[-which(nchar(reviews) == 0)]
reviews <- reviews[order(nchar(reviews), decreasing = TRUE)]
reviews <- reviews[1:20]

reviews %<>%
  gsub("신고", "", .)
positive <- readLines("./data/positive.txt", encoding="UTF-8")
positive <- positive[-1]
negative <- readLines("./data/negative.txt", encoding="UTF-8")
negative <- negative[-1]

sentimental <- function(sentences, positive, negative){
  scores = laply(sentences, function(sentence, positive, negative) {
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    pos.matches = match(words, positive)
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

result=sentimental(reviews, positive, negative)

result$color[result$score >=1] = "blue"
result$color[result$score ==0] = "green"
result$color[result$score < 0] = "red"

result$remark[result$score >=1] = "긍정"
result$remark[result$score ==0] = "중립"
result$remark[result$score < 0] = "부정"

sentiment_result = table(result$remark)
sentiment_result
barplot(sentiment_result, col = c("blue", "green", "red"))
