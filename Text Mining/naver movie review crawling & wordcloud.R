if(!require(rvest)){install.packages("rvest")}
library(rvest)

### 데이터 크롤링

base_url <- "https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=157297&target=&page="

reviews <- c()
for(i in 1:1000){
  url <- paste(base_url, i, sep = "")
  html <- read_html(url, encoding = 'CP949')
  table <- html_nodes(html, '.list_netizen')
  text <- html_nodes(table, '.title')
  review <- html_text(text)
  reviews <- c(reviews, review)
}

#### 데이터 전처리
review_positive <- c()
review_negative <- c()

for(i in 1:length(reviews)){
  
  temp_1 <- gsub("\n", "", reviews[i])
  temp_2 <- gsub("\t", "", temp_1)
  temp_3 <- gsub("\"", "", temp_2)
  temp_4 <- gsub('[ㄱ-ㅎ]','',temp_3)
  temp_5 <- gsub('(ㅜ|ㅠ)+','',temp_4)
  temp_6 <- gsub("[[:punct:]]", "", temp_5)
  
  temp_7 <- strsplit(temp_6, "중")[[1]][2]
  
  if(substr(temp_7, 1,2) == "10"){
    review <- substr(temp_7, 3, nchar(temp_7)-2)
    review_positive <- c(review_positive, review)
  }else if(as.integer(substr(temp_7, 1,1)) >= 7){
    review <- substr(temp_7, 2, nchar(temp_7)-2)
    review_positive <- c(review_positive, review)
  }else{
    review <- substr(temp_7, 2, nchar(temp_7)-2)
    review_negative <- c(review_negative, review)
  }
}

review_positive <- review_positive[-which(nchar(review_positive) == 0)]
review_negative <- review_negative[-which(nchar(review_negative) == 0)]

review_positive <- review_positive[order(nchar(review_positive), decreasing = TRUE)]
review_negative <- review_negative[order(nchar(review_negative), decreasing = TRUE)]

movie_p <- review_positive[1:100]
movie_n <- review_negative[1:100]

write.table(movie_p, "movie_p.txt", row.names=FALSE, col.names=FALSE)
write.table(movie_n, "movie_n.txt", row.names=FALSE, col.names=FALSE)

if(!require(wordcloud2)){install.packages("wordcloud2")}
if(!require(rJava)){install.packages("rJava")}
if(!require(KoNLP)){install.packages("KoNLP")}
if(!require(dplyr)){install.packages("dplyr")}

library(wordcloud2)
library(rJava)
library(KoNLP)
library(dplyr)
useNIADic()

movie <- readLines("movie_p.txt")

word <- sapply(movie, extractNoun, USE.NAMES = FALSE) %>% 
  unlist() %>% 
  table()

word_df <- as.data.frame(word, stringAsFactors = FALSE)

word_df <- word_df %>% 
  filter(nchar(as.character(.)) >= 2) %>% 
  arrange(desc(Freq))

wordcloud(words = word_df$., 
          freq = word_df$Freq, 
          random.order = FALSE,
          min.freq = 3,
          rot.per = .1,
          colors = brewer.pal(6,"RdYlGn"))

word_df <- word_df[1:60,]
word_df <- word_df[-c(1,8,11,21,27,42,46,47,48,52,58,60),] #p
word_df <- word_df[-c(1,3,9,19,31,33,43,47,53,54,55,64,66,69,76),] #n
```