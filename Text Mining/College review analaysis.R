library(RSelenium)
library(magrittr)
library(dplyr)
library(stringr)
library(plyr)
library(wordcloud2)
library(KoNLP)
library(tm)
library(qgraph)

remDr <- remoteDriver(port = 4444, browserName="chrome")
remDr$open()

calculate <- function(num){
  cnt <- ((num-4)/4)
  cnt <- round(cnt)
  return(cnt)
}

base_url = "https://www.addcampus.com/search/campus?keyword="
college <- c("한림대학교", "강원대학교(춘천)")
review.cnt <- c()
for(i in 1:2){
  url <- paste0(base_url, college[i], "&category=")
  remDr$navigate(url)
  
  review.cnt <- c(review.cnt, remDr$findElements(using='class', value='searching-number') %>% 
    lapply(., function(x){x$getElementText()}) %>% 
    unlist() %>% 
    as.integer())
  
  btn.more <- remDr$findElement(using='xpath', 
                                value='//*[@id="container"]/div[2]/div/div/div/div/div[1]/div[2]/div[2]/button')
  
  for(j in 1:calculate(review.cnt[i])){
    btn.more$clickElement()
    Sys.sleep(0.5)
  }
  
  if(i==1){
    review_h <- remDr$findElements(using = 'class', value="review-list_box")%>% 
      lapply(., function(x){x$getElementText()}) %>% 
      unlist()
  }else{
    review_g <- remDr$findElements(using = 'class', value="review-list_box")%>% 
      lapply(., function(x){x$getElementText()}) %>% 
      unlist()
  }
}

#제목을 뺀 리뷰
temp_h <- c()
for(i in 1:length(review_h)){
  temp_h <- c(temp_h, strsplit(review_h, college[1])[[i]][2])
}

temp_g <- c()
for(i in 1:length(review_g)){
  temp_g <- c(temp_g, strsplit(review_g, college[2])[[i]][2])
}

#학과, 학번, 리뷰 분리
info_h <- c()
major_h <- c()
age_h <- c()
review_h <- c()
for(i in 1:length(temp_h)){
  tmp_h <- strsplit(temp_h[i], "\n장점\n")
  info_h <- c(info_h, tmp_h[[1]][1])
  major_h <- c(major_h, strsplit(info_h[i], " ")[[1]][2])
  age_h <- c(age_h, strsplit(info_h[i], " ")[[1]][3])
  review_h <- c(review_h, tmp_h[[1]][2])
}

info_g <- c()
major_g <- c()
age_g <- c()
review_g <- c()
for(i in 1:length(temp_g)){
  tmp_g <- strsplit(temp_g[i], "\n장점\n")
  info_g <- c(info_g, tmp_g[[1]][1])
  major_g <- c(major_g, strsplit(info_g[i], " ")[[1]][2])
  age_g <- c(age_g, strsplit(info_g[i], " ")[[1]][3])
  review_g <- c(review_g, tmp_g[[1]][2])
}


info.data.h <- data.frame(major=major_h,
                          age=age_h)

info.data.g <- data.frame(major=major_g,
                          age=age_g)

barplot(table(info.data.h$major), las=2, cex.names=0.7, col = "#eecccc")
barplot(table(info.data.h$age)[1:7], las=2, col = "#eecccc")

barplot(table(info.data.g$major), las=2, cex.names=0.5, col = "#e5ccee")
barplot(table(info.data.g$age), las=2, col = "#e5ccee")

#리뷰 전처리
review_h %<>% 
  strsplit(., "상세검색")

review_g %<>% 
  strsplit(., "상세검색")

hallym <- c()
for(i in 1:length(review_h)){
  hallym <- c(hallym, review_h[[i]][1])
}

gangwon <- c()
for(i in 1:length(review_g)){
  gangwon <- c(gangwon, review_g[[i]][1])
}

processing <- function(data){
  data %<>%
    gsub('\n단점', '', .) %>% 
    gsub('\n', '', .) %>% 
    gsub('[[:punct:]]','', .) %>% 
    gsub('[ㄱ-ㅎ]', '', .) %>% 
    gsub('(ㅜ|ㅠ)+', '', .) %>% 
    gsub('[0-9]', '', .)
}

hallym %<>% processing(.)
gangwon %<>% processing(.)

#감성 분석
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

result_h <- sentimental(hallym, positive, negative)
result_h$remark[result_h$score >=1] = "긍정"
result_h$remark[result_h$score ==0] = "중립"
result_h$remark[result_h$score < 0] = "부정"
table(result_h$remark)

result_g <- sentimental(gangwon, positive, negative)
result_g$remark[result_g$score >=1] = "긍정"
result_g$remark[result_g$score ==0] = "중립"
result_g$remark[result_g$score < 0] = "부정"
table(result_g$remark)

par(mfrow=c(1, 2))
pie(table(result_h$remark))
pie(table(result_g$remark))

#Term Document Matrix
noun_h <- c()
for (i in 1:length(hallym)) {
  temp <- sapply(hallym[i], extractNoun, USE.NAMES = F) %>% 
    as.vector()
  noun_h <-c(noun_h, temp)
}

noun_h <- sapply(hallym, extractNoun, USE.NAMES = F) %>% 
  unlist()
noun_h <- nn %>%
  unlist()

noun_g <- sapply(gangwon, extractNoun, USE.NAMES = F) %>% 
  unlist()

noun <- list(noun_h, noun_g)
tdm <- noun %>% 
  VectorSource() %>% 
  Corpus() %>% 
  TermDocumentMatrix() %>% 
  as.matrix()

sum <- c()
for(i in 1:nrow(tdm)){
  sum <- c(sum, sum(tdm[i,]))
}

tdm %<>%  cbind(., sum)

dimnames(tdm)[[2]] <- c(college[1], college[2], "합계")

tdm %<>%  as.data.frame(.)
tdm <- tdm[order(tdm$합계, decreasing = TRUE),] %>% 
  head(100)
  

row.names(tdm) %<>% 
  gsub('[[:punct:]]','', .)

index <- c()
for(i in 1:nrow(tdm)){
  if(nchar(row.names(tdm)[i])==1){
    index <- c(index, i)
  }
}

tdm <- tdm[-index,] %>% 
  as.matrix()

word.count <- rowSums(tdm)
word.order <- order(word.count, decreasing = TRUE)
freq.word <- tdm[word.order[1:30],]
co.matrix <- freq.word %*% t(freq.word)

qgraph(co.matrix, labels=rownames(co.matrix),
       diag=FALSE, layout='spring', threshold=4,
       vsize=log(diag(co.matrix)))

#워드클라우드
noun_h %<>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(., desc(Freq))
names(noun_h)[1] <- "word"
noun_h$word %<>% as.character()
noun_h <- noun_g[(noun_h$Freq)>=8&nchar(noun_h$word)>1,]

noun_h %<>% 
  na.omit()
noun_g %<>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(., desc(Freq))
names(noun_g)[1] <- "word"

noun_g$word %<>% as.character()
noun_g <- noun_g[(noun_g$Freq)>=8&nchar(noun_g$word)>1,]
noun_g <- noun_g[-c(3,23,26,32),]

wordcloud2(noun_h, fontFamily = "NanumGothic", color='random-light')
wordcloud2(noun_g, fontFamily = "NanumGothic", color='random-light')

save.image("project.RData")
