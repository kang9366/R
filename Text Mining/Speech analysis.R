library(magrittr)
library(tm)
library(dplyr)
library(KoNLP)
library(stringr)
library(ggdendro)
library(dendextend)
library(factoextra)
library(cluster)

read <- function(file){
  noun <- c()
  data <- readLines(file)
  data <- data[nchar(data)!=0] %>% 
    paste(., collapse = "") %>% 
    gsub("[[:punct:]]", "",.) %>% 
    gsub("\n", "",.) %>% 
    SimplePos09()
  data <- str_match(data, "([가-힣]+)/N")[,2] %>% 
    na.omit() %>% 
    paste(., collapse = " ")
  return(data)
}

src_dir = "/Users/kangseunggu/Desktop/R/Text Mining/data"
setwd(src_dir)
src_file <- list.files(src_dir)

data <- c(read(src_file[1]), read(src_file[2]), read(src_file[3]), read(src_file[4]), read(src_file[5]))

tdm <- data %>% 
  VectorSource() %>% 
  Corpus() %>% 
  TermDocumentMatrix() %>% 
  as.matrix()

index <- c()
sum <- c()
for(i in 1:nrow(tdm)){
   if(nchar(dimnames(tdm)[[1]][i]) == 1){
    index <- c(index, i)
   }
  sum <- c(sum, sum(tdm[i,]))
}

tdm %<>%  cbind(., sum)
tdm <- tdm[-index,]

dimnames(tdm)[[2]] <- c(src_file[1], src_file[2], src_file[3], src_file[4], src_file[5], "합계")

tdm %<>%  as.data.frame(.) %>% 
  arrange(., desc(합계)) %>% 
  as.matrix()

View(tdm)

par(mfrow=c(1,1))

dist <- dist(t(tdm[,1:5]))
plot(clust, sub="", xlab="", ylab="", main = "대통령 연설문 덴드로그램")
rect.hclust(hclust(dist), k=4)

