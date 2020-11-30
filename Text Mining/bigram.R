library(dplyr)
library(janeaustenr)
library(tidyr)
library(tidytext)
library(ggraph)
library(tidygraph)

book <- austen_books()
table(book$book)
bigrams <- unnest_ngrams(book, bigram, text, n=2)

count(bigrams, bigram, sort = TRUE)

#bigram을 각 단어로 분리
bs <- separate(bigrams, bigram, c("word1", "word2"), sep = " ")

#stop word에 해당되는 것이 있는 경우 제외
bf <- filter(bs, !word1 %in% stop_words & !word2 %in% stop_words$word)

#street 찾기
count(filter(bf, word2 == "street"), book, word1, sort=TRUE)

bc <- count(bf, word1, word2, sort = TRUE)

bg <- as_tbl_graph(filter(bc, n > 50))

ggraph(bg, layout = "fr") +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label=name), vjust=1)
  
