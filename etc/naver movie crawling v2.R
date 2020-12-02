library(rvest)
library(progress)

base_url <- "https://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=pnt&date=20201124"

eliminate <- function(data){
  data %>% 
    gsub("\r", "", .) %>% 
    gsub("\t", "", .) %>% 
    gsub("\n", "", .)
}

pb <- progress_bar$new(total = 40)
title <- c()
point <- c()
code <- c()
for(i in 1:40){
  url <- paste(base_url, "&page=", i, sep = "")
  html <- read_html(url, encoding = 'CP949')
  table <- html_nodes(html, '.list_ranking')
  title_html <- html_nodes(table, '.tit5 a')
  
  title_text <- html_text(title_html) %>% 
    eliminate()
  title <- c(title, title_text)
  
  point_text <- html_nodes(table, ".point") %>% 
    html_text()
  point <- c(point, point_text)
  
  for(i in 1:50){
    link <- html_attrs(title_html)[[i]][1]
    code_text <- strsplit(link, "=")[[1]][2]
    code <- c(code, code_text)
  }
  pb$tick()                                                                     
  Sys.sleep(0.01)
}

base_url <- "https://movie.naver.com/movie/bi/mi/basic.nhn?code="

genre <- c()
country <- c()
runtime <- c()
release <- c()
director <- c()
actor	<- c()
view_class <- c()
audience_age_10 <- c()
audience_age_20 <- c()
audience_age_30 <- c()
audience_age_40	<- c()
audience_age_50 <- c()
netizen_score <- c()
netizen_count <- c()
ntz_male <- c()
ntz_female <- c()
ntz_10 <- c()
ntz_20 <- c()
ntz_30 <- c()
ntz_40 <- c()
ntz_50 <- c()
audience_score <- c()
audience_count <- c()
audience_male <- c()
audience_female <- c()
audience_10 <- c()
audience_20 <- c()
audience_30 <- c()
audience_40 <- c()
audience_50 <- c()

pb <- progress_bar$new(total = 1000)
for(i in 1:1000){
  url <- paste0(base_url, code[i])
  html <- read_html(url, encoding = 'eu-kr')

  info <- html_nodes(html, ".info_spec span") %>% 
    html_text() %>% 
    eliminate()
  genre <- c(genre, info[1])
  country <- c(country, info[2])
  runtime <- c(runtime, info[3])
  release <- c(release, info[4])
  view_class <- c(view_class, info[5])
  
  director_data <- html_node(html, xpath='//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[2]/p') %>% 
    html_text()
  director <- c(director, director_data)
  
  actor_data <- html_node(html, xpath='//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[3]/p') %>% 
    html_text()
  actor <- c(actor, actor_data)
  
  percent <- html_nodes(html, ".graph_percent") %>% 
    html_text()
  audience_age_10 <- c(audience_age_10, percent[1])
  audience_age_20 <- c(audience_age_20, percent[2])
  audience_age_30 <- c(audience_age_30, percent[3])
  audience_age_40 <- c(audience_age_40, percent[4])
  audience_age_50 <- c(audience_age_50, percent[5])
  
  url_point <- gsub("basic", "point", url)
  html_point <- read_html(url_point, encoding="eu-kr")
  
  score_ntz_1 <- html_nodes(html_point, xpath='//*[@id="graph_area"]/div[1]/div[1]/div[2]') %>% 
    html_text() %>% 
    eliminate() %>% 
    strsplit(., "참여")
  
  if(length(score_ntz_1)==0){
    netizen_score <- c(netizen_score, NA)
    netizen_count <- c(netizen_count, NA)
  }else{
    netizen_score <- c(netizen_score, gsub("\\s", "", score_ntz_1[[1]][1]))
    netizen_count <- c(netizen_count, gsub("명", "", score_ntz_1[[1]][2]))
  }
  
  score_ntz_2 <- html_nodes(html_point, xpath='//*[@id="netizen_point_graph"]/div/div[1]') %>% 
    html_nodes(., ".graph_point") %>% 
    html_text()
  ntz_male <- c(ntz_male, score_ntz_2[1])
  ntz_female <- c(ntz_female, score_ntz_2[2])
  ntz_10 <- c(ntz_10, score_ntz_2[3])
  ntz_20 <- c(ntz_20, score_ntz_2[4])
  ntz_30 <- c(ntz_30, score_ntz_2[5])
  ntz_40 <- c(ntz_40, score_ntz_2[6])
  ntz_50 <- c(ntz_50, score_ntz_2[7])
  
  score_aud_1 <- html_nodes(html_point, xpath='//*[@id="actual_point_tab_inner"]') %>% 
    html_text() %>% 
    eliminate() %>% 
    strsplit(., "참여")
  if(length(score_aud_1)==0){
    audience_score <- c(audience_score, NA)
    audience_count <- c(audience_count, NA)
  }else{
    audience_score <- c(audience_score, score_aud_1[[1]][1])
    audience_count <- c(audience_count, score_aud_1[[1]][2])
  }

  score_aud_2 <- html_nodes(html_point, xpath='//*[@id="actual_point_graph"]') %>% 
    html_nodes(., ".graph_point") %>% 
    html_text()
  audience_male <- c(audience_male, score_aud_2[1])
  audience_female <- c(audience_female, score_aud_2[2])
  audience_10 <- c(audience_10, score_aud_2[3])
  audience_20 <- c(audience_20, score_aud_2[4])
  audience_30 <- c(audience_30, score_aud_2[5])
  audience_40 <- c(audience_40, score_aud_2[6])
  audience_50 <- c(audience_50, score_aud_2[7])
  pb$tick()                                                                     
  Sys.sleep(0.01)
}

data_1 <- data.frame(title=title,
                   code=code,
                   genre=genre,
                   country=country,
                   runtime=runtime,
                   release=release,
                   director=director,
                   actor=actor,
                   view_class=view_class,
                   audience_age_10=audience_age_10,
                   audience_age_20=audience_age_20,
                   audience_age_30=audience_age_30,
                   audience_age_40=audience_age_40,
                   audience_age_50=audience_age_50,
                   netizen_score=netizen_score,
                   netizen_count=netizen_count,
                   ntz_male=ntz_male,
                   ntz_female=ntz_female,
                   ntz_10=ntz_10,
                   ntz_20=ntz_20,
                   ntz_30=ntz_30,
                   ntz_40=ntz_40,
                   ntz_50=ntz_50,
                   audience_score=audience_score,
                   audience_count=audience_count,
                   audience_male=audience_male,
                   audience_female=audience_female,
                   audience_10=audience_10,
                   audience_20=audience_20,
                   audience_30=audience_30,
                   audience_40=audience_40,
                   audience_50=audience_50)


genre <- c()
country <- c()
runtime <- c()
release <- c()
director <- c()
actor	<- c()
view_class <- c()
audience_age_10 <- c()
audience_age_20 <- c()
audience_age_30 <- c()
audience_age_40	<- c()
audience_age_50 <- c()
netizen_score <- c()
netizen_count <- c()
ntz_male <- c()
ntz_female <- c()
ntz_10 <- c()
ntz_20 <- c()
ntz_30 <- c()
ntz_40 <- c()
ntz_50 <- c()
audience_score <- c()
audience_count <- c()
audience_male <- c()
audience_female <- c()
audience_10 <- c()
audience_20 <- c()
audience_30 <- c()
audience_40 <- c()
audience_50 <- c()
for(i in 1001:2000){
  url <- paste0(base_url, code[i])
  html <- read_html(url, encoding = 'eu-kr')
  
  info <- html_nodes(html, ".info_spec span") %>% 
    html_text() %>% 
    eliminate()
  genre <- c(genre, info[1])
  country <- c(country, info[2])
  runtime <- c(runtime, info[3])
  release <- c(release, info[4])
  view_class <- c(view_class, info[5])
  
  director_data <- html_node(html, xpath='//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[2]/p') %>% 
    html_text()
  director <- c(director, director_data)
  
  actor_data <- html_node(html, xpath='//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[3]/p') %>% 
    html_text() %>% 
    eliminate()
  actor <- c(actor, actor_data)
  
  percent <- html_nodes(html, ".graph_percent") %>% 
    html_text()
  audience_age_10 <- c(audience_age_10, percent[1])
  audience_age_20 <- c(audience_age_20, percent[2])
  audience_age_30 <- c(audience_age_30, percent[3])
  audience_age_40 <- c(audience_age_40, percent[4])
  audience_age_50 <- c(audience_age_50, percent[5])
  
  url_point <- gsub("basic", "point", url)
  html_point <- read_html(url_point, encoding="eu-kr")
  
  score_ntz_1 <- html_nodes(html_point, xpath='//*[@id="graph_area"]/div[1]/div[1]/div[2]') %>% 
    html_text() %>% 
    eliminate() %>% 
    strsplit(., "참여")
  
  if(length(score_ntz_1)==0){
    netizen_score <- c(netizen_score, NA)
    netizen_count <- c(netizen_count, NA)
  }else{
    netizen_score <- c(netizen_score, gsub("\\s", "", score_ntz_1[[1]][1]))
    netizen_count <- c(netizen_count, gsub("명", "", score_ntz_1[[1]][2]))
  }
  
  score_ntz_2 <- html_nodes(html_point, xpath='//*[@id="netizen_point_graph"]/div/div[1]') %>% 
    html_nodes(., ".graph_point") %>% 
    html_text()
  ntz_male <- c(ntz_male, score_ntz_2[1])
  ntz_female <- c(ntz_female, score_ntz_2[2])
  ntz_10 <- c(ntz_10, score_ntz_2[3])
  ntz_20 <- c(ntz_20, score_ntz_2[4])
  ntz_30 <- c(ntz_30, score_ntz_2[5])
  ntz_40 <- c(ntz_40, score_ntz_2[6])
  ntz_50 <- c(ntz_50, score_ntz_2[7])
  
  score_aud_1 <- html_nodes(html_point, xpath='//*[@id="actual_point_tab_inner"]') %>% 
    html_text() %>% 
    eliminate() %>% 
    strsplit(., "참여")
  if(length(score_aud_1)==0){
    audience_score <- c(audience_score, NA)
    audience_count <- c(audience_count, NA)
  }else{
    audience_score <- c(audience_score, score_aud_1[[1]][1])
    audience_count <- c(audience_count, score_aud_1[[1]][2])
  }
  
  score_aud_2 <- html_nodes(html_point, xpath='//*[@id="actual_point_graph"]') %>% 
    html_nodes(., ".graph_point") %>% 
    html_text()
  audience_male <- c(audience_male, score_aud_2[1])
  audience_female <- c(audience_female, score_aud_2[2])
  audience_10 <- c(audience_10, score_aud_2[3])
  audience_20 <- c(audience_20, score_aud_2[4])
  audience_30 <- c(audience_30, score_aud_2[5])
  audience_40 <- c(audience_40, score_aud_2[6])
  audience_50 <- c(audience_50, score_aud_2[7])
  pb$tick()                                                                     
  Sys.sleep(0.01)
}
data_2 <- data.frame(title=title,
                   code=code,
                   genre=genre,
                   country=country,
                   runtime=runtime,
                   release=release,
                   director=director,
                   actor=actor,
                   view_class=view_class,
                   audience_age_10=audience_age_10,
                   audience_age_20=audience_age_20,
                   audience_age_30=audience_age_30,
                   audience_age_40=audience_age_40,
                   audience_age_50=audience_age_50,
                   netizen_score=netizen_score,
                   netizen_count=netizen_count,
                   ntz_male=ntz_male,
                   ntz_female=ntz_female,
                   ntz_10=ntz_10,
                   ntz_20=ntz_20,
                   ntz_30=ntz_30,
                   ntz_40=ntz_40,
                   ntz_50=ntz_50,
                   audience_score=audience_score,
                   audience_count=audience_count,
                   audience_male=audience_male,
                   audience_female=audience_female,
                   audience_10=audience_10,
                   audience_20=audience_20,
                   audience_30=audience_30,
                   audience_40=audience_40,
                   audience_50=audience_50)

data <- rbind(data_1, data_2)
write.csv(data, "./movie.csv", fileEncoding = "CP949")
