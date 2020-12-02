library(RSelenium)
library(magrittr)
library(progress)

pb <- progress_bar$new(total=55)

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome")
remDr$open()
base_url = "https://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=pnt&date=20201124"

title <- c()
code <- c()
male <- c()
female <- c()

for(i in 1:40){
  page <- i
  url <- paste0(base_url, "&page=", i)
  remDr$navigate(url)
  titles <- remDr$findElements(using="class",value="tit5") %>% 
    lapply(., function(x){x$getElementText()}) %>% 
    unlist()
  title <- c(titles, title)
  for(i in 2:55){
    if(i < 12){
      movie <- remDr$findElement(using = "xpath", value=paste0('//*[@id="old_content"]/table/tbody/tr[', i, ']/td[2]/div/a'))
      movie$clickElement()
      code_url <- movie$getCurrentUrl()
      code_data <- strsplit(code_url[[1]][1], "=")[[1]][2]
      code <- c(code, code_data)
      remDr$navigate(code_url)
      score <- remDr$findElements(using='tag name',value='tspan') %>% 
        lapply(., function(x){x$getElementText()}) %>% 
        unlist()
      if(length(score)==0){
        male <- c(male, NA)
        female <- c(female, NA)
      }else{
        male <- c(male, score[1])
        female <- c(female, score[2])
      }
      remDr$navigate(url)
    }else if(i > 12 & i < 23){
      movie <- remDr$findElement(using = "xpath", value=paste0('//*[@id="old_content"]/table/tbody/tr[', i, ']/td[2]/div/a'))
      movie$clickElement()
      code_url <- movie$getCurrentUrl()
      code_data <- strsplit(code_url[[1]][1], "=")[[1]][2]
      code <- c(code, code_data)
      remDr$navigate(code_url)
      score <- remDr$findElements(using='tag name',value='tspan') %>% 
        lapply(., function(x){x$getElementText()}) %>% 
        unlist()
      if(length(score)==0){
        male <- c(male, NA)
        female <- c(female, NA)
      }else{
        male <- c(male, score[1])
        female <- c(female, score[2])
      }
      remDr$navigate(url)
    }else if(i > 23 & i < 34){
      movie <- remDr$findElement(using = "xpath", value=paste0('//*[@id="old_content"]/table/tbody/tr[', i, ']/td[2]/div/a'))
      movie$clickElement()
      code_url <- movie$getCurrentUrl()
      code_data <- strsplit(code_url[[1]][1], "=")[[1]][2]
      code <- c(code, code_data)
      remDr$navigate(code_url)
      score <- remDr$findElements(using='tag name',value='tspan') %>% 
        lapply(., function(x){x$getElementText()}) %>% 
        unlist()
      if(length(score)==0){
        male <- c(male, NA)
        female <- c(female, NA)
      }else{
        male <- c(male, score[1])
        female <- c(female, score[2])
      }
      remDr$navigate(url)
    }else if(i > 34 & i < 45){
      movie <- remDr$findElement(using = "xpath", value=paste0('//*[@id="old_content"]/table/tbody/tr[', i, ']/td[2]/div/a'))
      movie$clickElement()
      code_url <- movie$getCurrentUrl()
      code_data <- strsplit(code_url[[1]][1], "=")[[1]][2]
      code <- c(code, code_data)
      remDr$navigate(code_url)
      score <- remDr$findElements(using='tag name',value='tspan') %>% 
        lapply(., function(x){x$getElementText()}) %>% 
        unlist()
      if(length(score)==0){
        male <- c(male, NA)
        female <- c(female, NA)
      }else{
        male <- c(male, score[1])
        female <- c(female, score[2])
      }
      remDr$navigate(url)
    }else if(i > 45 & i <= 55){
      movie <- remDr$findElement(using = "xpath", value=paste0('//*[@id="old_content"]/table/tbody/tr[', i, ']/td[2]/div/a'))
      movie$clickElement()
      code_url <- movie$getCurrentUrl()
      code_data <- strsplit(code_url[[1]][1], "=")[[1]][2]
      code <- c(code, code_data)
      remDr$navigate(code_url)
      score <- remDr$findElements(using='tag name',value='tspan') %>% 
        lapply(., function(x){x$getElementText()}) %>% 
        unlist()
      if(length(score)==0){
        male <- c(male, NA)
        female <- c(female, NA)
      }else{
        male <- c(male, score[1])
        female <- c(female, score[2])
      }
      remDr$navigate(url)
    }else{
      NULL
    }
  }
  print(paste0("complete ", page, " page of 40 pages"))
}

data <- data.frame(title = title,
                   code = code,
                   audience_male = male,
                   audience_female = female)

write.csv(data, "movie.csv")