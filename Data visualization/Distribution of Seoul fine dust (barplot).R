library(readxl)
library(dplyr)
library(RColorBrewer)

rm(list=ls())
src_dir <- c("/Users/kangseunggu/Desktop/R/Data2")
src_file <- list.files(src_dir)
jan_19 <- read_xlsx(paste(src_dir, "/", src_file[1], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
feb_19 <- read_xlsx(paste(src_dir, "/", src_file[2], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
mar_19 <- read_xlsx(paste(src_dir, "/", src_file[3], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
apr_19 <- read_xlsx(paste(src_dir, "/", src_file[4], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
may_19 <- read_xlsx(paste(src_dir, "/", src_file[5], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
jun_19 <- read_xlsx(paste(src_dir, "/", src_file[6], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
july_19 <- read_xlsx(paste(src_dir, "/", src_file[7], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
aug_19 <- read_xlsx(paste(src_dir, "/", src_file[8], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
sep_19 <- read_xlsx(paste(src_dir, "/", src_file[9], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
oct_19 <- read_xlsx(paste(src_dir, "/", src_file[10], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
nov_19 <- read_xlsx(paste(src_dir, "/", src_file[11], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
jan_20 <- read_xlsx(paste(src_dir, "/", src_file[12], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
feb_20 <- read_xlsx(paste(src_dir, "/", src_file[13], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
mar_20 <- read_xlsx(paste(src_dir, "/", src_file[14], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
apr_20 <- read_xlsx(paste(src_dir, "/", src_file[15], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
may_20 <- read_xlsx(paste(src_dir, "/", src_file[16], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
jun_20 <- read_xlsx(paste(src_dir, "/", src_file[17], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
july_20 <- read_xlsx(paste(src_dir, "/", src_file[18], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
aug_20 <- read_xlsx(paste(src_dir, "/", src_file[19], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
sep_20 <- read_xlsx(paste(src_dir, "/", src_file[20], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
oct_20 <- read_xlsx(paste(src_dir, "/", src_file[21], sep=""), col_names=T) %>% 
  t() %>% 
  data.frame()
nov_20 <- read_xlsx(paste(src_dir, "/", src_file[22], sep=""), col_types = "guess") %>% 
  t() %>% 
  data.frame()

str(nov_20)

data_19 <- data.frame("1월" = as.numeric(jan_19[2:length(jan_19), 1]),
                      "2월" = as.numeric(feb_19[2:length(feb_19), 1]),
                      "3월" = as.numeric(mar_19[2:length(mar_19), 1]),
                      "4월" = as.numeric(apr_19[2:length(apr_19), 1]),
                      "5월" = as.numeric(may_19[2:length(may_19), 1]),
                      "6월" = as.numeric(jun_19[2:length(jun_19), 1]),
                      "7월" = as.numeric(july_19[2:length(july_19), 1]),
                      "8월" = as.numeric(aug_19[2:length(aug_19), 1]),
                      "9월" = as.numeric(sep_19[2:length(sep_19), 1]),
                      "10월" = as.numeric(oct_19[2:length(oct_19), 1]),
                      "11월" = as.numeric(nov_19[2:length(feb_19), 1]))

mean_19 <- c(mean(data_19$X1월),
             mean(data_19$X2월),
             mean(data_19$X3월),
             mean(data_19$X4월),
             mean(data_19$X5월),
             mean(data_19$X6월),
             mean(data_19$X7월),
             mean(data_19$X8월),
             mean(data_19$X9월),
             mean(data_19$X10월),
             mean(data_19$X11월))


data_20 <- data.frame("1월" = as.numeric(jan_20[2:length(jan_20), 1]),
                      "2월" = as.numeric(feb_20[2:length(feb_20), 1]),
                      "3월" = as.numeric(mar_20[2:length(mar_20), 1]),
                      "4월" = as.numeric(apr_20[2:length(apr_20), 1]),
                      "5월" = as.numeric(may_20[2:length(may_20), 1]),
                      "6월" = as.numeric(jun_20[2:length(jun_20), 1]),
                      "7월" = as.numeric(july_20[2:length(july_20), 1]),
                      "8월" = as.numeric(aug_20[2:length(aug_20), 1]),
                      "9월" = as.numeric(sep_20[2:length(sep_20), 1]),
                      "10월" = as.numeric(oct_20[2:length(oct_20), 1]),
                      "11월" = as.numeric(nov_20[2:length(feb_20), 1]))

mean_20 <- c(mean(data_20$X1월),
             mean(data_20$X2월),
             mean(data_20$X3월),
             mean(data_20$X4월),
             mean(data_20$X5월),
             mean(data_20$X6월),
             mean(data_20$X7월),
             mean(data_20$X8월),
             mean(data_20$X9월),
             mean(data_20$X10월),
             mean(data_20$X11월))

par(mfrow=c(1,2))
barplot(mean_19, names=c("1월", "2월", "3월", "4월", "5월", "6월", "7월", "8월", "9월", "10월", "11월"), 
        main="2019년 서울시 미세먼지 월 별 평균", col=brewer.pal(11, "Set2"), ylim=c(0, 80))
barplot(mean_20, names=c("1월", "2월", "3월", "4월", "5월", "6월", "7월", "8월", "9월", "10월", "11월"), 
        main="2020년 서울시 미세먼지 월 별 평균", col=brewer.pal(11, "Set2"), ylim=c(0, 80))
