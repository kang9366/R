###########1
my.cor <- function(x, y){
  if(length(x)==length(y)){
    a <- c()
    b <- c()
    for(i in 1:length(x)){
      a <- c(a, x[i]-mean(x))
      b <- c(b, y[i]-mean(y))
    }
    
    d <- c()
    e <- c()
    for(i in 1:length(x)){
      d <- c(d, (x[i]-mean(x))^2)
      e <- c(e, (y[i]-mean(y))^2)
    }
    return(sum(a*b)/(sqrt(sum(d))*sqrt(sum(e))))
  }else{
    print("error")
  }
}

a <- c(2, 3, 4, 1, 4, 2)
b <- c(5, 2, 6, 7, 8, 1)
my.cor(a, b)
cor(a, b)

###########2
my_summary <- function(dist, n){
  if(dist == 1){
    data <- rnorm(n)
  }else if(dist==2){
    data <- rexp(n)
  }
  data_mean <- mean(data)
  data_sd <- sd(data)
  data_var <- var(data)
  data_range <- range(data)
  data_max <- max(data)
  data_min <- min(data)
  data_quantile <- quantile(data, c(0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9))
  cat("평균 : ", data_mean, '\n')
  cat("표준편차 : ", data_sd, '\n')
  cat("분산 : ", data_var, '\n')
  cat("범위 : ", data_range, '\n')
  cat("최대값 : ", data_max, '\n')
  cat("최소값 : ", data_min, '\n')
  cat("분위수 : ", data_quantile)
}
my_summary(2, 50)

############3
#####a
my.moment <- function(x){
  dat_sum <- sum(x)
  dat_mean <- mean(x)
  dat_var <- sum((x-mean(x))^2) / (length(x)-1)
  
  sum1<-sum((x-mean(x))^3)
  sum2<-sum((x-mean(x))^2)
  dat_skew<-(sqrt(length(x))*sum1)/(sum2^(3/2))
  
  sum3<-sum((x-mean(x))^4)
  sum4<-sum((x-mean(x))^2)
  dat_kurtosis<-((sum3/length(x))/((sum4/length(x))^2))-3
  
  cat("합계 : ", dat_sum, "\n")
  cat("평균 : ", dat_mean, "\n")
  cat("분산 : ", dat_var, "\n")
  cat("왜도 : ", dat_skew, "\n")
  cat("첨도 : ", dat_kurtosis)
}

#####b
my.moment.for <- function(x){
  dat_sum <- 0
  for(i in 1:length(x)){
    dat_sum <- dat_sum + x[i]
  }
  
  dat_mean <- dat_sum / length(x)
  
  temp_1 <- c()
  for(i in 1:length(x)){
    temp_1 <- c(temp_1, (x[i]-dat_mean)^2)
  }
  
  temp_2 <- 0
  for(i in 1:length(temp_1)){
    temp_2 <- temp_2 + temp_1[i]
  }
  dat_var <- temp_2 / (length(x)-1)
  
  temp_3 <- c()
  for(i in 1:length(x)){
    temp_3 <- c(temp_3, x[i]-dat_mean)
  }
  
  temp_4 <- 0
  for(i in 1:length(x)){
    temp_4 <- temp_4 + temp_3[i]^3
  }
  temp_5 <- 0
  for(i in 1:length(x)){
    temp_5 <- temp_5 + temp_3[i]^2
  }
  dat_skew <- (sqrt(length(x))*temp_4)/(temp_5^(3/2))
  
  temp_6 <- 0
  for(i in 1:length(x)){
    temp_6 <- temp_6 + temp_3[i]^4
  }
  temp_7 <- 0
  for(i in 1:length(x)){
    temp_7 <- temp_7 + temp_3[i]^2
  }
  dat_kurtosis<-((temp_6/length(x))/((temp_7/length(x))^2))-3
  
  cat("합계 : ", dat_sum, "\n")
  cat("평균 : ", dat_mean, "\n")
  cat("분산 : ", dat_var, "\n")
  cat("왜도 : ", dat_skew, "\n")
  cat("첨도 : ", dat_kurtosis)
}

data <- c(159, 280, 101, 121, 224, 222, 379, 179, 250, 170)
my.moment.for(data)
my.moment(data)
