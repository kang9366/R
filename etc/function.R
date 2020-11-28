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
  a <- c()
  b <- c()
  for(i in 1:length(x)){
    a <- c(a, (x[i]-mean(x))^3)
    b <- c(b, (x[i]-mean(x))^2)
  }
  dat_ske <- sum(a)/(sum(b))^(3/2)
  
}

my.moment(c(1, 2, 3, 4))

