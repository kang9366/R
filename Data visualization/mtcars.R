library(ggplot2)

mtcars <- read.csv("./data/mtcars.csv")

g1 <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
g2 <- ggplot(mtcars, aes(x=wt, y=mpg, shape=as.factor(cyl))) + geom_point(size=3)
g3 <- ggplot(mtcars, aes(x=wt, y=mpg, shape=as.factor(cyl), color=as.factor(cyl))) + geom_point(size=3)

g4 <- g3 + 
  geom_smooth(method = lm) +
  theme_gray()
g5 <- g3 + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

g6 <- g5 + labs(title="실린더 수 별 mpg와 wt의 관계")
