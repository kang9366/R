if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(scatterplot3d)){install.packages("scatterplot3d")}
if(!require(magrittr)){install.packages("magrittr")}

library(magrittr)
library(ggplot2)
library(dplyr)
library(scatterplot3d)
library(RColorBrewer)

diamonds <- read.csv("diamonds.csv")
names(diamonds)[3] <- "colour"

diamonds$cut %<>%  as.factor(.)
diamonds$clarity %<>% as.factor(.)
diamonds$colour %<>% as.factor(.)

col_cut <- c("red", "skyblue", "black", "green", "yellow")
col_col <- c()
col_cty <- c()

col_cut <- col_cut[as.numeric(diamonds$cut)]
col_col <- col_col[as.numeric(diamonds$colour)]
col_cty <- col_cty[as.numeric(diamonds$clarity)]

scatterplot3d(diamonds[,c(1,2,5)], color = col_cut, pch=19, angle=140, 
              xlab="무게", ylab="컷팅 품질", zlab="가격",
              y.ticklabs=
              main="커팅 품질에 따른 다이아몬드의 무게와 가격의 관계")

scatterplot3d(diamonds[,c(1,3,5)], angle=140, pch=19, highlight.3d=TRUE, col.grid="lightblue",
              xlab="무게", ylab="색상 품질", zlab="가격",
              main="색상 품질에 따른 다이아몬드의 무게와 가격의 관계")

p <- scatterplot3d(diamonds[,c(1,4,5)], angle=140, pch=19, col.axis="blue",
              #axis(3, at= 1:length(levels(diamonds$clarity)),lab=c(levels(diamonds$clarity))),
              xlab="무게", ylab="순수도", zlab="가격",
              main="순수함에 따른 다이아몬드의 무게와 가격의 관계")
text(p$xyz.convert((ma.x[lab],
                    rep(min(y),length(lab)),
                    rep(min(z) - 1,length(lab))),
                   labels=c(levels(diamonds$clarity))


summary(lm(carat~price+cut, diamonds))


z <- seq(-10,10,.01)
x <- as.POSIXct("2013-01-10", tz = "EST")+sin(z)*10000
y <- sin(z)
p3 <- scatterplot3d(x,y,z, x.ticklabs = NA, highlight.3d=TRUE, col.axis="blue",
                    col.grid="lightblue",pch=20)
lab <- seq(1,length(x),800)
ma.x <- matrix(x,nrow=length(x),ncol=1)
text(p3$xyz.convert(ma.x[lab],
                    rep(min(y),length(lab)),
                    rep(min(z) - 1,length(lab))),
     labels=paste(x[lab], "EST", sep=" "), cex=.8)
