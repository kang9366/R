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
View(diamonds)
str(diamonds)
names(diamonds)[3] <- "colour"

diamonds$cut %<>%  factor(., levels=c("Fair", "Good", "Very Good", "Premium", "Ideal"), order=T)
diamonds$clarity %<>% factor(., levels=c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"), order=T)
diamonds$colour %<>% as.factor(.)

plot(diamonds$carat~diamonds$price, pch=19,
     main="다이아몬드 가격과 무게의 관계", xlab="가격", ylab="무게")

abline(lm(diamonds$carat~diamonds$price), col="red", lwd=2)

cor(diamonds$price, diamonds$carat)
text(5000, 3, "cor = 0.9282161")

col_cut <- c("red", "skyblue", "black", "green", "yellow")

col_col <- c("red", "skyblue", "black", "green", "yellow", "purple", "pink")

col_cty <- c("red", "skyblue", "black", "green", "yellow", "purple", "pink", "gray")
col_cty <- col_cty[as.numeric(diamonds$clarity)]

col_col <- col_col[as.numeric(diamonds$colour)]
col_cut <- col_cut[as.numeric(diamonds$cut)]

scatterplot3d(diamonds[,c(1,2,5)], angle=140, pch=19, color=col_cut,  
              xlab="무게", ylab="컷팅 품질", zlab="가격",
              main="커팅 품질에 따른 다이아몬드의 무게와 가격의 관계")

scatterplot3d(diamonds[,c(1,3,5)], angle=140, pch=19, color=col_col,
              xlab="무게", ylab="색상 품질", zlab="가격",
              main="색상 품질에 따른 다이아몬드의 무게와 가격의 관계")

scatterplot3d(diamonds[,c(1,4,5)], angle=140, pch=19, color=col_cty,
              xlab="무게", ylab="순수도", zlab="가격",
              main="순수함에 따른 다이아몬드의 무게와 가격의 관계")

ggplot(diamonds, aes(cut, price)) + 
  geom_jitter(aes(color=cut))+
  ggtitle("커팅 품질에 따른 다이아몬드의 무게와 가격의 관계") + 
  xlab("커팅품질") + ylab("가격") + 
  theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"),
        legend.position = "none")

ggplot(diamonds, aes(colour, price)) + 
  geom_jitter(aes(color=colour))+
  ggtitle("색상 품질에 따른 다이아몬드의 무게와 가격의 관계") + 
  xlab("색상 품질") + ylab("가격") + 
  theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"),
        legend.position = "none")

ggplot(diamonds, aes(clarity, price)) + 
  geom_jitter(aes(color=clarity))+
  ggtitle("순수함에 따른 다이아몬드의 무게와 가격의 관계") + 
  xlab("순수함") + ylab("가격") + 
  theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"),
        legend.position = "none")

cor(as.numeric(diamonds$colour), diamonds$price)
cor(as.numeric(diamonds$cut), diamonds$price)
cor(diamonds$clarity, diamonds$price)

summary(lm(diamonds$price ~ diamonds$clarity))
summary(lm(diamonds$price ~ diamonds$cut))
summary(lm(diamonds$price ~ diamonds$colour))
