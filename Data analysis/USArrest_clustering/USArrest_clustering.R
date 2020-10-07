library(graphics)
library(magrittr)
library(MASS)
library(psych)
library(cluster)
install.packages("factoextra")
data("USArrests")
head(USArrests)
str(USArrests)

# 4가지 변수의 척도가 다르므로 scale 함수를 적용해 표준 점수로 변환 시킨다.
USArrests <- USArrests %>% scale
dim(USArrests)

# Hierarchical Clustering
# 군집 간 거리를 측정하여 가장 유사한 군집끼리 묶음, 하나의 군집이 형성될 때까지 반복

# 주어진 행렬을 거리 행렬로 변환
dist.mat <- as.dist(1 - cor(t(USArrests)))

h1 <- hclust(dist.mat, method = "ward.D")
plot(h1)

#cutree : 군집의 인덱스를 구하는 함수
clusterCut1 <- cutree(h1, k=3)
rect.hclust(h1, k = 3)

h1 <- hclust(dist.mat, method = "ward.D")
h2 <- hclust(dist.mat, method = "complete")
h3 <- hclust(dist.mat, method = "average")
h4 <- hclust(dist.mat, method = "single")

plot(h1)
plot(h2)
plot(h3)
plot(h4)
