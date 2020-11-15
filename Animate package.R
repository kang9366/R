install.packages("animation")
library(animation)

ani.options(interval = 0.5)
sum <- 0

for(i in 1:100){
  plot.new()
  if(i%%3 == 0){
    sum <- sum + i
  }
  text(0.5, 0.5, sum, cex = 10, col = "black")
  ani.pause()
}