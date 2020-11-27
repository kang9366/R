library(HistData)
library(dplyr)

p <- PearsonLee
fs <- p %>% 
  filter(gp=="fs")

fd <- p %>% 
  filter(gp=="fd")

ms <- p %>% 
  filter(gp=="ms")

md <- p %>% 
  filter(gp=="md")

par(mfrow=c(2,2))
plot(fs$parent, fs$child)
plot(fd$parent, fd$child)
plot(ms$parent, ms$child)
plot(md$parent, md$child)