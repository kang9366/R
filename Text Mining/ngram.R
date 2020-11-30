library(ngramr)
library(ggplot2)

options("scipen" = 100)
ng <- ngram(c("mozart", "beethoven"), year_start = 1800, year_end = 2020)
ggplot(ng, aes(x=Year, y=Frequency, colour=Phrase)) + geom_line()

ng <- ngram(c("thee", "you"))
ggplot(ng, aes(x=Year, y=Frequency, colour=Phrase)) + geom_line()

ng <- ngram(c("cute", "lovely", "pretty"))
ggplot(ng, aes(x=Year, y=Frequency, colour=Phrase)) + geom_line()

ggram(c("a unicorn", "an unicorn"), google_theme = T)
ggram(c("a unicorn", "an unicorn"), google_theme = T, geom = "area")

ng <- c("((The United States is + The United States has) / The United States)",
        "((The United States are + The United States have) / The United States)")
ggram(ng, year_start=1800, google_theme = TRUE) + theme(legend.direction = "vertical")
