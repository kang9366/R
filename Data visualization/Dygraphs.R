library(dygraphs)
dygraph(co2, main = "Maura Loa Atmosphere co2 Concentration") %>% 
        dyRangeSelector(dateWindow = c("1970-01-01", "1990-01-01"))
