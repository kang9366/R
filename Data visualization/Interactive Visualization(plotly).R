library(arulesViz)

data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.002, confidence=0.8))
rules

plot(rules, method="graph", control=list(type="items", vertex.color="blue"))
plot(rules, method="graph",
   control=list(nodeCol=heat.colors(5),
                edgeCol="black"),engine="htmlwidget")

library(plotly)
data("iris")
plot_ly(data=iris, x=~Sepal.Length, y=~Petal.Length, color=~Species, col="Set1")
plot_ly(data=iris, x=~Sepal.Length, y=~Petal.Length, type="scatter", mode="markers",
        symbol=~Species, symbols=c("circle", "x", "o"),
        color=I('black'), marker=list(size=10))
plot_ly(z=~volcano, tuype="surface")

library(DT)
datatable(iris, options=list(pageLength=5))
