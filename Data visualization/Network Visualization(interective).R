library(visNetwork)

nodes <- read.csv("./data/NODES.csv")
links <- read.csv("./data/LINKS.csv")

nodes$label <- nodes$media
nodes$size <- nodes$audience.size
nodes$color <- ifelse(nodes$media.type==1, "red",
                      ifelse(nodes$media.type==2, "blue", "green"))

visNetwork(nodes, links)

