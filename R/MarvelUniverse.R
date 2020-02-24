library(data.table)
library(dplyr)
library(stringr)
library(widyr)
library(igraph)
library(ggplot2)
library(visNetwork)
library(RColorBrewer)

char <- fread(paste0(getwd(), "/Data/Marvel/characters.csv"))
comic_char <- fread(paste0(getwd(),"/Data/Marvel/charactersToComics.csv"))

setkey(char, "characterID")
setkey(comic_char, "characterID")

#join the data
dat <- comic_char[char, nomatch = NA]

#count the number of time a character appears with another
word_pairs <- dat %>% pairwise_count(name, comicID, sort = TRUE)
word_pairs$item1 <- unlist(word_pairs$item1)
word_pairs$item2 <- unlist(word_pairs$item2)

#check
nrow(word_pairs); nrow(unique(word_pairs[,c("item1", "item2")]))

#remove duplicates
indx <- !duplicated(t(apply(word_pairs, 1, sort)))
mywords <- word_pairs[indx, ]

#filter by name
#mywords <- mywords %>% filter(item2 == "Wolverine" |item2 == "Wolverine")

#rename columns
colnames(mywords) <- c("Source","Target", "weight")
#apply filter
gdata <-  filter(mywords, weight >= 100)
#visualize
graph <- graph_from_data_frame(gdata, directed = FALSE)
clp <- cluster_label_prop(graph)
V(graph)$community <- clp$membership
V(graph)$betweenness <- betweenness(graph, v = V(graph), directed = FALSE)
V(graph)$degree <- degree(graph, v = V(graph))

data <- toVisNetworkData(graph)

nodes <- data[[1]]
nodes$shape <- "dot"
nodes$shadow <- FALSE # Nodes will drop shadow
nodes$title <- nodes$label # Text on click
nodes <- nodes[nodes$degree != 0, ]
nodes$group <- nodes$community
nodes$size <- ((nodes$betweenness / max(nodes$betweenness)) + .2) * 50 # Node size
nodes$borderWidth <- 2 # Node border width

#edges

nodes <- nodes
edges <- data[[2]]
edges$value <- edges$weight
edges <- edges[edges$value > 1, ]

col <- brewer.pal(12, "Set3")[as.factor(nodes$community)]
nodes$color.background <- col
nodes$color.border <- "black"

visNetwork(nodes, edges, width = "100%", height = "500px",label = nodes$label) %>%
  visPhysics(solver = "forceAtlas2Based",
             forceAtlas2Based = list(centralGravity = 0.02)) %>%
  visOptions(highlightNearest = TRUE, selectedBy = "community",
             nodesIdSelection = TRUE, clickToUse = TRUE) %>% 
  visInteraction(navigationButtons = TRUE)




