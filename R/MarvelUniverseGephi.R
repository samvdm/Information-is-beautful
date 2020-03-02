library(data.table)
library(widyr)
library(igraph)
library(visNetwork)

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

#rename columns
colnames(mywords) <- c("Source","Target", "weight")

#save graph object
gl <- graph_from_data_frame(mywords, directed=F)
write_graph(gl, "marvelcomics.graphml", format ="graphml")

