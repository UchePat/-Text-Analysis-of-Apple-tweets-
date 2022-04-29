# Text analysis and Graph Network. here we analyze text and create a graph network from the tokenized text

# Read file
mydata <- read.table("apple.txt", header = T, sep = ',')  # using read.table since it is a .txt file

# Build corpus
library(tm)     # tm - text mining

mycorpus <- iconv(mydata$text, to = "UTF-8")

mycorpus <- Corpus(VectorSource(mycorpus))


# Clean text/tweets corpuses
mycorpus <- tm_map(mycorpus, tolower)

mycorpus <- tm_map(mycorpus, removePunctuation)

mycorpus <- tm_map(mycorpus, removeNumbers)

ourcorpus <- tm_map(mycorpus, removeWords, stopwords('english'))

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)    # removes all URLs

ourcorpus <- tm_map(ourcorpus, content_transformer(removeURL))

ourcorpus <- tm_map(ourcorpus, removeWords, c('aapl', 'apple'))

ourcorpus <- tm_map(ourcorpus, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')

ourcorpus <- tm_map(ourcorpus, stripWhitespace)


# Term document matrix
tdm <- TermDocumentMatrix(ourcorpus)

tdm <- as.matrix(tdm)
tdm[1:10,1:10]     # displays d 1st 10 columns and rows


tdm <- tdm[rowSums(tdm)>30,]   # displays only text(row values) that are frequent

tdm[1:10,1:10]


# Network of terms
library(igraph)

tdm[tdm > 1] <- 1   # this means if d value in a row has a value dats > 1, then its assign value 1
tdm[1:10,1:10]    # any value > 1 becomes value 1

termM <- tdm %*% t(tdm)   
termM[1:10,1:10]

g <- graph.adjacency(termM, weighted = T, mode = 'undirected')   
g

g <- simplify(g)   
g

V(g)$label <- V(g)$name
V(g)$label

V(g)$degree <- degree(g)
V(g)$degree


# Histogram of node degree
hist(V(g)$degree, breaks = 100, col = 'green',           # breaks = 100 means creating 100 bars
     main = 'Histogram of Node Degree', ylab = 'Frequency',
     xlab = 'Degree of Vertices')   # d histogram is right-skewed since d bars are all on d RHS
# we see that d histogram is well dispersed in d chart when we remove d infrequent words


# Network diagram
set.seed(222)

plot(g)      

plot(g, vertex.color='green', vertex.size = 4,
     vertex.label.dist = 1.5, vertex.label = NA)   # vertex.label = NA will remove d values/label inside d circles
     

# Community detection: creating clusters of nodes that are in close/similar proximity to each oda
comm <- cluster_edge_betweenness(g)
plot(comm, g)

prop <- cluster_label_prop(g)
plot(prop, g)

greed <- cluster_fast_greedy(as.undirected(g))
plot(greed, as.undirected(g))


# Hub and Authorities
hs <- hub_score(g, weights = NA)$vector

as <- authority_score(g, weights = NA)$vector

par(mfrow = c(1,2))

plot(g, vertex.size = hs*50, main = 'Hubs',
     vertex.label = NA,
     vertex.color = rainbow(50))

plot(g, vertex.size = as*30, main = 'Authorities',
     vertex.label = NA,
     vertex.color = rainbow(50))

par(mfrow=c(1,1))


# Highlighting degrees
V(g)$label.cex <- 2.2*V(g)$degree / max(V(g)$degree) + 0.3   # d numbers(2.2 and 0.3) can be changed

V(g)$label.color <- rgb(0, 0, 0.2, 0.8)   # V - Vertex

V(g)$frame.color <- NA

egam <- (log(E(g)$weight) + 0.4) / max(log(E(g)$weight) + 0.4)  # E - Edge

E(g)$color <- rgb(0.5, 0.5, 0, egam)

E(g)$width <- egam

plot(g, vertex.color = 'green',
     vertex.size = V(g)$degree*0.5)    


# Network of tweets
tweetM <- t(tdm) %*% tdm    

g <- graph.adjacency(tweetM, weighted = T, mode = 'undirected')   # dis does not plot a graph but displays d attributes/structure in Console tab in

V(g)$degree <- degree(g)   # dis removes vertexes of same names that are connecting each other(ie it removes beat--beat)

g <- simplify(g)

hist(V(g)$degree, breaks = 100, col = 'green',
     main = 'Histogram of Degree', ylab = 'Freqency',
     xlab = 'Degree')


# Set labels of vertices to tweet IDs. we wanto input text values in d graph network circles
V(g)$label <- V(g)$name

V(g)$label.cex <- 1   # size of d text value

V(g)$label.color <- rgb(0.4, 0, 0, 0.7)

V(g)$size <- 2

V(g)$frame.color <- NA

plot(g, vertex.label=NA, vertex.size=4)


# Delete vertices
egam <- (log(E(g)$weight)+0.2)/ max(log(E(g)$weight)+ 0.2)

E(g)$color <- rgb(0.5, 0.5, 0, egam)

E(g)$width <- egam

g2 <- delete.vertices(g, V(g)[degree(g)<40])   # removing any vertex < 40 (use 30)

plot(g2, vertex.label.cex = 0.9,
     vertex.label.color = 'black')   # d numbers displayed are tweet/text indexes


# Delete edges
E(g)$color <- rgb(0.5, 0.5, 0, egam)

E(g)$width <- egam

g3 <- delete.edges(g, E(g)$weight <- 1)  # delete edges dat have weight < 1

g3 <- delete.vertices(g3, V(g3)[degree(g3)<20])

plot(g3)

apple$text[c(747, 430)]    # tweet index 747 and 430 are just 2 random tweets that we wanto view becuz dey are visible and separated from d huge mess of index values 



