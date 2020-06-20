#install.packages("igraph") # install the package, if not already installed

###################    Part I: Network construction and visulaization   ################################################
library("igraph")
?igraph
## A number of useful functions in this package can be accessed at http://igraph.org/r/doc/.

###### Example 1: Network using list of edges  ##################
g1=graph(c(1,2,1,3,2,3,3,5), n = 5)
plot(g1)

g2 = graph(c(1,2, 1,3, 2,3, 3,5), n = 5,directed = F)
plot(g2)

V(g2)
E(g2)

###### Example 2: Star Network using Adjacency Matrix from file ##################
# Load the adjacency matrix from the csv file
star <- read.csv("C:/Datasets_BA/Network Analytics/star.csv", header=TRUE)
head(star) # shows initial few rows of the loaded file

# create a newtwork using adjacency matrix
?graph.adjacency # help file for the api graph.adjacency
starNW <- graph.adjacency(as.matrix(star), mode="undirected")
plot(starNW)


###### Example 3:  Another Star using Adjacency Matrix  ##################
# Load the adjacency matrix from the csv file
another_star <- read.csv("C:/Datasets_BA/Network Analytics/Anotherstar.csv", header=TRUE)
head(another_star) # shows initial few rows of the loaded file
# create a newtwork using adjacency matrix
AnotherstarNW <- graph.adjacency(as.matrix(another_star), mode="undirected", weighted=TRUE)
plot(AnotherstarNW)

###### Example 4: Circular Network using Adjacency Matrix  ##################
# Load the adjacency matrix from the csv file
circular <- read.csv("C:/Datasets_BA/Network Analytics/Circular.csv", header=TRUE)
head(circular) # shows initial few rows of the loaded file
# create a network using adjacency matrix
CircularNW <- graph.adjacency(as.matrix(circular), mode="undirected", weighted=TRUE)
plot(CircularNW)

###### Example 5: Network using list of edges  ##################
# Load the graph from an webpage
?read.graph
Karate <- read.graph("http://cneurocvs.rmki.kfki.hu/igraph/karate.net", format="pajek")
plot(Karate)
plot(Karate,layout=layout.circle,vertex.label=NA)
plot(Karate,layout=layout.circle)
plot(Karate,layout=layout_on_grid)
## some other layout for plots: layout.fruchterman.reingold, layout.random, layout.drl, layout.sphere, layout.kamada.kawai

