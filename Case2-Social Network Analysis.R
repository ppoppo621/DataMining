############################################################################
# Author: Xuan Wu              Date: 10/2/2019
############################################################################
# Case 2 - Social Network Analysis
# Set the working directory to appropriate folder on your machine, so as to access the data files.

# Load the libraries/packages.
install.packages("igraph",dependencies=TRUE, INSTALL_opts = c('--no-lock'))
library(igraph)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Description of the Network.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Consider an undirected network for individuals A, B, C, D, and E. A is connected to B
# and C. B is connected to A and C. C is connected to A, B, and D. D is connected to C
# and E. E is connected to D.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# a. Produce a network plot for this network.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# define links in data
edges <- rbind(c('A','B'),c('A','C'),c('B','C'),c('C','D'),c('D','E'))

# generate and plot graph, set directed=FALSE to plot an undirected graph.
g <- graph.edgelist(edges,directed=FALSE)
plot(g,vertex.size = 2, vertex.label.dist = 0.5)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# b. What node(s) would need to be removed from the graph for the remaining nodes 
# to constitute a clique? 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# draw a clique
clique <- delete_vertices(g,c('D','E'))
plot(clique,vertex.size = 2, vertex.label.dist = 0.5)

# Answer: nodes D and E need to be removed so that the left nodes constitute a clique.
print('nodes D and E need to be removed so that the left nodes constitute a clique.')
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# c. What is the degree for node A? 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# get the degree of node in the network.
degree(g)
# Answer: the degree for node A is 2.
print('the degree for node A is 2.')
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# d. Which node(s) have the lowest degree? 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Answer: Node E has the lowest degree.
print('Node E has the lowest degree.')
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# e. Tabulate the degree distribution for this network. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
g.dist <- degree_distribution(g)
barplot(g.dist*vcount(g),names.arg=seq(0,max(degree(g))),xlab='Degree',ylab='Frequency Percentage',main='Degree Distribution')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# f. Is this network connected? 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Answer: yes, the network is connect since each node is connected to at least one node.
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# g. Calculate the betweenness centrality for nodes A and C. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# calculate the betweenness
betweenness(g)
print('Answer: the betweenness of A is 0, of C is 4')
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# h. Calculate the density of the network. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#use edge_density() to calculate the density 
edge_density(g) 
print('The density of the network is 0.5')
