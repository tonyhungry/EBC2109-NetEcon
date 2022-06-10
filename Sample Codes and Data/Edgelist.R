#### EDGELIST!! ####
# Tony Hung

require(igraph)
require(rio)

classnet<-graph(edges=c("Ines","Matthias",
                        "Ines","Paula",
                        "Ines","JL",
                        "Ines","Me",
                        "Ines","Sasha",
                        "Ines","Linus",
                        "Ines","Clemens",
                        "Ines","Albert",
                        "Ines","Giovanni",
                        "Ines","James",
                        "Ines","Flavio",
                        "Avan","Marco",
                        "Matthias","Paula","Matthias","JL","Matthias","Me","Matthias","Sasha","Matthias","Clemens","Matthias","James","Paula","JL","Paula","Me","Paula","Sasha","Paula","Clemens","Paula","James","JL","Me","JL","Sasha","JL","Clemens","Me","Sasha","Me","Clemens","Linus","Clemens","Linus","Giovanni","Linus","James","Linus","Flavio","Linus","Marco","Jonas","Clemens","Jonas","Albert","Jonas","Avan","Clemens","Albert","Clemens","Avan","Albert","Avan","Giovanni","James","Giovanni","Flavio","Giovanni","Marco","James","Flavio","James","Marco","Flavio","Marco","Me","Jonas","Me","Albert","Me","James","Me","James","Me","Flavio","Me","Marco","Me","Linus","Albert","Albert"),directed=T) #You just made an igraph object!

# You need to find out whether or not your data is directed!

plot(classnet)

plot.igraph(classnet,edge.arrow.size=.3,vertex.color="darkorange",vertex.frame.color="white",edge.color="darkgrey",edge.width=2,vertex.label.family="sans",main="Tony's Macro Tutorial Observation")

## Adding vertex attribute
V(classnet) #first look at the different nodes in my tutor network
V(classnet)$Gender <- c(0,1,0,1,1,1,1,1,1,1,1,1,0,1,1) # added the gender component: 1 = M, 0 = F


#### NETWORK BASICS ####
# Looking at some network basics
summary(classnet)
degree(classnet)
mean(degree(classnet)) # average total degree centrality
mean(degree(classnet,mode="in")) # average in degree centrality
mean(degree(classnet,mode="out")) # average out degree centrality DO WE NEED THIS IN UNDIRECTED GRAPHS?
graph.density(classnet) # percentage between number of edges in the network and the total possible number of the network
diameter(classnet) # the longest geodesic path in the network. Geodesic path is the shortest path

#### Description of the network ####
# This is an observation of my Macroeconomics tutorial last period These are the students that I know and often times you will see some type of discussion going on in the class, especially when the class is going divided into small groups to discuss a certain question. The question I used to plot this network is: "Who talks with who in the Macroeconomics Tutorial?"

# I listed 15 people, which means that there are 15 nodes in the network. There are 53 edges in this network as well. The edges are directed, as at times, the conversations between students are more of a one directional chit chat. The average total degree is 7.06, which means that on average, people in the network talks to 7 other people (whether they are the listener or the one talking. On average, the in and out degrees are 3.53. This means that each person either listens or talks to 3.5 other people. The person other than me, who has the most degree centrality is Ines. She talked to 11 people in the tutorial, where as I talked to 13 different people. The diameter of the network is 3, meaning that everyone in this network is pretty closely connected, reachable in only a few intermediaries. The density of the network is 0.25, which means that 25.2% of the possible ties are present.

# From the directions, you could definitely who are chatterboxes and who are more of a listener in the tutorial


#### Misc ####
# Another way to plot the graph 
plot(classnet,vertex.color=V(classnet)$Gender,vertex.size=degree(classnet),edge.arrow.size=.3) 
# plotting the size of the nodes according to the number of edges each node has, adjusted the arrow size, and blank if the node is a female

plot(classnet,vertex.size=degree(classnet),edge.arrow.size=.3,vertex.color=degree(classnet))
# Node color now represents the degree of each node

## Edge List
classEdgeList = get.edgelist(classnet)

export(classEdgeList, "CEL.csv") # please use more informative names

## Adjacency Matrix
ClassAdjM = as.matrix(as_adj(classnet))

export(ClassAdjM, "CAM.csv")

#### Eurovision #### 

eurovision_final_2017 <- read.csv("eurovision_final_2017.csv")
View(eurovision_final_2017)

eurov <- graph.data.frame(eurovision_final_2017, directed=T)
summary(eurov)
