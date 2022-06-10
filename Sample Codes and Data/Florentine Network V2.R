#### Florentine Network #####
# EBC2109

#### Install Packages #### 
# Comment out a section by highlighting it, and then cmd+shift+c
# install.packages("igraph")
# install.packages("ergm")
# install.packages("network")
# install.packages("intergraph")
# install.packages("scales")

#### Setting Up the Script####
library(ergm) # load ergm into the environment

data("florentine")
?"florentine" # tells you a bit more about the Florentine dataset

detach(package:ergm)
detach(package:network)
require(intergraph)
require(igraph)
require(scales)

flo_m<-asIgraph(flomarriage) # converting the flomarriage from a Networks object into an Igraph object
flo_b<-asIgraph(flobusiness) # converting the flobusiness from a Networks object into an Igraph object

is.directed(flo_m) # checks if the graph is directed or not
is.directed(flo_b)

is.weighted(flo_m) # checks if the graph is weighted or not
is.weighted(flo_b)

summary(flo_b) # gives a summary of the network: number of nodes + edges, what kind of attributes it has
summary(flo_m)


#### Plotting the networks ####
plot.igraph(flo_b,edge.arrow.size=0,vertex.color="darkorange",vertex.frame.color="white",edge.color="darkgrey",edge.width=2,vertex.label.family="sans",vertex.label=V(flo_b)$vertex.names,main="Florentine Business Network")

plot.igraph(flo_m,edge.arrow.size=0,vertex.color=degree(flo_m),vertex.frame.color="white",edge.color="darkgrey",edge.width=2,vertex.label.family="sans",vertex.label=V(flo_m)$vertex.names,main="Florentine Marriage Network")
#legend("topleft", legend = levels(f), pch = 16, col = vcols, bty = "n")


#### Number of Nodes and Edges ####

vcount(flo_b) # gives the number of nodes in the network, thus 16 nodes
gorder(flo_b) # another way to see how many nodes there are

ecount(flo_b) # gives the number of edges in the network, thus 15 edges
gsize(flo_b) # another way to see how many edges there are

#### Converting to Adjacency Matrix + Edgelist ####

adj_flo_b = as.matrix(get.adjacency(flo_b)) # a bit different than the PDF. Converts the network into an adjacency matrix
adj_flo_b1 = as.matrix(as_adj(flo_b)) # a bit different than the PDF

el_flo_b = get.edgelist(flo_b) 
el_flo_b1 = as_edgelist(flo_b)

#### Network Demographics: Unweighted Networks ####

## Degree Centrality: the number of edges each node has
deg_flo_b = degree(flo_b) # degree centrality for each node in the flo_b network
mean(degree(flo_b)) # average degree centrality in the flo_b network
mean(deg_flo_b) # alternatively, since you've saved the degree centralities, you can also 

## Density: the number of actual links divided by the number of potential links
edge_density(flo_b) # calculating the density of the network. So this means that 12.5% of the possible total edges are present in this network

## Path Length: calculates the length of all the shortest paths from or to the vertices in the network

dist_flo_b = distances(flo_b) # it gives a matrix/table
colnames(dist_flo_b) = rownames(dist_flo_b) = V(flo_b)$vertex.names # putting in names so you can actually read the table a bit better... The Inf means that there's no possible connection. If you look at the graph, you can see that there are a lot of isolates in this network. 

## Average Path Length: the average geodesic paths between all pairs of nodes
apl_flo_b = mean_distance(flo_b,unconnected=T) # this means that on average, the shortest path between nodes are 2.38 steps.
apl_flo_b1 = average.path.length(flo_b,unconnected=T) # same thing but different function

apl_flo_b_vector =  rowMeans(dist_flo_b) # this gives you the vector of average path lengths for each node. As you can see it doesn't really work over here because we have a disconnected graph.

## Diameter: the longest shortest path
diam_flo_b = diameter(flo_b) # this means that the highest geodesic path is 5 

## Closeness Centrality: measures the average distance to all other vertices. The higher the closer.
clos_flo_b = closeness(flo_b) # as you can see, because there are isolates in the graph, closeness is not that well-defined

min(clos_flo_b) 

## Reach Centrality: basically like closeness, but solves the problem of isolates/ disconnected graphs. The higher the better.
tempMatrix = 1/dist_flo_b
diag(tempMatrix) = 0
reach_flo_b = rowSums(tempMatrix)

#### Network Demographics: Weighted Networks ####
# For this section, we will be using our Eurovision dataset again. In the previous exercise, we sort of just ignore the weighted edges, but now since we have the knowledge, we can then also use the more appropriate tools to look at the Eurovision network.

# Quick set up for this part...
require(rio)
eurovision_final_2017 = import("eurovision_final_2017.csv")

eurov <- graph.data.frame(eurovision_final_2017, directed=T)
eurov <- set_edge_attr(eurov, "weight", value= eurovision_final_2017$Points) # we need to set the weights as points, or else Igraph doesn't recognize it. There are two ways to do this. Either you can go back to the original data frame and rename the Points into weight or you can make a new attribute, which is what I've done here and added to the Igraph object.

summary(eurov) # you can see now that there's an additional attribute.
votingpoints = as.factor(unique(E(eurov)$Points))

plot(eurov,edge.arrow.size=.3,vertex.frame.color="white",edge.color=E(eurov)$Points,edge.width=0.5*E(eurov)$Points,vertex.label.family="sans",main="Eurovision 2017 Voting Results")

is.directed(eurov)
is.weighted(eurov)

## Strength: the weighted version of degree centrality. Measures how strong is the connection 
strength(eurov) # measures the total strength
strength(eurov,mode="in") # measures the strength of  incoming edges
strength(eurov,mode="out") # measures the strength of outgoing edges

## Average Strength
mean(strength(eurov)) # measures the average of total strength
mean(strength(eurov,mode="in")) # measures the average strength of incoming edges
mean(strength(eurov,mode="out")) # measures the average strength of outgoing edges

## Path Length: calculates the length of all the shortest paths from or to the vertices in the network
euro_dist = distances(eurov,weights=E(eurov)$Points) # it gives a matrix/table
colnames(euro_dist) = rownames(euro_dist) = V(eurov)$name 

## Average Path Length. Stays the same as before...
apl_eurov = mean_distance(eurov)
apl_eurov1 = average.path.length(eurov) # same thing but different function

apl_eurov_vector =  rowMeans(euro_dist) # this gives you the vector of average path lengths for each node.

## Diameter
diameter(eurov,weights=E(eurov)$Points) # it says 42... If you run this command without the weight component, it will tell you the diameter is 4. What do you think this means? Since the weights are the points that countries are giving to each other, so then maybe this means the highest cumulative amount of points that countries are giving to each other is 42, given that it forms a path length of 4. 

#### Local Structures: unweighted, undirected network ####
# For this section, we will go back to the Florentine networks again to look at a few things...

## Transitivity/clustering/cliquishness: measures whether a node is embedded in a tightly knit group of clique, of other nodes.

# local transitivity: gives you the transitivity per node
loc_trans_flo_b = transitivity(flo_b,type="local")

# global transitivity: gives you the overall transitivity
glo_trans_flo_b = transitivity(flo_b,type="global")

## Betweenness transitivity: measures how often a node lies on the geodesic path between other nodes. Implies brokerage. The higher, the more in between nodes.
bet_flo_b = betweenness(flo_b)

## Eigenvector centrality: measures the type of connections you have. If you have a lot of powerful/important connections, then this means that you are more important.The higher the better. Remember our Mean Girls example? 
eigen_flo_b = eigen_centrality(flo_b)$vector

## Assortativity/homophily: measures how nodes will connect with each other based on a characteristic. Should interpret these values just like correlation

# In the business network, we have the wealth attribute. The wealth attribute gives each family's net wealth in 1427.
assortativity(flo_b,types1=V(flo_b)$wealth) #The Console returns -0.2, which means that wealth and is negatively correlated to how families connect. This also means that in terms of wealth, there is some dissortativity (as represented by the negative values). What can this possibly tell us? Perhaps business relationships are not built on top of how wealthy each family is...

#### Extension 1: Table Comparison ####

# Whew, we've surely made a lot of different metrics in this script. 

families = V(flo_b)$vertex.names # extracting the list of names from the network and save it as a variable

flo_b_stats = cbind(families,deg_flo_b,apl_flo_b_vector,clos_flo_b,reach_flo_b,loc_trans_flo_b,bet_flo_b,eigen_flo_b)
colnames(flo_b_stats) = c("Florentine Families","Degree Centrality","Average Path Length per Node","Closeness Centrality","Reach Centrality","Local Transitivity","Betweenness Centrality","Eigenvector Centrality") # As you can see, the isolates are REALLY messing up some of the things we would like to know about...

#### Extension 2: Removing Isolates ####

# As we have seen in the business network, keeping the isolates really do mess things up. This is especially the case for determining closeness.  
flo_b_no_i = delete.vertices(flo_b, degree(flo_b)==0)
summary(flo_b_no_i)

# Let's plot it.
plot.igraph(flo_b_no_i,edge.arrow.size=0,vertex.color="darkorange",vertex.frame.color="white",edge.color="darkgrey",edge.width=2,vertex.label.family="sans",vertex.label=V(flo_b_no_i)$vertex.names,main="Florentine Business Network No Isolates")

# So then let's make another table with the same metrics as from the above section and see perhaps things have improved...

tempMatrix = 1/distances(flo_b_no_i)
diag(tempMatrix) = 0
reach_flo_b_no_i = rowSums(tempMatrix)

# As compared to above, you can basically create columns directly functions and then just bind it together. Pretty nice, but only if you are 100% sure of what you are doing. Unfortunately for our reach centrality, we need to compute it before hand.

flo_b_stats_i_removed = as.data.frame(cbind(V(flo_b_no_i)$vertex.names,degree(flo_b_no_i),rowMeans(distances(flo_b_no_i)),closeness(flo_b_no_i),reach_flo_b_no_i,transitivity(flo_b_no_i,type="local"),betweenness(flo_b_no_i),eigen_centrality(flo_b_no_i)$vector))
colnames(flo_b_stats_i_removed) = c("Florentine Families","Degree Centrality","Average Path Length per Node","Closeness Centrality","Reach Centrality","Local Transitivity","Betweenness Centrality","Eigenvector Centrality")

# Saw anything surprising about this? We see that there is NaN for the local transitivity of Pazzi, Salviati, and Tournabuori. Is this surprising? No. Transitivity measures triads. All three of those families are only dyads, with their only connection being the Medici family. 