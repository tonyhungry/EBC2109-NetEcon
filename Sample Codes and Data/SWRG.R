# Small World / Random Graph 
# Tony Hung

#### Setting up the script ####
# remember to set your working directory first!

require(igraph)
require(igraphdata)
# install.packages("igraphdata") 
# uncomment the above if you do not have the igraphdata package

data("UKfaculty")
?UKfaculty

summary(UKfaculty)

#### 1. First Things First ####
plot(UKfaculty,vertex.label = "", vertex.size = 4, edge.arrow.size = 0.15,layout = layout.grid,main = "UK Faculty Friendship Network",vertex.color = V(UKfaculty)$Group)

g = as.undirected(UKfaculty) # removing the directions for an easier analysis
g = delete_edge_attr(g, "weight") # removing the weights for an easier analysis
summary(g) # checking it again

#### 2. Basic Stats ####

# Looking at the degree distribution 
plot(density(degree(g))) # Looking at the density function of degree centrality
hist(degree(g)) # another way to look at the distribution
summary(degree(g)) 

# Network Statistics
g_order <- vcount(g)
g_and <- mean(degree(g))
g_tran <- transitivity(g)
g_apl <- average.path.length(g)
g_dens = graph.density(g)

res_table <- data.frame(g_order, g_and, g_tran, g_apl,g_dens)
colnames(res_table) <- c('Number of Nodes', 'Average Degree', 'Transitivity', 'Average Path Length','Density')
res_table

#### 3. Modelling the Network as SW/RG ####

# The average degree in this network is 14.24691, so we will approximate it with 14. 
# We need to create multiple instances of the random and small world networks, because the process that generates them is probabilistic. This means that the networks generated in each run are unlikley to be identical so estimates will vary slightly. To account for this, we can repeat the process a number of times and average any measures over all instances. 
# There is no need to replicate the lattice since the process is deterministic.

# We can use the function sample_smallworld() from igraph to implement the small world model. The parameter "dim" determines the dimension of the lattice (use dim = 1 for a ring lattice), "size" – the number of nodes, "nei" – the radius of the local neighborhood (e.g., in a ring lattice nei = 2 means every node is initially connected to 4 fo their immediate neighbors, 2 on the left and 2 on the right), and "p" – the rewiring probability.

# Creating a lattice (Ising Model)
g_lat <- sample_smallworld(dim=1, size=81, nei=7, p=0) 

# Create multiple small worlds. For small worlds, we tend to to use values of p between 0.1 and 0.001.
g_sw <- lapply(rep(1, 100), function(x)
  sample_smallworld(dim=1, size=81, nei=7, p=0.1)) 

# Create multiple random networks. We set the p = 1. 
g_ran <- lapply(rep(1, 100), function(x)
  sample_smallworld(dim=1, size=81, nei=7, p=1))

#### 4. Compare Results ####

g_lat_apl <- average.path.length(g_lat)
g_lat_tran <- transitivity(g_lat)
g_sw_apl <- sapply(g_sw, average.path.length)
g_sw_tran <- sapply(g_sw, transitivity)
g_ran_apl <- sapply(g_ran, average.path.length)
g_ran_tran <- sapply(g_ran, transitivity)

res_table1 <- data.frame(c('Friendship', 'Ising', 'Small World', 'Random'), 
                        c(g_apl, g_lat_apl, mean(g_sw_apl), mean(g_ran_apl)), 
                        c(g_tran, g_lat_tran, mean(g_sw_tran), mean(g_ran_tran)))
colnames(res_table1) <- c('Name', 'Average Path length', 'Transitivity')
res_table1

#### 5. Analyze The Results ####
# From the results, it is easy to see that the friendship network most closely resemble the small world network. In both the friendship network and the small world sample, the average path length is around 2, and the transitivity is around 0.4. In comparison to the sample random graph network and the Ising model, the transitivity in the small world and friendship network is high. 

# This high transitivity means that the friendship network has a lot of clusters, and as to specifically how, we should look at the assortativity of the friendship network. The clusters could mean that friendships circles in the network could be formed due to some sort of common characteristics, such as research interests, or simply being in the same university/institution.

# By looking the distribution of the degree centrality in the network, we can see that the distribution is skewed, which means that the majority of the people has only about 10 friends, but then in combination of the high transitivity, this means that although the there aren't a lot of connections for each person, but the clusters are rather dense itself. The short average path length in the network makes it possible such that the information between different faculty members can travel fast. By looking at the graph of the network, we can see that there are certain hubs that could make this possible. 


