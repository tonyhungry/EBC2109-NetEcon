# Scale Free
# Tony Hung

#### Setting up the script ####
# remember to set your working directory first!

require(igraph)
require(igraphdata)
# install.packages("igraphdata") 
# uncomment the above if you do not have the igraphdata package

data("enron")
?enron # Email network regarding the Enron Corporation. Read more here: https://www.cis.jhu.edu/~parky/Enron/Anno_Topic_exp_LDC.pdf 
# A bit more about the network here: http://konect.cc/networks/enron/ 

summary(enron) 
# enron is a directed unweighted network with 184 nodes and 125409 edges. 

#### 1. First Things First ####

# plot(enron,vertex.label = "", vertex.size = 4, edge.arrow.size = 0.15,main = "US Department of Justice Email Network") 
# plotting this network will take a lot of time, so totally do not recommend...

# However, we can already sneak a peak at the degree distributions

par(mfrow = c(2,1))
hist(degree(enron,mode="out"), main="Out degree distribution") # POWER LAW!
hist(degree(enron,mode="in"), main="In degree distribution") # POWER LAW!
par(mfrow = c(1,1))

outhist = hist(degree(enron,mode="out"))
inhist = hist(degree(enron,mode="in"))

plot(outhist$breaks[-1],outhist$counts,ylab="Number of nodes with X edges",xlab="Number of edges",main="Out degree distribution") # another way to look at the distributions
plot(inhist$breaks[-1],inhist$counts,ylab="Number of nodes with X edges",xlab="Number of edges",main="In degree distribution") # another way to look at the distributions

# As we can already observe from the plots that it looks like a power law distribution, we can now go ahead and make further analysis on those distributions. 

#### 2. Looking deeper at the degree distributions ####

ind <- degree(enron, mode='in')
outd <- degree(enron, mode='out')
summary(ind)
summary(outd)
in_prob <- degree.distribution(enron, mode='in') # this gives the degree distribution of all in-degrees
out_prob <- degree.distribution(enron, mode='out') # this gives the degree distribution of all out-degrees

# As we have some nodes with degree = 0, we should remove that in order to have a clearer picture when we are looking at the distribution. We should do the same for the degree distribution as well.

# Remove degree=0
in_prob <- in_prob[-1] 
out_prob <- out_prob[-1]
# Remove degrees with proportion=0
in_nonzero_pos <- which(in_prob!=0)
in_prob <- in_prob[in_nonzero_pos]
out_nonzero_pos <- which(out_prob!=0)
out_prob <- out_prob[out_nonzero_pos]

# Create a vector including all non-zero-probability degrees
indegree <- 1:max(ind)
indegree <- indegree[in_nonzero_pos]
outdegree <- 1:max(outd)
outdegree <- outdegree[out_nonzero_pos]

# Plot the probability on y axis and degree on x axis
par(mfrow=c(1,2), mar=c(4,4,2,2)) 
plot(in_prob ~ indegree, xlab='Indegree i', ylab='Probability P(X=i)', col='green')
plot(out_prob ~ outdegree, xlab='Outdegree o', ylab='Probability P(X=o)', col='red')

# Not exactly a graph that is informative. This is because we have 184 nodes and 125409 edges, where if you actually look at how the in and out degrees are distributed, it's rather spread out. In order to make a better judgement, we should look at the complementary cumulative distribution function (CCDF), where awe are plotting the probability of observing a certain in or out degree that is larger than said degree P(X>=x)
# Read more about CCDF here: https://en.wikipedia.org/wiki/Cumulative_distribution_function#Complementary_cumulative_distribution_function_(tail_distribution) 

in_ccdf <-NULL
for (i in 1:length(in_prob)) {
  in_ccdf[i] = sum( in_prob[ seq(i, length(in_prob)) ] )
}
out_ccdf <-NULL
for (i in 1:length(out_prob)) {
  out_ccdf[i] = sum( out_prob[ seq(i, length(out_prob)) ] )
}

par(mfrow=c(1,2), mar=c(4,4,2,2)) 
plot(in_ccdf ~ indegree, xlab='Indegree i', ylab='Complementary CDF P(X>=i)', log='xy', col='blue')
plot(out_ccdf ~ outdegree, xlab='Outdegree o', ylab='Complementary CDF P(X>=o)', log='xy', col='red')

#### 3. Determining the alpha for Power Law ####

# Essentially what you would like to do here is to determine the slope in the downward sloping part of the graph. For both degrees, I would probably take it starting from the 100, which means that we will exclude the first 100.

# In degree
reg <- lm( log(tail(in_ccdf,-100) ) ~ log(tail(indegree,-100)) )
coeff <- coef(reg)
coeff
par(mfrow=c(1,1))

# In degree plot
plot(in_ccdf ~ indegree, xlab='Indegree d', ylab='Complementary CDF P(X>=i)', log='xy', col='blue')
power_law_fit <- function(x) exp( coeff[[1]] + coeff[[2]]*log(x) )
curve(power_law_fit, col = "red", add = TRUE)

# Determining the alpha
# Since this is a CCDF where P(X>=x) ∝ x^(-alpha-1) and not P(X=x) ∝ x^(-alpha), thus we need to exponentiate using the formula: -(alpha-1), where the alpha is the coefficient from our linear regression.

-(coeff[[2]]-1) # gives us 2.409539
# This means then in terms of expressing this as a Power Law, we get P(X=x) ∝ x^(-2.409539)

# Out degree 
reg <- lm( log(tail(out_ccdf,-100) ) ~ log(tail(outdegree,-100)) )
coeff <- coef(reg)
coeff
par(mfrow=c(1,1))

# Out degree plot
plot(out_ccdf ~ outdegree, xlab='Outdegree d', ylab='Complementary CDF P(X>=o)', log='xy', col='blue')
power_law_fit <- function(x) exp( coeff[[1]] + coeff[[2]]*log(x) )
curve(power_law_fit, col = "red", add = TRUE)

# Determining the alpha
-(coeff[[2]]-1) # gives us 2.102921
# This means then in terms of expressing this as a Power Law, we get P(X=x) ∝ x^(-2.102921)

#### Extension: The Configuration Model (Not essential) ####

# Now we have concluded that our network follows the power law distribution and also found the alpha, we can now actually dig a little more and look at the configuration model. The configuration model generates a random graph from a given degree sequence. It starts from each node being assigned the same number of “stubs”, or half-edges, as their degree. The model then constructs the graph by randomly matching a stub from each of two different nodes to create an edge between them. In effect, the model does a degree-preserving randomization of the underlying network.

# In igraph, we use the sample_degseq() function to make the configuration model, solely based on the degree centralities of our actual network and we can compare it.

ind = degree(enron,mode="in")
outd = degree(enron,mode="out")

config = sample_degseq(out.deg = outd,in.deg=ind,method="simple")

# Computing the network statistics and put it in a table
res_table <- data.frame(
  c('Enron', 'Configuration Model'), 
  c(graph.density(enron), graph.density(config)),
  c(average.path.length(enron), average.path.length(config)),
  c(transitivity(enron), transitivity(config)))
colnames(res_table) <- c('Network', 'Density', 'Average Path Length', 'Transitivity')
res_table

# From the table, you can see that the density is exactly the same (no surprise over here). The APL is nearly one step shorter in the configuration model, as compared to our actual network. The transitivity is higher in the configuration model than our actual network. 
# Is this surprising? Not really. This is purely because in our actual network (enron), we have all these hubs that people are constantly emailing to and these hubs are the reason why we have these very long tails and the power law distribution. In the enron network, the edges tend to occur between the same nodes, but in the configuration model, the edges are more evenly distributed. 
