#LIBRARIES
library(rgexf)
library(igraph)
library(rio)

#IMPORT DATA
#Loads networks gexf data into variables
g1988 <- read.gexf("net_se1988-1991.gexf")
g1991 <- read.gexf("net_se1991-1994.gexf")
g1994 <- read.gexf("net_se1994-1998.gexf")
g1998 <- read.gexf("net_se1998-2002.gexf")
g2002 <- read.gexf("net_se2002-2006.gexf")
g2006 <- read.gexf("net_se2006-2010.gexf")
g2010 <- read.gexf("net_se2010-2014.gexf")

#Loads datasets with gender and age
data2002 = import("2002matched.xls", "xls")
data2006 = import("2006matched.xls", "xls")
data2010 = import("2010matched.xls", "xls")

# Converts networks to format usable by igraph
ig1988 <- gexf.to.igraph(g1988)
ig1991 <- gexf.to.igraph(g1991)
ig1994 <- gexf.to.igraph(g1994)
ig1998 <- gexf.to.igraph(g1998)
ig2002 <- gexf.to.igraph(g2002)
ig2006 <- gexf.to.igraph(g2006)
ig2010 <- gexf.to.igraph(g2010)

#PLOTS
# Plots a network graph
plot(ig1988, edge.arrow.size=.002, 
     vertex.size=3, 
     vertex.label.cex=0.8, 
     vertex.label.dist=0, 
     edge.curved=0.2,
     vertex.label=NA)

# Adding a legend (must be color adjusted to match dataset)
legend(x=-1.5, y=-1.1, c("Centerpartiet",
                         "Moderaterna", 
                         "Socialdemokraterna", 
                         "Folkpartiet",
                         "Vänsterpartie"), 
       pch=21, pt.cex=2, cex=.8, bty="n", ncol=1)

# Histogram of the nodes
deg <- degree(ig1988, mode="all")
hist(deg, breaks=1:vcount(ig1988)-1, main="Histogram of node degree")


# Degree Distribution
deg.dist <- degree_distribution(ig1988, cumulative=T, mode="all")

plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      
      xlab="Degree", ylab="Cumulative Frequency")


#DESCRIPTIVE STATISTICS 

#Number of edges 
ecount(ig2010)
vcount(ig2010)

#Degree mean, median, stdev
mean(degree(ig2002)) # average total degree centrality
mean(degree(ig2006)) # average total degree centrality
mean(degree(ig2010)) # average total degree centrality

median(degree(ig2002))
median(degree(ig2006))
median(degree(ig2010))

sd(degree(ig2002))
sd(degree(ig2006))
sd(degree(ig2010))

#Density and diameter
edge_density(ig2002)
edge_density(ig2006)
edge_density(ig2010)
diameter(ig2002)
diameter(ig2006)
diameter(ig2010)

#Average path length
mean_distance(ig2002,unconnected=T)
mean_distance(ig2006,unconnected=T)
mean_distance(ig2010,unconnected=T)

global_efficiency(ig2002)
global_efficiency(ig2006)
global_efficiency(ig2010)

#Closeness for each MP
g_betw2 <- Closeness(ig2002)
g_betw2 = data.frame(round(g_betw2, 2))
g_betw6 <- Closeness(ig2006)
g_betw6 = data.frame(round(g_betw6, 2))
g_betw10 <- Closeness(ig2010)
g_betw10 = data.frame(round(g_betw10, 2))

#degreee for each MP
g_deg2 = degree(ig2002)
g_deg2 = data.frame(round(g_deg2, 2))
g_deg6 = degree(ig2006)
g_deg6 = data.frame(round(g_deg6, 2))
g_deg10 = degree(ig2010)
g_deg10 = data.frame(round(g_deg10, 2))

#closeness for each MP
g_close2 <- closeness(ig2002)
g_close2 = data.frame(g_close2)
g_close6 <- closeness(ig2006)
g_close6 = data.frame(g_close6)
g_close10 <- closeness(ig2010)
g_close10 = data.frame(g_close10)

#eigenvalue for each MP
eigen2 = as.matrix(eigen_centrality(ig2002)$vector)
eigen6 = as.matrix(eigen_centrality(ig2006)$vector)
eigen10 = as.matrix(eigen_centrality(ig2010)$vector)




#REGRESSIONS
lmfemaledegree = lm(degree ~ female, data2010)
summary(lmfemaledegree)
lmmaledegree = lm(degree ~ male, data2010)
summary(lmmaledegree)

lmAge = lm(degree ~ age, data2010)
summary(lmAge)

lmfemaleBetweenness = lm(Betweenness ~ female, data2010)
summary(lmfemaleBetweenness)
lmmaleBetweenness = lm(Betweenness ~ male, data2010)
summary(lmmaleBetweenness)

lmfemaleCloseness = lm(Closeness ~ female, data2010)
summary(lmfemaleCloseness)
lmmaleCloseness = lm(Closeness ~ male, data2010)
summary(lmmaleCloseness)

lmfemaleEigenvalue = lm(Eigenvalue ~ female, data2010)
summary(lmfemaleEigenvalue)
lmmaleEigenvalue = lm(Eigenvalue ~ male, data2010)
summary(lmmaleEigenvalue)

lmpartyDegree = lm(degree ~ Folkpartiet + Socialdemokraterna + Moderaterna + Kristdemokraterna + Centerpartiet + V?nsterpartiet + Milj?partiet + Sverigedemokraterna + Independent, data2010)
lmpartyDegreeRed = lm(degree ~ Socialdemokraterna + V?nsterpartiet + Milj?partiet + Sverigedemokraterna, data2010)
summary(lmpartyDegree)
summary(lmpartyDegreeRed)

lmorientation = lm(degree ~ orientation, data2010)
summary(lmorientation)

lmAll = lm(degree ~ votes + female + age + orientation + Folkpartiet + Socialdemokraterna + Moderaterna + Kristdemokraterna + Centerpartiet + V?nsterpartiet + Milj?partiet + Sverigedemokraterna + Independent, data2010)
summary(lmAll)

lmVotes = lm(degree ~ votes, data2010)
summary(lmVotes)


# Boxplots on Degree 
Parliament2006 = degree(ig2006)
g_deg6 = data.frame(Parliament2006)
Parliament2002 = degree(ig2002)
g_deg2 = data.frame(Parliament2002)
Parliament2010 = degree(ig2010)
g_deg10 = data.frame(Parliament2010)

boxplot(c(g_deg2,g_deg6,g_deg10), xlab="Parliament", ylab="Degree per MP")

# ANOVA on avg. Degree in three legislation periods
anova_test <- import("anova.xlsx")
res.aov <- aov(degree ~ year, anova_test)

summary(anova_test)
summary(res.aov)

#EXPORTS
export(as.data.frame(vertex.attributes(ig2010)), "att.csv")

export(eigen2, "eigen2.csv")
export(eigen6, "eigen6.csv")
export(eigen10, "eigen10.csv")

export(g_betw2, "betw2.csv")
export(g_betw6, "betw6.csv")
export(g_betw10, "betw10.csv")

export(g_close2, "close2.csv")
export(g_close6, "close6.csv")
export(g_close10, "close10.csv")

export(g_deg2, "deg2.csv")
export(g_deg6, "deg6.csv")
export(g_deg10, "deg10.csv")

