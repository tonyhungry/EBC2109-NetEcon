# Bipartite Example

#### Setting Up Your Project ####

# Set up the working directory if necessary
# Load required packages and data

require(igraph)
library(readxl)
macro_groups <- read_excel("C+P Groups.xlsx") # loading in data. You can also load it by using the import data button, and click From Excel
macro_p_info <- read_excel("C+P Info.xlsx")

#### Taking a first look ####

head(macro_groups) # To see how the data looks like.
# As you can see, there are two different types of projects. The C projects are the chapters each pair/triple has to write, and the P projects are the presentations that each team of five has to present. 

macronet = graph.data.frame(macro_groups, directed=FALSE)
summary(macronet) # not a lot of attributes, we should add a few more later when we have extracted the appropriate graphs

plot(macronet) # Not very pretty, but we can see how students are linked together through the different projects that people have worked on

bipartite_mapping(macronet) # this function tells you if your igraph object satisfies to be a bipartite network. If the $res returns true, then it is fine.

# The $type tells you which mode of projection. We need to add this to our network. 
V(macronet)$type <- bipartite_mapping(macronet)$type


#### Plotting the graph as a classic bipartite network ####


V(macronet)$color <- ifelse(V(macronet)$type, "#33FFD1", "#FF3369") # over here you are assigning colors according to the type of nodes. You can find more HTML HEX color codes here: https://htmlcolorcodes.com 
V(macronet)$shape <- ifelse(V(macronet)$type, "circle", "square") # over here you are assigning shapes according to the type of nodes
E(macronet)$color <- "lightgray" # assigning 

plot(macronet, vertex.label.cex = 0.5, vertex.label.color = "black",vertex.label.family="sans")

plot(macronet, layout=layout.bipartite, vertex.label.cex=0.5, vertex.label.family="sans",vertex.size = 6)


#### Extracting the two projections ####
# This part is verrryyyy important

projections = bipartite.projection(macronet)

plot(projections$proj1) # This is the projection of the students
plot(projections$proj2) # This is the projection of the projects

students = projections$proj1 # saving the appropriate projections as igraph objects
projects = projections$proj2

summary(students)

#### Adding attributes ####

V(students)$gender = as.factor(macro_p_info$Gender) #
V(students)$age = macro_p_info$Age
V(students)$country = as.factor(macro_p_info$Country)

summary(students)



#### Making Subgraphs ####
# In order to make the appropriate subgraphs, you can do it in many ways. Below I'll show a few possibilities. 

# Deleting vertices based on some characteristic
female_students = delete.vertices(students, V(students)$gender==2) # This command delete vertices based on some sort of "logic"
# Since we have added this attribute as a factor, we need to figure out which level is which. The first level is female and the second level is male, thus the 2 in the command. You can check this by using: levels(as.factor(macro_p_info$Gender))

plot(female_students,vertex.label.cex=0.5, vertex.label.family="sans",vertex.size = 6)

# Keeping only German students
levels(as.factor(macro_p_info$Country))
german_students = delete.vertices(students, V(students)$country!=3) # Keeping only German students, which means then you are deleting every other vertex that is not classified as German
plot(german_students, vertex.label.cex=0.5, vertex.label.family="sans",vertex.size = 6)

# Subsetting data using dplyr

require(dplyr)

p1 = macro_groups %>% filter(Project == "P1") 
p1net = graph.data.frame(p1,directed = F)
plot(p1net, vertex.label.cex=0.5, vertex.label.family="sans",vertex.size = 6)

summary(p1net) # As you can see, this creates some sort of difficulty when adding attributes to our graphs because the info data no longer matches to the names that we have!!

p1_info = left_join(p1,macro_p_info,by="Name") # what this does is that it only keeps the observations from the macro_info data when it matches the names that are also present in the p1 data frame. 

# Before adding attributes, we still need to correctly make a projection of the graphs

bipartite_mapping(p1net)
V(p1net)$type <- bipartite_mapping(p1net)$type

p1_students = bipartite.projection(p1net)$proj1

# Add attributes!
V(p1_students)$gender = as.factor(p1_info$Gender)
V(p1_students)$age = p1_info$Age
V(p1_students)$country = as.factor(p1_info$Country)

plot(p1_students, vertex.color = V(p1_students)$gender,edge.curved=.3,main="Presentation Team 1", vertex.label.cex=0.5, vertex.label.family="sans",vertex.size = 6)

#### Correlations ####
# Between two continuous variables
cor(eigen_centrality(students)$vector,V(students)$age)

# Between multiple continuous variables
eig = eigen_centrality(students)$vector
clos = closeness(students)
deg = degree(students)
age = V(students)$age

df = data.frame(eig,clos,age,deg)
cor(df)

# Between a dummy and a continuous
cor.test(V(students)$gender,V(students)$age) # between a dummy variable and a continuous
# Totally nonsensical. According to the Pearson's product-moment correlation, the correlation between gender and age is 0.25, however because of the p-value, which is 0.359, the correlation generated does not 

# Between a categorical variables (multiple categories) and continuous
kruskal.test(age,V(students)$country)
# As the p-value is above any significant alpha levels, we can conclude that there are no statically significant difference between different countries and age.

# A finer multiple pairwise comparison between groups
pairwise.wilcox.test(age,V(students)$country,p.adjust.method = "BH")
# all of these values are 1, which means that the p-value for all of these different pairs are 1. This means that no statistically significant difference could be observed between age and the country that they are from. 

table(V(students)$country)

# For analysis of bipartite network as a whole, please have a look here: https://rpubs.com/pjmurphy/317838 
# Otherwise, if you just use each of the projections, then things we have done previously would easily apply.

# tkplot(students)
