# RE-EXAM ASSIGNMENT - SOCIAL NETWORK ANALYSIS MODULE: AIRLINES, IRON MAN MOVIE AND   
# Luis Felipe Villota Macías 


# Exploring social networks and analyzing their properties  


# Loading packages

library(tidyverse)	
library(psych)	
library(gridExtra)
library(readr)
library(skimr)
library(network)
library(plyr)
library(igraph)
library(intergraph)
library(sna)
library(RColorBrewer)
library(vcd)
library(ggplot2)
library(networkR)



# Setting working directory

getwd()
setwd("C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMP56 - Using Social Theory/Scripts/Kadri")

dir <- "C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMP56 - Using Social Theory/Scripts/Kadri"



### PART A: 

## AIRLINES (routes between cities)---------------------------------------------


# Loading the data: Airlines database(s)

zip.files <- list.files(dir, pattern = "*.zip",
                        full.names = TRUE)
zip.files

ldply(.data = "C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMP56 - Using Social Theory/Scripts/Kadri/airlines.zip", .fun = unzip, exdir= dir)

edges = read.csv("C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMP56 - Using Social Theory/Scripts/Kadri/airlines_edges_2.csv")

nodes = read.csv("C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMP56 - Using Social Theory/Scripts/Kadri/airlines_nodes_2.csv")



# Node variables:

# "Label" - name of airport
# "Code" - code of airport
# "City" - city of the airport
# "latitude, longitude" - location of airport 
# "ToFly" - recommended destination (1 - yes; 0 - no)
# "Visits" - number of flights per day. 

# As instructed, we ignore the edge variable "Weights"


# Basic descriptive and exploratory data analysis of raw data

view(nodes)
view(edges)

nodes %>% summary()

edges %>% summary()

describe(nodes)
describe(edges)

skim(nodes) # 235 observations 8 variables
skim(edges) # 1297 observations 3 variables 


# Nodes
length(unique(nodes$Label)) # 217 unique Labels
unique(nodes$Label)

length(unique(nodes$Id)) # 235 unique Id's = number of observations
unique(nodes$Id)


length(unique(nodes$City)) # 232 unique cities
unique(nodes$City)

# Edges
length(unique(edges$Source)) # 210 unique sources
unique(edges$Source)

length(unique(edges$Target)) # 193 unique targets
unique(edges$Target)


# Checking for missing values

any(is.na(edges)) # FALSE
any(is.na(nodes)) # FALSE
sum(is.na(edges)) # 0 cases in total
sum(is.na(nodes)) # 0 cases in total
colSums(is.na(edges)) # No missing values 
colSums(is.na(nodes)) # No missing values 


# We can proceed now onto dataset management


edges_1 <- data.frame(edges$Source,edges$Target) # data frame without Weights (for "edges")
        

# Creating a "igraph" object from the two datasets: "edges_1" & "nodes",
# specifying a directed network. 

net_routes = graph_from_data_frame(d=edges_1, vertices = nodes, directed=TRUE)

class(net_routes)
net_routes

E(net_routes) # 1297 edges

V(net_routes) # 235 vertices

V(net_routes)$City

print(net_routes)


V(net_routes)$label <- V(net_routes)$Code
V(net_routes)$degree <- igraph::degree(net_routes)

V(net_routes)$label
V(net_routes)$degree 



# Histogram of node degree

hist(V(net_routes)$degree,
     col= "gold",
     main= "Node Degree Histogram",     # The majority of the nodes have between 0-20 degrees, 
     ylab= "Frequency",                 # in other words, we have many airports with few connections.
     xlab= "Degree of Vertices")


# or

igraph::degree(net_routes, mode = "all")
igraph::degree(net_routes, mode = "in")
igraph::degree(net_routes, mode = "out")


hist(igraph::degree(net_routes, mode = "all"))




# Plotting the network of routes(flights) made between airports


set.seed(369)

plot(net_routes, 
     edge.arrow.size=.07, 
     vertex.color="gold", 
     vertex.size=3,
     vertex.frame.color="white",
     vertex.label.color="black", 
     vertex.label.cex=0.8,
     vertex.label.dist=.5, 
     edge.curved=0.2) 

# We can highlight the nodes by their degrees on the plot


set.seed(333)

plot(net_routes, 
     edge.arrow.size=.06, 
     vertex.color= rainbow(123), 
     vertex.size= V(net_routes)$degree*0.2,
     vertex.frame.color="grey",
     vertex.label.color="black", 
     vertex.label.cex=0.6,
     vertex.label.dist=.9, 
     edge.curved=0.2, 
     layout= layout.kamada.kawai)  


# Hub and authorities scores 

net_hub <- igraph::hub_score(net_routes)$vector

net_auth <- igraph::authority_score(net_routes)$vector

net_hub
net_auth

par(mfrow =c(1,2))

plot(net_routes,
     main= "Hubs",
edge.arrow.size=.06, 
vertex.color= rainbow(123), 
vertex.size= net_hub*10,
vertex.frame.color="grey",
vertex.label.color="black", 
vertex.label.cex=0.6,
vertex.label.dist=.9, 
edge.curved=0.2, 
layout= layout.kamada.kawai)

plot(net_routes,
     main= "Authorities",
     edge.arrow.size=.06, 
     vertex.color= rainbow(123), 
     vertex.size= net_auth*10,
     vertex.frame.color="grey",
     vertex.label.color="black", 
     vertex.label.cex=0.6,
     vertex.label.dist=.9, 
     edge.curved=0.2, 
     layout= layout.kamada.kawai)

par(mfrow =c(1,1))


# Communities

net_comm = graph.data.frame(d=edges_1, vertices = nodes, directed=F)

net_comm_1 = cluster_edge_betweenness(net_comm)

plot(net_comm_1,
     net_comm,
     vertex.size= 10, 
     vertex.label.cex=0.6)



# Adjacency matrix

as_adj( net_routes,
        type = c("both"),
        attr = NULL,
        edges = TRUE,
        names = TRUE,
        sparse = TRUE)

summary(net_routes)


as_adjacency_matrix(net_routes, attr=NULL)

# Edge list

head(edges_1,50)

as_edgelist(net_routes, names=T)






# Questions (7 in total):

        # 1. Is the network directed or undirected?

is_directed(net_routes) # TRUE , the network is directed

        # 2. How many airport and flights routes there are?


length(unique(nodes$Code)) # 235 unique airports by "Code". Note: The variable "Label" is misguiding 
unique(nodes$Code)         # for looking at this uniqueness because some names are repeated but have different "Code".

dim(nodes[duplicated(nodes$Label),])[1] # In fact we have 18 duplicates in "Label"
nodes[duplicated(nodes$Label),] # Duplicates are : "International", "Municipal" and "Regional"


length(unique(edges_1$edges.Source)) # 210 unique "Sources"
unique(edges_1$edges.Source)

length(unique(edges_1$edges.Target)) # 193 unique "Targets"
unique(edges_1$edges.Target)
                                     # We have more variety of places of departure, than actual destinations


nrow(edges_1) # 1297 total number of edges

nrow(unique(edges_1[,c("edges.Source", "edges.Target")])) # 1297 unique flights

# The total number of edges are unique source-target combinations. 
# This means that there are no observations in our data that have more than one connection 
# between the same two nodes.



        # 3. What is the density of the network?


        # Density for a directed network: N*(N-1)

ecount(net_routes)/((vcount(net_routes) * (vcount(net_routes) - 1))) # 0.0235
                                                                     
# or

gorder(net_routes) # 235
gsize(net_routes) # 1297 

1297/(235*235-1) # 0.0235

edge_density(net_routes, loops = F) # 0.02358611


# or 

igraph::edge_density(net_routes) # 0.0235



        # 4. What is the average degree of the network?


# Total edges / total nodes

1297/235  # 5.519149

# or (N-1)*density

(vcount(net_routes) - 1) * 0.02358611 # 5.51915



        # 5. What is the average shortest path of the network?

mean_distance(net_routes, directed=T) # The mean of the shortest path between 
                                      #  each pair of vertices = 3.006053 

distances(net_routes, weights = NA)


# Some other properties of the network

igraph::diameter(net_routes, directed = T) # diameter = 8


igraph::reciprocity(net_routes) # reciprocity = 0 , there are no mutual connections 


igraph::triad_census(net_routes)# transitivity


        # 6. Which are the 5 most central airports in this network? Use 3 different centrality measures
        #    and present the top airports in a nice table. Describe and interpret the results.



# New objects for centrality measures

Degree_net = as.data.frame(igraph::degree(net_routes)) # degree centrality

Eigen_net <- as.data.frame(igraph::evcent(net_routes)$vector)# EigenVector centrality 

Close_net <- as.data.frame(igraph::closeness(net_routes, mode = "all")) # closeness centrality 

Between_net <- as.data.frame(igraph::betweenness(net_routes, directed = T)) # Betweenness centrality 

Hub_net <- as.data.frame(igraph::hub.score(net_routes)$vector)  # Hubs

Authority_net <- as.data.frame(igraph::authority.score(net_routes)$vector) # Authorities


# New data frame made of centrality measures

Centrality_net <- cbind(Degree_net, Eigen_net, Hub_net, Authority_net, Close_net, Between_net)

# We add new columns to have more info about the airports

Id <- as.data.frame(nodes$Id)
Label <- as.data.frame(nodes$Label)
City <- as.data.frame(nodes$City) 
Code <- as.data.frame(nodes$Code)

Centrality_net$Label <- Label
Centrality_net$Id <- Id
Centrality_net$City <- City
Centrality_net$Code <- Code

# We change the names  and order of the variables in the "Centrality_net" frame

colnames(Centrality_net)
names(Centrality_net)[1] <- "Degree"
names(Centrality_net)[2] <- "Eigenvector"
names(Centrality_net)[3] <- "Hub"
names(Centrality_net)[4] <- "Authority"
names(Centrality_net)[5] <- "Closenness"
names(Centrality_net)[6] <- "Betweenness"

# We reorder the columns

col_order <- c("Id", "Label", "City",
               "Code", "Degree", "Eigenvector", "Closenness", "Betweenness",
               "Hub", "Authority")

Centrality_net <- Centrality_net[, col_order]
Centrality_net



# We sort by (descending) "Degree" to find the top 5 most central airports

Cen_net <- Centrality_net %>% arrange(desc(Degree))

top_air<- head(Cen_net, 5)


library(gridExtra)
library(grid)
library(gt)

dev.off()

grid.table(top_air)


# Other characteristics of the network

names(igraph::betweenness(net_routes))[which(igraph::betweenness(net_routes) == max(igraph::betweenness(net_routes)))] # most central node

centralization.degree(net_routes)$centralization # 0.255278 centralization degree

# St. Paul Intl.
 
igraph::degree(net_routes, 137, mode="out")

igraph::degree(net_routes, 137, mode="in")

igraph::degree(net_routes, 137, mode="all")




        # 7. Are there different communities in the network (use Louvain algorithm)? Describe, interpret
        #    and visualise the results.

is_connected(net_routes) # TRUE, the network is connected, all nodes are connected somehow
component_distribution(net_routes)


net_comm = graph.data.frame(d=edges_1, vertices = nodes, directed=F) # turning into an undirected network




# Louvain community detection algorithm

lou_net_routes <- cluster_louvain(net_comm, weights=NULL)
class(comm)

lou_net_routes$membership 

modularity(lou_net_routes) # 0.2422615

length(lou_net_routes) # 9 communities 

sizes(lou_net_routes) # Biggest community has 42 nodes

is_hierarchical(lou_net_routes) # FALSE not hierarchical 



# and

net_comm_1 = cluster_edge_betweenness(net_comm) # community detection

membership(net_comm_1) # list of the nodes associated with the identified clusters


net_comm_2 <- cut_at(net_comm_1, no = 9) # Nodes into 9 identified clusters 
net_comm_2


# Dendrogram


dendPlot(net_comm_1, mode="hclust", rect = 9) # showing all 9 communities

# Plot for communities 

set.seed(456)                  # by cluster edge betweenness
plot(net_comm_1,
     net_comm,
     vertex.size= 10, 
     vertex.label.cex=0.6)

set.seed(333)                 # by Louvain's cluster
plot(lou_net_routes,
     net_comm,
     vertex.size= 10, 
     vertex.label.cex=0.6,
             layout= layout.auto)







## IRON MAN MOVIE (interactions of characters)----------------------------------

install.packages("devtools")
devtools::install_github("pj398/movienetdata")


library(movienetdata)
library(network)

# Loading the dataset

data(package = "movienetdata")
force(mcu01_ironman)
iron <- movienetdata::mcu01_ironman

iron

iron$event_list
iron$node_list
iron$adjacency

# Node list

iron_nodes <- as.data.frame(iron$node_list) #


# Edge list

iron_edgelist <- as.matrix(iron_network, matrix.type="edgelist")

iron_edges <- as.data.frame(iron_edgelist) #



# "Network" object 

iron_network <- network(iron$adjacency)  #
class(iron_network)
iron_network


# "Igraph" object

iron_igraph = graph_from_data_frame(d=iron_edges, vertices = iron_nodes) #
class(iron_igraph)
iron_igraph

# or


iron_g <- asIgraph(iron_network) #
class(iron_g)
iron_g


# Plot of adjacency matrix, as done by Matthew Lincoln (2014) in: https://matthewlincoln.net/2014/12/20/adjacency-matrix-plots-with-r-and-ggplot2.html


ggplot(iron_edges, aes(x = V1, y = V2)) +
        geom_raster() +
        theme_bw() +
        scale_x_discrete(drop = FALSE) +
        scale_y_discrete(drop = FALSE) +
        theme(
                axis.text.x = element_text(angle = 270, hjust = 0),
                aspect.ratio = 0.9,
                legend.position= "none")



#         8. Is the network directed or undirected?


summary(iron_igraph)
summary(iron_g)
summary(iron_network) # Directed

is.directed(iron_g) 
is.directed(iron_igraph) # TRUE, the network is directed


#         9. How many actors and ties there are?

network.edgecount(iron_network)  # 54 connections 

network.size(iron_network) # 18 vertices/characters
network.vertex.names(iron_network) # Characters' names 


#         10. What is the density of the network?

network.density(iron_network) # 0.1764706    

edge_density(iron_igraph, loops = FALSE) # 0.1764706


#         11. What is the average degree of the network?

# Total edges / total nodes

54/18  # 3

# or (N-1)*density

(vcount(iron_g) - 1) * 0.1764706 # 3



#         12. What is the average shortest path of the network?

mean_distance(iron_igraph, directed=T) # The mean of the shortest path between 
#  each pair of vertices = 1.98366 

distances(iron_g, weights = NA)

#         13. Who are the 3 most important characters in this movie in terms of speaking themselves?

top3_iron_speakers<- slice_max(iron_nodes, order_by = iron_nodes$nlines, n= 3)
dev.off()
grid.table(top3_iron_speakers) # by total lines spoken


#         14. Who does James Rhodes interact with in the movie? Are there any characters who he speaks to, but they do not speak back, or vice versa?

# As a "Network" object

iron_nodes # the ID for the character is 3
get.neighborhood(iron_network, 3, "out") # James Rhodes speaks to Tony Stark (ID: 1), Major Allen (ID: 15) and Pepper Potts (ID: 8)
get.neighborhood(iron_network, 3, "in") # All the above characters speak back to him, plus Obadiah Stane (ID: 4)
                                        # In conclusion, James does not speak to Obadiah
        
       

#         15. Visualise the network so that node size depends on some centrality measure and node colour corresponds to the sex of the character.


# Inspired on Pete Jones (2022) in https://www.petejon.es/posts/2020-03-03-movienetdata/
        

dev.off()



V(iron_igraph)$label <- V(iron_igraph)$char_name
V(iron_igraph)$degree <- igraph::degree(iron_igraph)

V(iron_igraph)$label
V(iron_igraph)$degree 


set.seed(578)

plot(iron_igraph, label= iron_igraph$char_name,
     edge.col = "blue", 
     edge.arrow.size=.06, 
     vertex.color= ifelse(V(iron_igraph)$char_female==1, # Colored by sex
                          "yellow", "red"),
     vertex.size= V(iron_igraph)$degree*0.8,    # By degree centrality 
     vertex.frame.color="grey",
     vertex.label.color="black", 
     vertex.label.cex=0.6,
     vertex.label.dist=.9, 
     edge.curved=0.2, 
     layout= layout.kamada.kawai, 
     main = "Sociogram of Iron Man (2008)") 



#         16. Are there different communities in the network (use Louvain algorithm)? 
#            Describe, interpret and visualise the results.


is_connected(iron_g) # TRUE, the network is connected, all nodes are connected somehow
component_distribution(iron_g)

iron_comm = graph.data.frame(d=iron_edges, vertices = iron_nodes, directed=F)  # turning into an undirected network


# Louvain community detection algorithm

class(iron_comm)

iron_lou <- cluster_louvain(iron_comm, weights = NULL) 
communities(iron_lou) 


iron_lou$membership 

modularity(iron_lou) # 0.2340535

length(iron_lou) # 3 communities 

sizes(iron_lou) # Biggest community has 8 nodes

is_hierarchical(iron_lou) # FALSE not hierarchical 




# Plot for communities 

plot(iron_lou,
     iron_comm,
     vertex.size= 10, 
     vertex.label.cex=0.6)



#         17. Perform clustering based on edge betweenness for an undirected network (set seed = 2). 
#            How closely do the clustering results match with the community algorithm results? 
#            Tabulate the two memberships and calculate the association between the 
#            two memberships (??-squared)). Describe, interpret and visualise the results.


# Dendrogram

iron_comm_1 = cluster_edge_betweenness(iron_comm) # cluster edge betweenness

set.seed(2)
dendPlot(iron_comm_1, mode="hclust") # showing 4 clusters

cluster_edge_betweenness(iron_comm)
communities(iron_lou) 

# Plot of cluster by the edge betweenness

set.seed(2)
plot(iron_comm_1,
     iron_comm,
     vertex.size= 10, 
     vertex.label.cex=0.6)




# Storing the community membership by node


iron_lou$membership
iron_lou.mem

iron_comm_1$membership

iron_lou.mem <- iron_lou$membership # community membership
iron_clu <- iron_comm_1$membership  # node properties

iron_t<- table(iron_lou.mem, iron_clu) # Table

grid.table(iron_t)



chisq.test(iron_lou.mem, iron_clu)



iron_attr <- vertex.attributes(iron_g)
iron_labels <- iron_g$vertex.names



colors <- brewer.pal(length(iron_lou), 'Set3')


V(iron_g)$color <- colors[membership(iron_lou)] # color by community membership

set.seed(2)

plot(iron_g, vertex.label=iron_clu,
     edge.arrow.size=.07, 
     vertex.color= V(iron_g)$color, 
     vertex.size=15,
     vertex.frame.color="white",
     vertex.label.color="black", 
     vertex.label.cex=0.8,
     vertex.label.dist=.5, 
     edge.curved=0.2)
     
   




#         18. Briefly compare the two networks to other real-life networks 
#             (use the examples from the last slide in Lecture 2). 
#             Include a table with the main descriptions of networks that you compare 
#            and interpret the results.

# Airlines & Iron Man movie (2008), GLOBAL MEASURES

# Density ok
# Shortest path ok
# Average path length ok
# Reciprocity
igraph::reciprocity(net_routes)
igraph::reciprocity(iron_igraph)
# Communities ok
# Most central node ok



### PART B (own network with 16 actors)-----------------------------------------


#       19. Compare your network to a random network and small world network of the same size (also set seed). 
#         Provide a brief description of network, including a table with the main descriptives and figures of 
#         degree distribution for all 3 networks.


# Own network

own_igraph <- (
        graph( c(
                "Larsson", "Claes",
                "Anya", "Nicole",
                "Nicole", "Claes",
                "Claes", "Nicole",
                "John", "Claes",
                "Philipp", "Anya", 
                "Anya", "Philipp", 
                "Philipp", "Flor",
                "Flor", "Anya",
                "Anya", "Philipp", 
                "Nicole", "Philipp", 
                "James", "Rose", 
                "Rose", "Philipp", 
                "James", "Claes", 
                "Philipp", "Larsson", 
                "Sofia", "Larsson", 
                "Larsson", "Anya", 
                "Dan", "Anya", 
                "Craig", "Larsson", 
                "Robert", "Michael", 
                "Michael", "Rose", 
                "Rose", "Benjamin", 
                "Benjamin", "Dan", 
                "Dan", "Claes",
                "Anya", "Sofia",
                "Sofia", "Anya",
                "Bob", "Maria",
                "Maria", "Bob", 
                "Matthias", "Bob",
                "Lee", "Bob",
                "Lee", "Mathias",
                "Matthias", "Lee",
                "Matthias", "Philipp",
                "Bob", "Philipp",
                "Lee", "Philipp"
                
        ))
)

is_directed(own_igraph) # Directed 

E(own_igraph) # 35

V(own_igraph) # 20

35/(20* 19) # 0.0921

35/20  # 1.75


mean_distance(own_igraph, directed=T) # 2.378


igraph::reciprocity(own_igraph)# 0.22



# Random 

set.seed(123)
random_igraph <- erdos.renyi.game(100, 340, type = "gnm")


is_directed(random_igraph) # FALSE 

E(random_igraph) # 340

V(random_igraph) # 100

340/(100*99/2) # 0.0686
edge_density(random_igraph, loops = F) # 0.0686


340/100  # 3.4 av degree


mean_distance(random_igraph, directed=F) # 2.598


igraph::reciprocity(random_igraph)# 1



# Small world

set.seed(898)
small_igraph<- watts.strogatz.game(1, 333, 1, 0.35, loops = FALSE, multiple = FALSE)

is_directed(small_igraph) # FALSE 

E(small_igraph) # 333

V(small_igraph) # 333

333/(333*332/2) # 0.006
edge_density(small_igraph, loops = F) # 0.006


333/333  # 1 av degree


mean_distance(small_igraph, directed=F) # 10.1657


igraph::reciprocity(small_igraph)# 1



#       20. Present a very basic visualisation of all 3 networks (just to capture the basic structure of connections).


par(mfrow =c(1,3))

set.seed(193)
plot(own_igraph,                       # Own graph
     edge.arrow.size=.07, 
     vertex.color="green", 
     vertex.size=15,
     vertex.frame.color="white",
     vertex.label.color="black", 
     vertex.label.cex=0.8,
     vertex.label.dist=.5, 
     edge.curved=0.2, 
     layout= layout.kamada.kawai,
     main= "Own Network (Music class)")



set.seed(123)

plot(random_igraph,                       # Random graph
     edge.arrow.size=.07, 
     vertex.color="purple", 
     vertex.size=8,
     vertex.frame.color="white",
     vertex.label.color="black", 
     vertex.label.cex=0.8,
     vertex.label.dist=.5, 
     edge.curved=0.2, 
     layout= layout.graphopt,
     main= "Random Network")

set.seed(898)


plot(small_igraph,                       # Small world
     edge.arrow.size=.07, 
     vertex.color="yellow", 
     vertex.size=8,
     vertex.frame.color="white",
     vertex.label.color="black", 
     vertex.label.cex=0.8,
     vertex.label.dist=.5, 
     edge.curved=0.2, 
     layout= layout.circle,
     main= "Small World")




#       21. Create a list of top 5 members by 3 centrality measures for your network. 
#          In your network:

Degree_own = as.data.frame(igraph::degree(own_igraph)) # degree centrality

Eigen_own <- as.data.frame(igraph::evcent(own_igraph)$vector)# EigenVector centrality 

Close_own <- as.data.frame(igraph::closeness(own_igraph, mode = "all")) # closeness centrality 

Centrality_own <- cbind(Degree_own, Eigen_own, Close_own)


Name <- as.data.frame(own_nodes$name)

Centrality_own$Name <- Name


colnames(Centrality_net)
names(Centrality_own)[1] <- "Degree"
names(Centrality_own)[2] <- "Eigenvector"
names(Centrality_own)[3] <- "Closenness"
names(Centrality_own)[4] <- "Name"

# We reorder the columns

col_order <- c("Name", "Degree", "Eigenvector", "Closenness")

Centrality_own <- Centrality_own[, col_order]
Centrality_own



# We sort by (descending) "Degree" to find the top 5 most central airports

Cen_own <- Centrality_own %>% arrange(desc(Degree))

top_own<- head(Cen_own, 5)


dev.off()

grid.table(top_own)



#         a) Who you consider the key figure to contact for distributing information? Why?
#          Anya          

#         b) Who should get vaccinated first to avoid quick spreading of contagious disease?
#               Why?
#         Philipp

igraph::degree(own_igraph, "Philipp", mode="all")

igraph::degree(own_igraph, "Philipp", mode="in")

igraph::degree(own_igraph, "Philipp", mode="out")


#         22. Create a visualisation of your social network (use labels, colours etc).
par(mfrow =c(1,1))

dev.off()

V(own_igraph)$degree=igraph::degree(own_igraph)

set.seed(335)

plot(own_igraph, 
     edge.arrow.size=.06, 
     vertex.color= rainbow(199), 
     vertex.size= V(own_igraph)$degree*2,
     vertex.frame.color="grey",
     vertex.label.color="black", 
     vertex.label.cex=0.6,
     vertex.label.dist=.9, 
     edge.curved=0.2, 
     layout= layout.kamada.kawai)  




