library(igraph)
nodes <- c("Dog", "Cat", "Couch", "Sofa", "Animal")
edges <- data.frame(
  from = c("Dog", "Couch", "Animal", "Animal", "Couch"),
  to = c("Cat", "Sofa", "Dog", "Cat", "Cat"),
  type = c("semantic", "semantic", "semantic", "semantic", "phonologic"),
  weight = c(3, 3, 2, 2, 1)
)

edge_colors <- c(
  semantic = "blue",
  phonologic = "red"
)

g <- graph_from_data_frame(d = edges, directed = FALSE, vertices = nodes)
plot(g, 
     layout = layout.circle, 
     vertex.label.cex = 1.5, 
     edge.arrow.size = 0.5,
     edge.color = edge_colors[E(g)$type],
     edge.width = 2*E(g)$weight
)
    



num_nodes <- vcount(g)
num_edges <- ecount(g)
avg_degree <- mean(degree(g))
central_nodes <- V(g)$name[which.max(betweenness(g))]

betweenness(g) 
#betweenessis # of words that word is on the shortest path length for a diff word. 
#how many times you have to use that word as a bridge basically


cat("Number of Individuals:", num_nodes, "\n")
E(g)$type
E(g)$weight 

betweenness(g, E(g)$weight) 
betweenness(g) 

library(tidyr)
library(dplyr)


mat <- mcrae_df %>%
  mutate(value = 1) %>%                      # use production freq instead of 1 if desired
  pivot_wider(names_from = feature,
              values_from = value,
              values_fill = list(value = 0)) %>%
  as.data.frame()

