#Creating and plotting networks, calculating network metric
library(igraph)
#Plot social networks for each season using igraph
#Season 1
s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)
s1G <- graph_from_data_frame(d = s1Edges, directed = TRUE)

V(s1G)$color <- sapply(V(s1G)$name, function(name) {
  row <- which(elo$ID == name)
  if(elo$Sex[row] == "F") {
    "white"
  } else {
    "gray"
  }
})

s1p <- plot.igraph(s1G, 
                   vertex.shape = "circle",
                   vertex.size = 15,
                   vertex.color = V(s1G)$color,
                   #vertex.label = "",
                   edge.curved = .75)

#Season 2
s2Edges <- data.frame(from = s2$"Actor", to = s2$"Recip", weight = s2$groom.rate)
s2G <- graph_from_data_frame(d = s2Edges, directed = TRUE)

V(s2G)$color <- sapply(V(s2G)$name, function(name) {
  row <- which(elo$ID == name)
  if(elo$Sex[row] == "F") {
    "white"
  } else {
    "gray"
  }
})

s2p <- plot.igraph(s2G, 
                   vertex.shape = "circle",
                   vertex.size = 15,
                   vertex.color = V(s1G)$color,
                   #vertex.label = "",
                   edge.curved = .75)

#Season 3
s3Edges <- data.frame(from = s3$"Actor", to = s3$"Recip", weight = s3$groom.rate)
s3G <- graph_from_data_frame(d = s3Edges, directed = TRUE)

V(s3G)$color <- sapply(V(s3G)$name, function(name) {
  row <- which(elo$ID == name)
  if(elo$Sex[row] == "F") {
    "white"
  } else {
    "gray"
  }
})

s3p <- plot.igraph(s3G, 
                   vertex.shape = "circle",
                   vertex.size = 15,
                   vertex.color = V(s1G)$color,
                   #vertex.label = "",
                   edge.curved = .75)

#Calculating 7 per season individual network metrics
#In-degree (number of partners indiv was groomed by), out-degree (number of partners
#indiv groomed), in-strength (indivs rate of being groomed), out-strength (indiv
#rate of grooming other) betweenness, eigenvector centrality, local clustering coefficient

#In-degree & Out-degree
s1IO <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::degree(s1G, mode = "in"), Out = igraph::degree(s1G, mode = "out"), Season = 1, rank = elo$Elo.May, row.names = NULL)
s2IO <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::degree(s2G, mode = "in"), Out = igraph::degree(s2G, mode = "out"), Season = 2, rank = elo$Elo.Sep, row.names = NULL)
s3IO <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::degree(s3G, mode = "in"), Out = igraph::degree(s3G, mode = "out"), Season = 3, rank = elo$Elo.Dec, row.names = NULL)
IO <- bind_rows(list(s1IO, s2IO, s3IO))

#In-strength & Out-Strength
s1S <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::strength(s1G, mode = "in"), Out = igraph::strength(s1G, mode = "out"), Season = 1, rank = elo$Elo.May, row.names = NULL)
s2S <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::strength(s2G, mode = "in"), Out = igraph::strength(s2G, mode = "out"), Season = 2, rank = elo$Elo.Sep, row.names = NULL)
s3S <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::strength(s3G, mode = "in"), Out = igraph::strength(s3G, mode = "out"), Season = 3, rank = elo$Elo.Dec, row.names = NULL)
strength <- bind_rows(list(s1S, s2S, s3S))

#Betweenness
s1B <- data.frame(ID = elo$ID, Sex = elo$Sex, bt = betweenness.estimate(s1G, cutoff = -1), Season = 1, rank = elo$Elo.May, row.names = NULL)
s2B <- data.frame(ID = elo$ID, Sex = elo$Sex, bt = betweenness.estimate(s2G, cutoff = -1), Season = 2, rank = elo$Elo.Sep, row.names = NULL)
s3B <- data.frame(ID = elo$ID, Sex = elo$Sex, bt = betweenness.estimate(s3G, cutoff = -1), Season = 3, rank = elo$Elo.Dec, row.names = NULL)
b <- bind_rows(list(s1B, s2B, s3B))

#Eigenvector centrality
s1E <- data.frame(ID = elo$ID, Sex = elo$Sex, EC = eigen_centrality(s1G), Season = 1, rank = elo$Elo.May, row.names = NULL)
s2E <- data.frame(ID = elo$ID, Sex = elo$Sex, EC = eigen_centrality(s2G), Season = 2, rank = elo$Elo.Sep, row.names = NULL)
s3E <- data.frame(ID = elo$ID, Sex = elo$Sex, EC = eigen_centrality(s3G), Season = 3, rank = elo$Elo.Dec, row.names = NULL)
EC <- bind_rows(list(s1E, s2E, s3E))

#Clustering coefficient
s1CC <- data.frame(ID = elo$ID, Sex = elo$Sex, CC = igraph::transitivity(s1G, type = "local"), Season = 1, rank = elo$Elo.May, row.names = NULL)
s2CC <- data.frame(ID = elo$ID, Sex = elo$Sex, CC = igraph::transitivity(s2G, type = "local"), Season = 2, rank = elo$Elo.Sep, row.names = NULL)
s3CC <- data.frame(ID = elo$ID, Sex = elo$Sex, CC = igraph::transitivity(s3G, type = "local"), Season = 3, rank = elo$Elo.Dec, row.names = NULL)
CC <- bind_rows(list(s1CC, s2CC, s3CC))


#Plotting metrics over time
InDegreePlot <- ggplot(data = IO, mapping = aes(x = Season, y = In, color = ID)) +
  geom_line() +
  theme(legend.position = "none") +
  ggtitle("In-Degree")

OutDegreePlot <- ggplot(data = IO, mapping = aes(x = Season, y = Out, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("Out-Degree")

InStrengthPlot <- ggplot(data = strength, mapping =aes(x= Season, y= In, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("In-Strength")

OutStrengthPlot <- ggplot(data = strength, mapping = aes(x = Season, y = Out, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("Out-Strength")

betweenPlot <- ggplot(data = b, mapping = aes(x = Season, y = bt, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("Betweenness")

eigenPlot <- ggplot(data = EC, mapping = aes(x = Season, y = EC.vector, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("Eigenvector Centrality")

clusterPlot <- ggplot(data = CC, mapping = aes(x = Season, y = CC, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("Clustering Coefficient")

cowplot <- plot_grid(InDegreePlot, OutDegreePlot, InStrengthPlot, OutStrengthPlot, betweenPlot, eigenPlot, clusterPlot, nrow = 3, ncol = 3)
cowplot

#Spearman Corrleation
network_metrics <- data.frame(In.Degree = IO$In, Out.Degree = IO$Out, In.Strength = strength$In,
                              Out.Strength = strength$Out, Eigenvector = EC$EC.vector, 
                              Cluster = CC$CC, Between = b$bt)
spearCor <- cor(network_metrics, method = "spearman")
spearCor