#Creating and plotting networks, calculating network metric
library(igraph)
library(ggplot2)
library(cowplot)
#Plot social networks for each season using igraph
#Season 1
s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)
s1G <- graph_from_data_frame(d = s1Edges, directed = TRUE)
nsize1 <- 5 + 15*(degree(s1G)/max(degree(s1G)))

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
                   vertex.size = nsize1,
                   vertex.color = V(s1G)$color,
                   vertex.label = "",
                   edge.arrow.size = 0.3,
                   edge.curved = .5,
                   main = c("Season 1",
                            "March - May",
                            "FOH = 497", " ND = 0.45"))

#Season 2
s2Edges <- data.frame(from = s2$"Actor", to = s2$"Recip", weight = s2$groom.rate)
s2G <- graph_from_data_frame(d = s2Edges, directed = TRUE)
nsize2 <- 5 + 15*(degree(s2G)/max(degree(s2G)))

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
                   vertex.size = nsize2,
                   vertex.color = V(s1G)$color,
                   vertex.label = "",
                   edge.curved = .75,
                   edge.arrow.size = 0.3,
                   main = c("Season 2",
                            "June - September",
                            "FOH = 967.1", " ND = 0.71"))
#Season 3
s3Edges <- data.frame(from = s3$"Actor", to = s3$"Recip", weight = s3$groom.rate)
s3G <- graph_from_data_frame(d = s3Edges, directed = TRUE)
nsize3 <- 5 + 15*(degree(s3G)/max(degree(s3G)))

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
                   vertex.size = nsize3,
                   vertex.color = V(s1G)$color,
                   vertex.label = "",
                   edge.curved = .75,
                   edge.arrow.size = 0.3,
                   main = c("Season 3",
                            "October - December",
                            "FOH = 705.2", " ND = 0.52"))

#Calculating 7 per season individual network metrics
#In-degree (number of partners indiv was groomed by), out-degree (number of partners
#indiv groomed), in-strength (indivs rate of being groomed), out-strength (indiv
#rate of grooming other) betweenness, eigenvector centrality, local clustering coefficient

#Calculating metrics for season 1
s1Metrics <- data.frame(ID = elo$ID, Sex = elo$Sex, Season = 1, rank = elo$Elo.May, In.Degree = igraph::degree(s1G, mode = "in"), Out.Degree = igraph::degree(s1G, mode = "out"), In.Strength = igraph::strength(s1G, mode = "in"), Out.Strength = igraph::strength(s1G, mode = "out"), b = igraph::betweenness(s1G, cutoff = -1), EC = eigen_centrality(s1G), CC = igraph::transitivity(s1G, type = "local"), row.names = NULL)
s1Metrics <- s1Metrics |>
  select(ID, Sex, Season, rank, In.Degree, Out.Degree, In.Strength, Out.Strength, b, EC.vector, CC)
head(s1Metrics)
#Calculating metrics for season 2
s2Metrics <- data.frame(ID = elo$ID, Sex = elo$Sex, Season = 2, rank = elo$Elo.Sep, In.Degree = igraph::degree(s2G, mode = "in"), Out.Degree = igraph::degree(s2G, mode = "out"), In.Strength = igraph::strength(s2G, mode = "in"), Out.Strength = igraph::strength(s2G, mode = "out"), b = igraph::betweenness(s2G, cutoff = -1), EC = eigen_centrality(s2G), CC = igraph::transitivity(s2G, type = "local"), row.names = NULL)
s2Metrics <- s2Metrics |>
  select(ID, Sex, Season, rank, In.Degree, Out.Degree, In.Strength, Out.Strength, b, EC.vector, CC)
head(s2Metrics)
#Calculating metrics for season 3
s3Metrics <- data.frame(ID = elo$ID, Sex = elo$Sex, Season = 3, rank = elo$Elo.Dec, In.Degree = igraph::degree(s3G, mode = "in"), Out.Degree = igraph::degree(s3G, mode = "out"), In.Strength = igraph::strength(s3G, mode = "in"), Out.Strength = igraph::strength(s3G, mode = "out"), b = igraph::betweenness(s3G, cutoff = -1), EC = eigen_centrality(s3G), CC = igraph::transitivity(s3G, type = "local"), row.names = NULL)
s3Metrics <- s3Metrics |>
  select(ID, Sex, Season, rank, In.Degree, Out.Degree, In.Strength, Out.Strength, b, EC.vector, CC)
head(s3Metrics)
#Combining metrics into one data.frame
allMetrics <- bind_rows(list(s1Metrics, s2Metrics, s3Metrics))
allMetrics <- allMetrics |>
  mutate(Season = as.factor(Season))
head(allMetrics)
#Creating data for Females and Males
Females <- allMetrics |>
  filter(Sex == "F")
Males <- allMetrics |>
  filter(Sex == "M")

IndivDescrip <- allMetrics |>
  group_by(ID) |>
  summarize(In.Degree.Mean = mean(In.Degree), In.Degree.SD = sd(In.Degree), Out.Degree.Mean = mean(Out.Degree), Out.Degree.SD = sd(In.Degree), In.Strength.Mean = mean(In.Strength), In.Strength.SD = sd(In.Strength), Out.Degree.Mean = mean(Out.Degree), Out.Degree.SD = sd(Out.Degree), Eigenvector.Mean = mean(EC.vector), Eigenvector.SD = sd(EC.vector), Clustering.Coefficient.Mean = mean(CC), Custering.Coefficient.SD = sd(CC), Betweennss.Mean = mean(b), Betweenness.SD = sd(b))
IndivDescrip

SeasonDescrip <- allMetrics |>
  group_by(Sex, Season) |>
  summarize(In.Degree.Mean = mean(In.Degree), In.Degree.SD = sd(In.Degree), Out.Degree.Mean = mean(Out.Degree), Out.Degree.SD = sd(In.Degree), In.Strength.Mean = mean(In.Strength), In.Strength.SD = sd(In.Strength), Out.Degree.Mean = mean(Out.Degree), Out.Degree.SD = sd(Out.Degree), Eigenvector.Mean = mean(EC.vector), Eigenvector.SD = sd(EC.vector), Clustering.Coefficient.Mean = mean(CC), Custering.Coefficient.SD = sd(CC), Betweennss.Mean = mean(b), Betweenness.SD = sd(b))
SeasonDescrip

#Plotting metrics over time
InDegreePlot <- ggplot(data = allMetrics, mapping = aes(x = as.numeric(Season), y = In.Degree, color = ID)) +
  geom_line() +
  theme(legend.position = "none") +
  ggtitle("In-Degree") +
  xlab(label = "Season") +
  ylab(label = "In-Degree")

OutDegreePlot <- ggplot(data = allMetrics, mapping = aes(x = as.numeric(Season), y = Out.Degree, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("Out-Degree") +
  xlab(label = "Season") +
  ylab(label = "Out-Degree")

InStrengthPlot <- ggplot(data = allMetrics, mapping = aes(x= as.numeric(Season), y= In.Strength, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("In-Strength") +
  xlab(label = "Season") +
  ylab(label = "In-Strength")

OutStrengthPlot <- ggplot(data = allMetrics, mapping = aes(x = as.numeric(Season), y = Out.Strength, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("Out-Strength") +
  xlab(label = "Season") +
  ylab(label = "Out-Strength")

betweenPlot <- ggplot(data = allMetrics, mapping = aes(x = as.numeric(Season), y = b, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("Betweenness") +
  xlab(label = "Season")

eigenPlot <- ggplot(data = allMetrics, mapping = aes(x = as.numeric(Season), y = EC.vector, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("Eigenvector Centrality") +
  xlab(label = "Season")

clusterPlot <- ggplot(data = allMetrics, mapping = aes(x = as.numeric(Season), y = CC, color = ID)) +
  geom_line()+
  theme(legend.position = "none") +
  ggtitle("Clustering Coefficient") +
  xlab(label = "Season")

cowplot <- plot_grid(InDegreePlot, OutDegreePlot, InStrengthPlot, OutStrengthPlot, eigenPlot, clusterPlot, betweenPlot, nrow = 2, ncol = 4)
cowplot


inD <- ggplot(data = allMetrics, mapping = aes(x = Season, y = In.Degree, fill = Sex)) +
  geom_boxplot() +
  ylab(label = "In-Degree") +
  ggtitle("In-Degree")
outD <- ggplot(data = allMetrics, mapping = aes(x = Season, y = Out.Degree, fill = Sex)) +
  geom_boxplot() +
  ylab(label = "Out-Degree") +
  ggtitle("Out-Degree")
inS <- ggplot(data = allMetrics, mapping = aes(x = Season, y = In.Strength, fill = Sex)) +
  geom_boxplot() +
  ylab(label = "In-Strength") +
  ggtitle("In-Strength")
outS <- ggplot(data = allMetrics, mapping = aes(x = Season, y = Out.Strength, fill = Sex)) +
  geom_boxplot() +
  ylab(label = "Out-Degree") +
  ggtitle("Out-Degree")
eigen <- ggplot(data = allMetrics, mapping = aes(x = Season, y = EC.vector, fill = Sex)) +
  geom_boxplot() +
  ylab(label = "Eigenvector Score") +
  ggtitle("Eigenector Centrality")
clust <- ggplot(data = allMetrics, mapping = aes(x = Season, y = CC, fill = Sex)) +
  geom_boxplot() +
  ylab(label = "Clustering Coefficient") +
  ggtitle("Clustering Coefficient")
between <- ggplot(data = allMetrics, mapping = aes(x = Season, y = b, fill = Sex)) +
  geom_boxplot() +
  ylab(label = "Betweenness") +
  ggtitle("Betweenness")

cowplot1 <- plot_grid(inD, outD, inS, outS, eigen, clust, between)
cowplot1