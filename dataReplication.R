library(tidyverse)
library(igraph)
library(EloRating)
library(rptR)
library(lme4)
library(sna)

#Load in datasets
f <- "https://raw.githubusercontent.com/NicoJaws23/dataReplication/refs/heads/main/IDs_Sex_EloScores.csv"
elo <- read_csv(f)

f1 <- "https://raw.githubusercontent.com/NicoJaws23/dataReplication/refs/heads/main/GroomDyads_S1.csv"
s1 <- read_csv(f1)

f2 <- "https://raw.githubusercontent.com/NicoJaws23/dataReplication/refs/heads/main/GroomDyads_S2.csv"
s2 <- read_csv(f2)

f3 <- "https://raw.githubusercontent.com/NicoJaws23/dataReplication/refs/heads/main/GroomDyads_S3.csv"
s3 <- read_csv(f3)

#add season value to all data sets along with type of season specified in article, get minutes from seconds columns
s1 <- mutate(s1, Season = 1, sType = "Mating", groomMin = total.groom.secs/60, dyadObsMin = dyad.obs.secs/60)
s2 <- mutate(s2, Season = 2, sType = "Winter", groomMin = total.groom.secs/60, dyadObsMin = dyad.obs.secs/60)
s3 <- mutate(s3, Season = 3, sType = "Birth", groomMin = total.groom.secs/60, dyadObsMin = dyad.obs.secs/60)

#Combine all 3 seasons into one big table
com <- bind_rows(list(s1, s2, s3))

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

plot.igraph(s1G, 
            vertex.shape = "circle",
            vertex.size = 15,
            vertex.color = V(s1G)$color,
            vertex.label = "",
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

plot.igraph(s2G, 
            vertex.shape = "circle",
            vertex.size = 15,
            vertex.color = V(s1G)$color,
            vertex.label = "",
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

plot.igraph(s3G, 
            vertex.shape = "circle",
            vertex.size = 15,
            vertex.color = V(s1G)$color,
            vertex.label = "",
            edge.curved = .75)

#Calculating 7 per season individual network metrics
#In-degree (number of partners indiv was groomed by), out-degree (number of partners
#indiv groomed), in-strength (indivs rate of being groomed), out-strength (indiv
#rate of grooming other) betweenness, eigenvector centrality, local clustering coefficient

#In-degree & Out-degree
s1IO <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::degree(s1G, mode = "in"), Out = igraph::degree(s1G, mode = "out"), Season = 1, row.names = NULL)
s2IO <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::degree(s2G, mode = "in"), Out = igraph::degree(s2G, mode = "out"), Season = 2, row.names = NULL)
s3IO <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::degree(s3G, mode = "in"), Out = igraph::degree(s3G, mode = "out"), Season = 3, row.names = NULL)
IO <- bind_rows(list(s1IO, s2IO, s3IO))

#In-strength & Out-Strength
s1S <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::strength(s1G, mode = "in"), Out = igraph::strength(s1G, mode = "out"), Season = 1, row.names = NULL)
s2S <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::strength(s2G, mode = "in"), Out = igraph::strength(s2G, mode = "out"), Season = 1, row.names = NULL)
s3S <- data.frame(ID = elo$ID, Sex = elo$Sex, In = igraph::strength(s3G, mode = "in"), Out = igraph::strength(s3G, mode = "out"), Season = 1, row.names = NULL)
strength <- bind_rows(list(s1S, s2S, s3S))

#Betweenness
s1B <- data.frame(ID = elo$ID, Sex = elo$Sex, bt = betweenness.estimate(s1G, cutoff = -1), Season = 1, row.names = NULL)
s2B <- data.frame(ID = elo$ID, Sex = elo$Sex, bt = betweenness.estimate(s2G, cutoff = -1), Season = 2, row.names = NULL)
s3B <- data.frame(ID = elo$ID, Sex = elo$Sex, bt = betweenness.estimate(s3G, cutoff = -1), Season = 3, row.names = NULL)
b <- bind_rows(list(s1B, s2B, s3B))

#Eigenvector centrality
s1E <- data.frame(ID = elo$ID, Sex = elo$Sex, EC = eigen_centrality(s1G), Season = 1, row.names = NULL)
s2E <- data.frame(ID = elo$ID, Sex = elo$Sex, EC = eigen_centrality(s2G), Season = 2, row.names = NULL)
s3E <- data.frame(ID = elo$ID, Sex = elo$Sex, EC = eigen_centrality(s3G), Season = 3, row.names = NULL)
EC <- bind_rows(list(s1E, s2E, s3E))

#Clustering coefficient
s1CC <- data.frame(ID = elo$ID, Sex = elo$Sex, CC = igraph::transitivity(s1G, type = "local"), Season = 1, row.names = NULL)
s2CC <- data.frame(ID = elo$ID, Sex = elo$Sex, CC = igraph::transitivity(s2G, type = "local"), Season = 2, row.names = NULL)
s3CC <- data.frame(ID = elo$ID, Sex = elo$Sex, CC = igraph::transitivity(s3G, type = "local"), Season = 3, row.names = NULL)
CC <- bind_rows(list(s1CC, s2CC, s3CC))
#Convert ELO scores to ranks
#Might already have this


#Stats: Looking for consistency ofsocial network metrics
#across season by calculating repeatability for each metric
#with both sexes combined and separatly (MF, M, F)
#Season was a random effect, all repeatability estimates
#are "adjusted repeatabilities". Repeatability was calculated
#as the (variance of random individual effect)/(sum of random
#individual effect variance and the residual (within individual variance))
#rep = var(random indiv effect)/sum(var(random indiv effect) + var(residual))
#FOR MALES ONLY: out-strength and eigenvectors were log-transformed
#FOR BETWEENNESS: GLMMs w/ Poisson dist

