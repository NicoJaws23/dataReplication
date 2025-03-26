library(tidyverse)
library(igraph)
library(EloRating)
library(rptR)
library(lme4)
library(ggplot2)
library(cowplot)

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
#Rank used later as fixed effect for separate models by sex

#FOR MALES ONLY: out-strength and eigenvectors were log-transformed
#FOR BETWEENNESS: GLMMs w/ Poisson dist

#Degree repeatability
#In-degree
IOm <- rpt(In ~ (1|ID) + (1|Season),
           grname = "ID",
           data = IO,
           datatype = "Gaussian")
summary(IOm)
IOm$R
IOm$CI_emp
IOm$P$LRT_P
IOmR <- rpt(In ~ rank + (1|ID) + (1|Season),
            grname = "ID",
            data = IO,
            datatype = "Gaussian")
#Females
ioFemales <- IO |>
  filter(Sex == "F")
IOmF <- rpt(In ~ (1|ID) + (1|Season),
            grname = "ID",
            data = ioFemales,
            datatype = "Gaussian")
summary(IOmF)
#Females with Rank
IOmF_rank <- rpt(In ~ rank + (1|ID) + (1|Season),
                 grname = "ID",
                 data = ioFemales,
                 datatype = "Gaussian")
summary(IOmF_rank)
#Males
ioMales <- IO |>
  filter(Sex == "M")
IOmM <- rpt(In ~ (1|ID) + (1|Season),
            grname = "ID",
            data = ioMales,
            datatype = "Gaussian")
summary(IOmM)
#Males with Rank
IOmM_rank <- rpt(In ~ rank + (1|ID) + (1|Season),
            grname = "ID",
            data = ioMales,
            datatype = "Gaussian")
summary(IOmM_rank)
#Out-degree
IOmOut <- rpt(Out ~ (1|ID) + (1|Season),
              grname = "ID",
              data = IO,
              datatype = "Gaussian")
summary(IOmOut)
IOmOutRank <- rpt(Out ~ rank + (1|ID) + (1|Season),
                  grname = "ID",
                  data = IO,
                  datatype = "Gaussian")
#Females
ioOutFemales <- rpt(Out ~ (1|ID) + (1|Season),
                    grname = "ID",
                    data = ioFemales,
                    datatype = "Gaussian")
summary(ioOutFemales)
#Females with rank
ioOutFemales_rank <- rpt(Out ~ rank + (1|ID) + (1|Season),
                    grname = "ID",
                    data = ioFemales,
                    datatype = "Gaussian")
summary(ioOutFemales_rank)
#Males
ioOutMales <- rpt(Out ~ (1|ID) + (1|Season),
                  grname = "ID",
                  data = ioMales,
                  datatype = "Gaussian")
summary(ioOutMales)
#Males with rank
ioOutMales_rank <- rpt(Out ~ rank + (1|ID) + (1|Season),
                  grname = "ID",
                  data = ioMales,
                  datatype = "Gaussian")
summary(ioOutMales_rank)



#Strength Repeatability
#In-Strength
inStrength <- rpt(In ~ (1|ID) + (1|Season),
                  grname = "ID",
                  data = strength,
                  datatype = "Gaussian")
summary(inStrength)
inStrengthRank <- rpt(In ~ rank + (1|ID) + (1|Season),
                      grname = "ID",
                      data = strength,
                      datatype = "Gaussian")
#Females
strengthF <- strength |>
  filter(Sex == "F")
inStrengthF <- rpt(In ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = strengthF,
                   datatype = "Gaussian")
summary(inStrengthF)
#Females with rank
inStrengthF_rank <- rpt(In ~ rank + (1|ID) + (1|Season),
                   grname = "ID",
                   data = strengthF,
                   datatype = "Gaussian")
summary(inStrengthF_rank)
#Males
strengthM <- strength |>
  filter(Sex == "M")
inStrengthM <- rpt(In ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = strengthM,
                   datatype = "Gaussian")
summary(inStrengthM)
#Males with rank
inStrengthM_rank <- rpt(In ~ rank + (1|ID) + (1|Season),
                   grname = "ID",
                   data = strengthM,
                   datatype = "Gaussian")
summary(inStrengthM_rank)
#Out-Strength
outStrength <- rpt(Out ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = strength,
                   datatype = "Gaussian")
summary(outStrength)
outStrengthRank <- rpt(Out ~ rank + (1|ID) + (1|Season),
                       grname = "ID",
                       data = strength,
                       datatype = "Gaussian")
#Females
outStrengthF <- rpt(Out ~ (1|ID) + (1|Season),
                    grname = "ID",
                    data = strengthF,
                    datatype = "Gaussian")
summary(outStrengthF)
#Females with rank
outStrengthF_rank <- rpt(Out ~ rank + (1|ID) + (1|Season),
                    grname = "ID",
                    data = strengthF,
                    datatype = "Gaussian")
summary(outStrengthF_rank)
#Males
outStrengthM <- rpt(Out ~ (1|ID) + (1|Season),
                    grname = "ID",
                    data = strengthM,
                    datatype = "Gaussian")
summary(outStrengthM)
#Males with rank
outStrengthM_rank <- rpt(Out ~ rank + (1|ID) + (1|Season),
                    grname = "ID",
                    data = strengthM,
                    datatype = "Gaussian")
summary(outStrengthM_rank)




#Eigenvector Centrality
egCent <- rpt(EC.vector ~ (1|ID) + (1|Season),
              grname = "ID",
              data = EC,
              datatype = "Gaussian")
summary(egCent)
egCentRank <- rpt(EC.vector ~ rank + (1|ID) + (1|Season),
                  grname = "ID",
                  data = EC,
                  datatype = "Gaussian")
#Females
ecFemales <- EC |>
  filter(Sex == "F")
egCentF <- rpt(EC.vector ~ (1|ID) + (1|Season),
               grname = "ID",
               data = ecFemales,
               datatype = "Gaussian")
summary(egCentF)
#Females with rank
egCentF_rank <- rpt(EC.vector ~ rank + (1|ID) + (1|Season),
               grname = "ID",
               data = ecFemales,
               datatype = "Gaussian")
summary(egCentF_rank)
#Males
ecMales <- EC |>
  filter(Sex == "M")
egCentM <- rpt(EC.vector ~ (1|ID) + (1|Season),
               grname = "ID",
               data = ecMales,
               datatype = "Gaussian")
summary(egCentM)
#Males with rank
egCentM_rank <- rpt(EC.vector ~ rank + (1|ID) + (1|Season),
               grname = "ID",
               data = ecMales,
               datatype = "Gaussian")
summary(egCentM_rank)




#Clustering coefficient, may need to recheck
ccM <- rpt(CC ~ (1|ID) + (1|Season),
           grname = "ID",
           data = CC,
           datatype = "Gaussian")
summary(ccM)
ccMRank <- rpt(CC ~ rank + (1|ID) + (1|Season),
               grname = "ID",
               data = CC,
               datatype = "Gaussian")
#Females
CC_Females <- CC |>
  filter(Sex == "F")
ccM_Females <- rpt(CC ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = CC_Females,
                   datatype = "Gaussian")
summary(ccM_Females)
#Females with rank
ccM_Females_rank <- rpt(CC ~ rank + (1|ID) + (1|Season),
                   grname = "ID",
                   data = CC_Females,
                   datatype = "Gaussian")
summary(ccM_Females_rank)
#Males
CC_Males <- CC |>
  filter(Sex == "M")
ccM_Males <- rpt(CC ~ (1|ID) + (1|Season),
                 grname = "ID",
                 data = CC_Males,
                 datatype = "Gaussian")
summary(ccM_Males)
#Males with rank
ccM_Males_rank <- rpt(CC ~ rank + (1|ID) + (1|Season),
                 grname = "ID",
                 data = CC_Males,
                 datatype = "Gaussian")
summary(ccM_Males_rank)




#Betweenness
btM <- rpt(bt ~ (1|ID) + (1|Season),
           grname = "ID",
           data = b,
           datatype = "Gaussian")
summary(btM)
btMRank <- rpt(bt ~ rank + (1|ID) + (1|Season),
               grname = "ID",
               data = b,
               datatype = "Gaussian")
#Females
bFemales <- b |>
  filter(Sex == "F")
btM_Females <- rpt(bt ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = bFemales,
                   datatype = "Gaussian")
summary(btM_Females)
#Females with rank
btM_Females_rank <- rpt(bt ~ rank + (1|ID) + (1|Season),
                   grname = "ID",
                   data = bFemales,
                   datatype = "Gaussian")
summary(btM_Females_rank)
#Males
bMales <- b |>
  filter(Sex == "M")
btM_Males <- rpt(bt ~ (1|ID) + (1|Season),
                 grname = "ID",
                 data = bMales,
                 datatype = "Gaussian")
summary(btM_Males)
#Males with rank
btM_Males_rank <- rpt(bt ~ rank + (1|ID) + (1|Season),
                 grname = "ID",
                 data = bMales,
                 datatype = "Gaussian")
summary(btM_Males_rank)

#FOR MALES ONLY: out-strength and eigenvectors were log-transformed
#FOR BETWEENNESS: GLMMs w/ Poisson dist

#Loggin male out-strength and eigenvenctor scores and re-running models
#Male Out-Strength logged
strengthM <- strengthM |>
  mutate(logOut = log(Out))
#New out strength model
outStrengthM_log <- rpt(logOut ~ (1|ID) + (1|Season),
                    grname = "ID",
                    data = strengthM,
                    datatype = "Gaussian")
summary(outStrengthM)log
#Males with rank
outStrengthM_rank_log <- rpt(logOut ~ rank + (1|ID) + (1|Season),
                         grname = "ID",
                         data = strengthM,
                         datatype = "Gaussian")
summary(outStrengthM_rank_log)



#Male eigenvectors logged
ecMales <- ecMales |>
  mutate(logEC = log(EC.vector))
#New eigenvector model
egCentM_log <- rpt(logEC ~ (1|ID) + (1|Season),
               grname = "ID",
               data = ecMales,
               datatype = "Gaussian")
summary(egCentM_log)
#Males with rank
egCentM_rank_log <- rpt(logEC ~ rank + (1|ID) + (1|Season),
                    grname = "ID",
                    data = ecMales,
                    datatype = "Gaussian")
summary(egCentM_rank_log)

#FOR BETWEENNESS: GLMMs w/ Poisson dist
btM_glmm <- rptPoisson(bt ~ (1|ID) + (1|Season),
           grname = "ID",
           data = b)
summary(btM_glmm)
btM_glmm$R$ID[1]
#Females
bFemales <- b |>
  filter(Sex == "F")
btM_Females_glmm <- rptPoisson(bt ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = bFemales)
summary(btM_Females_glmm)
#Females with rank
btM_Females_rank_glmm <- rptPoisson(bt ~ rank + (1|ID) + (1|Season),
                        grname = "ID",
                        data = bFemales)
summary(btM_Females_rank_glmm)
#Males
bMales <- b |>
  filter(Sex == "M")
btM_Males_glmm <- rptPoisson(bt ~ (1|ID) + (1|Season),
                 grname = "ID",
                 data = bMales)
summary(btM_Males_glmm)
#Males with rank
btM_Males_rank_glmm <- rptPoisson(bt ~ rank + (1|ID) + (1|Season),
                      grname = "ID",
                      data = bMales)
summary(btM_Males_rank_glmm)


#Node Based Permutations
#Shuffling IDs
names(s1G)
n <- 1000
permd <- s1G
permN <- (length = n)
for(i in 1:n){
  permd = sample(s1G$vertex.label)
  m <- igraph::degree(permd$vertex.label, mode = "in")
  permN[[i]] <- m
}

#In-Degree & Out-degree


#In-strength & Out-strenght


#Betweenness


#Eigenvector


#Clustering Coefficient


#Spearman Corrleation
network_metrics <- data.frame(In.Degree = IO$In, Out.Degree = IO$Out, In.Strength = strength$In,
                              Out.Strength = strength$Out, Eigenvector = EC$EC.vector, 
                              Cluster = CC$CC, Between = b$bt)
spearCor <- cor(network_metrics, method = "spearman")
spearCor

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

#sex as fixed effect
inD_sex <- lmer(In ~ (1|ID) + (1|Season) + Sex,
               data = IO)
summary(inD_sex)

outD_sex <- lmer(Out ~ (1|ID) + (1|Season) + Sex,
                 data = IO)
summary(outD_sex)

inS_sex <- lmer(In ~ (1|ID) + (1|Season) + Sex,
                data = strength)
summary(inS_sex)

outS_sex <- lmer(In ~ (1|ID) + (1|Season) + Sex,
                 data = strength)
summary(outS_sex)

eg_sex <- lmer(EC.vector ~ (1|ID) + (1|Season) + Sex,
               data = EC)
summary(eg_sex)

b <- b |>
  mutate(observation.id = factor(1:nrow(b)))
bt_sex <- glmer(bt ~ (1|ID) + (1|Season) + (1|observation.id) + Sex,
               data = b,
               family = poisson)
summary(bt_sex)

cc_sex <- lmer(CC ~ (1|ID) + (1|Season) + Sex,
               data = CC)
summary(cc_sex)

#remale rank as fixed effect
inDF_rank <- lmer(In ~ (1|ID) + (1|Season) + rank,
                data = ioFemales)
summary(inDF_rank)

outDF_rank <- lmer(Out ~ (1|ID) + (1|Season) + rank,
                 data = ioFemales)
summary(outDF_rank)

inSF_rank <- lmer(In ~ (1|ID) + (1|Season) + rank,
                data = strengthF)
summary(inSF_rank)

outSF_rank <- lmer(Out ~ (1|ID) + (1|Season) + rank,
                 data = strengthF)
summary(outSF_rank)

egF_rank <- lmer(EC.vector ~ (1|ID) + (1|Season) + rank,
               data = ecFemales)
summary(egF_rank)

bFemales <- bFemales |>
  mutate(observation.id = factor(1:nrow(bFemales)))
btF_rank <- glmer(bt ~ (1|ID) + (1|Season) + (1|observation.id) + rank,
                data = bFemales,
                family = poisson)
summary(btF_rank)

ccF_rank <- lmer(CC ~ (1|ID) + (1|Season) + rank,
               data = CC_Females)
summary(ccF_rank)

#Rank as fixed effect males
inDM_rank <- lmer(In ~ (1|ID) + (1|Season) + rank,
                  data = ioMales)
summary(inDM_rank)

outDM_rank <- lmer(Out ~ (1|ID) + (1|Season) + rank,
                   data = ioMales)
summary(outDF_rank)

inSM_rank <- lmer(In ~ (1|ID) + (1|Season) + rank,
                  data = strengthM)
summary(inSM_rank)

outSM_rank <- lmer(Out ~ (1|ID) + (1|Season) + rank,
                   data = strengthM)
summary(outSM_rank)

egM_rank <- lmer(EC.vector ~ (1|ID) + (1|Season) + rank,
                 data = ecMales)
summary(egM_rank)

bMales <- bMales |>
  mutate(observation.id = factor(1:nrow(bMales)))
btM_rank <- glmer(bt ~ (1|ID) + (1|Season) + (1|observation.id) + rank,
                  data = bMales,
                  family = poisson)
summary(btM_rank)

ccM_rank <- lmer(CC ~ (1|ID) + (1|Season) + rank,
                 data = CC_Males)
summary(ccM_rank)

