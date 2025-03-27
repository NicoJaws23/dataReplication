#Node based permutations of rptR models
library(rptR)
library(tidyverse)
library(mosaic)

n <- 10

#In-degree
permN <- do(n) * {
  permd <- IO
  permd$ID = sample(permd$ID)
  m <- rpt(In ~ (1|ID) + (1|Season),
           grname = "ID",
           data = permd,
           datatype = "Gaussian")
  data.frame(R = m$R)
}
inDeg_P <- sum(permN$R < -1 * abs(IOm$R) | permN$R > abs(IOm$R))/n
#In Degree Females
permN <- do(n) * {
   permd <- ioFemales
   permd$ID = sample(permd$ID)
  m <- rpt(In ~ (1|ID) + (1|Season),
           grname = "ID",
           data = permd,
           datatype = "Gaussian")
  data.frame(R = m$R)
}
inDegf_F_P <- sum(permN$R < -1 * abs(IOmF$R) | permN$R > abs(IOmF$R))/n
#In Degree Females w/ rank
permN <- do(n) * {
  permd <- ioFemales
  permd$ID = sample(permd$ID)
  m <- rpt(In ~ rank + (1|ID) + (1|Season),
           grname = "ID",
           data = permd,
           datatype = "Gaussian")
  data.frame(R = m$R)
}
inDegf_Frank_P <- sum(permN$R < -1 * abs(IOmF_rank$R) | permN$R > abs(IOmF_rank$R))/n
#In Degree Males
permN <- do(n) * {
  permd <- ioMales
  permd$ID = sample(permd$ID)
  m <- rpt(In ~ (1|ID) + (1|Season),
           grname = "ID",
           data = permd,
           datatype = "Gaussian")
  data.frame(R = m$R)
}
inDegf_Frank_P <- sum(permN$R < -1 * abs(IOmM$R) | permN$R > abs(IOmM$R))/n

################################################################################
################################################################################
#In-strength & Out-strenght
#In-strenght
permN <- do(n) * {
  permd <- strength
  permd$ID = sample(permd$ID)
  m <- rpt(In ~ (1|ID) + (1|Season),
           grname = "ID",
           data = permd,
           datatype = "Gaussian")
  data.frame(R = m$R)
}
inStrength_P <- (sum(permN$ID >= inStrength$R$ID) + 1) / (length(permN$ID) + 1) #Use this
#In-strength females

#Betweenness


#Eigenvector


#Clustering Coefficient


