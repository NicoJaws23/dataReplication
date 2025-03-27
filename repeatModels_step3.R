#rpt lmms
#Degree repeatability
library(rptR)
library(tidyverse)
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
summary(outStrengthM_log)
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
