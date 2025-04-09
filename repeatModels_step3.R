#rpt lmms
#Degree repeatability
library(rptR)
library(tidyverse)
#FOR MALES ONLY: out-strength and eigenvectors were log-transformed
#FOR BETWEENNESS: GLMMs w/ Poisson dist

#Degree repeatability
#In-degree
IOm <- rpt(In.Degree ~ (1|ID) + (1|Season),
           grname = "ID",
           data = allMetrics,
           datatype = "Gaussian")
summary(IOm)
#Females
IOmF <- rpt(In.Degree ~ (1|ID) + (1|Season),
            grname = "ID",
            data = Females,
            datatype = "Gaussian")
summary(IOmF)
#Females with Rank
IOmF_rank <- rpt(In.Degree ~ rank + (1|ID) + (1|Season),
                 grname = "ID",
                 data = Females,
                 datatype = "Gaussian")
summary(IOmF_rank)
#Males
IOmM <- rpt(In.Degree ~ (1|ID) + (1|Season),
            grname = "ID",
            data = Males,
            datatype = "Gaussian")
summary(IOmM)
#Males with Rank
IOmM_rank <- rpt(In.Degree ~ rank + (1|ID) + (1|Season),
                 grname = "ID",
                 data = Males,
                 datatype = "Gaussian")
summary(IOmM_rank)
#Out-degree
IOmOut <- rpt(Out.Degree ~ (1|ID) + (1|Season),
              grname = "ID",
              data = allMetrics,
              datatype = "Gaussian")
summary(IOmOut)
#Females
ioOutFemales <- rpt(Out.Degree ~ (1|ID) + (1|Season),
                    grname = "ID",
                    data = Females,
                    datatype = "Gaussian")
summary(ioOutFemales)
#Females with rank
ioOutFemales_rank <- rpt(Out.Degree ~ rank + (1|ID) + (1|Season),
                         grname = "ID",
                         data = Females,
                         datatype = "Gaussian")
summary(ioOutFemales_rank)
#Males
ioOutMales <- rpt(Out.Degree ~ (1|ID) + (1|Season),
                  grname = "ID",
                  data = Males,
                  datatype = "Gaussian")
summary(ioOutMales)
#Males with rank
ioOutMales_rank <- rpt(Out.Degree ~ rank + (1|ID) + (1|Season),
                       grname = "ID",
                       data = Males,
                       datatype = "Gaussian")
summary(ioOutMales_rank)



#Strength Repeatability
#In-Strength
inStrength <- rpt(In.Strength ~ (1|ID) + (1|Season),
                  grname = "ID",
                  data = allMetrics,
                  datatype = "Gaussian")
summary(inStrength)
#Females
inStrengthF <- rpt(In.Strength ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = Females,
                   datatype = "Gaussian")
summary(inStrengthF)
#Females with rank
inStrengthF_rank <- rpt(In.Degree ~ rank + (1|ID) + (1|Season),
                        grname = "ID",
                        data = Females,
                        datatype = "Gaussian")
summary(inStrengthF_rank)
#Males
inStrengthM <- rpt(In.Degree ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = Males,
                   datatype = "Gaussian")
summary(inStrengthM)
#Males with rank
inStrengthM_rank <- rpt(In.Strength ~ rank + (1|ID) + (1|Season),
                        grname = "ID",
                        data = Males,
                        datatype = "Gaussian")
summary(inStrengthM_rank)
#Out-Strength
outStrength <- rpt(Out.Strength ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = allMetrics,
                   datatype = "Gaussian")
summary(outStrength)
#Females
outStrengthF <- rpt(Out.Strength ~ (1|ID) + (1|Season),
                    grname = "ID",
                    data = Females,
                    datatype = "Gaussian")
summary(outStrengthF)
#Females with rank
outStrengthF_rank <- rpt(Out.Strength ~ rank + (1|ID) + (1|Season),
                         grname = "ID",
                         data = Females,
                         datatype = "Gaussian")
summary(outStrengthF_rank)

#Male Out-Strength logged
Males <- Males |>
  mutate(logOutStrength = log(Out.Strength))
#New out strength model
outStrengthM_log <- rpt(logOutStrength ~ (1|ID) + (1|Season),
                        grname = "ID",
                        data = Males,
                        datatype = "Gaussian")
summary(outStrengthM_log)
#Males with rank
outStrengthM_rank_log <- rpt(logOutStrength ~ rank + (1|ID) + (1|Season),
                             grname = "ID",
                             data = Males,
                             datatype = "Gaussian")
summary(outStrengthM_rank_log)

#Eigenvector Centrality
egCent <- rpt(EC.vector ~ (1|ID) + (1|Season),
              grname = "ID",
              data = allMetrics,
              datatype = "Gaussian")
summary(egCent)
#Females
egCentF <- rpt(EC.vector ~ (1|ID) + (1|Season),
               grname = "ID",
               data = Females,
               datatype = "Gaussian")
summary(egCentF)
#Females with rank
egCentF_rank <- rpt(EC.vector ~ rank + (1|ID) + (1|Season),
                    grname = "ID",
                    data = Females,
                    datatype = "Gaussian")
summary(egCentF_rank)

#Male eigenvectors logged
Males <- Males |>
  mutate(logEC = log(EC.vector))
#New eigenvector model
egCentM_log <- rpt(logEC ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = Males,
                   datatype = "Gaussian")
summary(egCentM_log)
#Males with rank
egCentM_rank_log <- rpt(logEC ~ rank + (1|ID) + (1|Season),
                        grname = "ID",
                        data = Males,
                        datatype = "Gaussian")
summary(egCentM_rank_log)

#Clustering coefficient, may need to recheck
ccM <- rpt(CC ~ (1|ID) + (1|Season),
           grname = "ID",
           data = allMetrics,
           datatype = "Gaussian")
summary(ccM)
#Females
ccM_Females <- rpt(CC ~ (1|ID) + (1|Season),
                   grname = "ID",
                   data = Females,
                   datatype = "Gaussian")
summary(ccM_Females)
#Females with rank
ccM_Females_rank <- rpt(CC ~ rank + (1|ID) + (1|Season),
                        grname = "ID",
                        data = Females,
                        datatype = "Gaussian")
summary(ccM_Females_rank)
#Males
ccM_Males <- rpt(CC ~ (1|ID) + (1|Season),
                 grname = "ID",
                 data = Males,
                 datatype = "Gaussian")
summary(ccM_Males)
#Males with rank
ccM_Males_rank <- rpt(CC ~ rank + (1|ID) + (1|Season),
                      grname = "ID",
                      data = Males,
                      datatype = "Gaussian")
summary(ccM_Males_rank)

#FOR BETWEENNESS: GLMMs w/ Poisson dist
btM_glmm <- rptPoisson(b ~ (1|ID) + (1|Season),
                       grname = "ID",
                       data = allMetrics)
summary(btM_glmm)
#Females
btM_Females_glmm <- rptPoisson(b ~ (1|ID) + (1|Season),
                               grname = "ID",
                               data = Females)
summary(btM_Females_glmm)
#Females with rank
btM_Females_rank_glmm <- rptPoisson(b ~ rank + (1|ID) + (1|Season),
                                    grname = "ID",
                                    data = Females)
summary(btM_Females_rank_glmm)
#Males
btM_Males_glmm <- rptPoisson(b ~ (1|ID) + (1|Season),
                             grname = "ID",
                             data = Males)
summary(btM_Males_glmm)
#Males with rank
btM_Males_rank_glmm <- rptPoisson(b ~ rank + (1|ID) + (1|Season),
                                  grname = "ID",
                                  data = Males)
summary(btM_Males_rank_glmm)

getR <- function(model) {
  d <- data.frame(R = as.numeric(model$R["ID"]), CI = paste(model$CI_emp["ID", "2.5%"], "-", model$CI_emp["ID", "97.5%"]))
  return(d)
}
getRb <- function(model) {
  d <- data.frame(R = model$R$ID[2], CI = paste(model$CI_emp$CI_link["ID", "2.5%"], "-", model$CI_emp$CI_link["ID", "97.5%"]))
  return(d)
}
models <- list(In.Degree = IOm, In.Degree.Females = IOmF, In.Degree.Females.Rank = IOmF_rank, In.Degree.Males = IOmM, In.Degree.Males.Rank = IOmM_rank, Out.Degree = IOmOut, Out.Degree.Females = ioOutFemales, Out.Degree.Females.Rank = ioOutFemales_rank, Out.Degree.Males = ioOutMales, Out.Degree.Males.Rank = ioOutMales_rank, In.Strength = inStrength, In.Strength.Females = inStrengthF, In.Strenght.Females.Rank = inStrengthF_rank, In.Strenght.Males = inStrengthM, In.Strength.Males.Rank = inStrengthM_rank, Out.Strength = outStrength, Out.Strength.Females = outStrengthF, Out.Strength.Females.Rank = outStrengthF_rank, Out.Strength.Males = outStrengthM_log, Out.Strength.Males.Rank = outStrengthM_rank_log, Eigenvector = egCent, Eigenvector.Females = egCentF, Eigenvector.Females.Rank = egCentF_rank, Eigenvector.Males = egCentM_log, Eigenvector.Males.Rank = egCentM_rank_log, Clustering = ccM, Clustering.Females = ccM_Females, Clustering.Females.Rank = ccM_Females_rank, Clustering.Males = ccM_Males, Clustering.Males.Rank = ccM_Males_rank)

x <- lapply(models, getR)
results <- do.call(rbind, x)
results <- results |>
  mutate(Metric.Sample = c("In-Degree", "In-Degree Females", "In-Degree Female Rank", "In-Degree Males", "In-Degree Males Rank", "Out-Degree", "Out-Degree Females", "Out-Degree Females Rank", "Out-Degree Males", "Out-Degree Males Rank", "In-Strength", "In-Strength Females", "In-Strength Females Rank", "In-Strength Males", "In-Strength Males Rank", "Out-Strength", "Out-Strength Females", "Out-Strength Females Rank", "Out-Strength Males", "Out-Strength Males Rank", "Eigenvector", "Eigenvector Females", "Eigenvector Females Rank", "Eigenvector Males", "Eigenvector Males Rank", "Clustering Coefficient", "Clustering Coefficient Females", "Clustering Coefficient Females Rank", "Clustering Coefficient Males", "Clustering Coefficient Males Rank"))

#Betweenness
modelB <- list(Betweenness = btM_glmm, Betweenness.Females = btM_Females_glmm, Betweenness.Females.Rank = btM_Females_rank_glmm, Betweenness.Males = btM_Males_glmm, Betweenness.Males.Rank = btM_Males_rank_glmm)
xB <- lapply(modelB, getRb)
resultsB <- do.call(rbind, xB)
resultsB <- resultsB |>
  mutate(Metric.Sample = c("Betweenness", "Betweenness Females", "Betweenness Females Rank", "Betweenness Males", "Betweenness Males Rank"))

allResults <- bind_rows(list(results, resultsB))
allResults <- allResults |>
  relocate(Metric.Sample, .before = R)
rownames(allResults) <- NULL
allResults