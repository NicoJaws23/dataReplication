#Node based permutations of rptR models
library(rptR)
library(tidyverse)
library(mosaic)

nodePerm <- function(df, n, formula){
  permN <- do(n) * {
    permd <- df
    permd$ID = sample(permd$ID)
    m <- rpt(formula,
             grname = "ID",
             data = permd,
             datatype = "Gaussian")
    data.frame(R = m$R$ID)
  }
  return(permN)
}
nodePermGlmm <- function(df, n, formula){
  permN <- do(n) * {
    permd <- df
    permd$ID = sample(permd$ID)
    m <- rptPoisson(formula,
                    grname = "ID",
                    data = permd)
    data.frame(R = m$R$ID)
  }
  return(permN)
}

#In-degree
inDegPerm <- nodePerm(df = allMetrics, n = 10, formula = In.Degree ~ (1|ID) + (1|Season))
inDegPval <- (sum(inDegPerm$R >= abs(IOm$R)) + sum(inDegPerm$R <= -abs(IOm$R)))/10

#In Degree Females
inDegPermF <- nodePerm(df = Females, n = 10, formula = In.Degree ~ (1|ID) + (1|Season))
inDegPermF_pval <- (sum(inDegPermF$R >= abs(IOmF$R)) + sum(inDegPermF$R <= -abs(IOmF$R)))/10

#In Degree Females w/ rank
inDegPermFrank <- nodePerm(df = Females, n = 10, formula = In.Degree ~ rank + (1|ID) + (1|Season))
inDegPermFrank_pval <- (sum(inDegPermFrank$R >= abs(IOmF_rank$R)) + sum(inDegPermFrank$R <= -abs(IOmF_rank$R)))/10

#In Degree Males
inDegPermM <- nodePerm(df = Males, n = 10, formula = In.Degree ~ (1|ID) + (1|Season))
inDegPermM_pval <- (sum(inDegPermM$R >= abs(IOmM$R)) + sum(inDegPermM$R <= -abs(IOmM$R)))/10

#In Degree Males w/ rank
inDegPermMrank <- nodePerm(df = Males, n = 10, formula = In.Degree ~ rank + (1|ID) + (1|Season))
inDegPermMrank_pval <- (sum(inDegPermMrank$R >= abs(IOmM_rank$R)) + sum(inDegPermMrank$R <= -abs(IOmM_rank$R)))/10

#Out degree
outDegPerm <- nodePerm(df = allMetrics, n = 10, formula = Out.Degree ~ (1|ID) + (1|Season))
outDegPval <- (sum(outDegPerm$R >= abs(IOmOut$R)) + sum(outDegPerm$R <= -abs(IOmOut$R)))/10

#Out degree females
outDegPermF <- nodePerm(df = Females, n = 10, formula = Out.Degree ~ (1|ID) + (1|Season))
outDegPermF_pval <- (sum(outDegPermF$R >= abs(ioOutFemales$R)) + sum(outDegPermF$R <= -abs(ioOutFemales$R)))/10

#Out degree females w/ rank
outDegPermFrank <- nodePerm(df = Females, n = 10, formula = Out.Degree ~ rank + (1|ID) + (1|Season))
outDegPermFrank_pval <- (sum(outDegPermFrank$R >= abs(ioOutFemales_rank$R)) + sum(outDegPermFrank$R <= -abs(ioOutFemales_rank$R)))/10

#Out degree males
outDegPermM <- nodePerm(df = Males, n = 10, formula = Out.Degree ~ (1|ID) + (1|Season))
outDegPermM_pval <- (sum(outDegPermM$R >= abs(ioOutMales$R)) + sum(outDegPermM$R <= -abs(ioOutMales$R)))/10

#Out degree males w/ rank
outDegPermMrank <- nodePerm(df = Males, n = 10, formula = Out.Degree ~ rank + (1|ID) + (1|Season))
outDegPermMrank_pval <- (sum(outDegPermMrank$R >= abs(ioOutMales_rank$R)) + sum(outDegPermMrank$R <= -abs(ioOutMales_rank$R)))/10
################################################################################
################################################################################
#In-strength
inStrengthPerm <- nodePerm(df = allMetrics, n = 10, formula = In.Strength ~ (1|ID) + (1|Season))
inStrengthPerm_pval <- (sum(inStrengthPerm$R >= abs(inStrength$R)) + sum(inStrengthPerm$R <= -abs(inStrength$R)))/10

#In-strength females
inStrengthPermF <- nodePerm(df = Females, n = 10, formula = In.Strength ~ (1|ID) + (1|Season))
inStrengthPermF_pval <- (sum(inStrengthPermF$R >= abs(inStrengthF$R)) + sum(inStrengthPermF$R <= -abs(inStrengthF$R)))/10

#In-strength females w/ rank
inStrengthPermFrank <- nodePerm(df = Females, n = 10, formula = In.Strength ~ rank + (1|ID) + (1|Season))
inStrengthPermFrank_pval <- (sum(inStrengthPermFrank$R >= abs(inStrengthF_rank$R)) + sum(inStrengthPermFrank$R <= -abs(inStrengthF_rank$R)))/10

#In-strength Males
inStrengthPermM <- nodePerm(df = Males, n = 10, formula = In.Strength ~ (1|ID) + (1|Season))
inStrengthPermM_pval <- (sum(inStrengthPermM$R >= abs(inStrengthM$R)) + sum(inStrengthPermM$R <= -abs(inStrengthM$R)))/10

#In-strength Males w/ rank
inStrengthPermMrank <- nodePerm(df = Males, n = 10, formula = In.Strength ~ rank + (1|ID) + (1|Season))
inStrengthPermMrank_pval <- (sum(inStrengthPermMrank$R >= abs(inStrengthM_rank$R)) + sum(inStrengthPermMrank$R <= -abs(inStrengthM_rank$R)))/10

#Out-strength
outStrengthPerm <- nodePerm(df = allMetrics, n = 10, formula = Out.Strength ~ (1|ID) + (1|Season))
outStrengthPerm_pval <- (sum(outStrengthPerm$R >= abs(outStrength$R)) + sum(outStrengthPerm$R <= -abs(outStrength$R)))/10

#out-strength females
outStrengthPermF <- nodePerm(df = Females, n = 10, formula = Out.Strength ~ (1|ID) + (1|Season))
outStrengthPermF_pval <- (sum(outStrengthPermF$R >= abs(outStrengthF$R)) + sum(outStrengthPermF$R <= -abs(outStrengthF$R)))/10

#out-strength females w/ rank
outStrengthPermFrank <- nodePerm(df = Females, n = 10, formula = Out.Strength ~ rank + (1|ID) + (1|Season))
outStrengthPermFrank_pval <- (sum(outStrengthPermFrank$R >= abs(outStrengthF_rank$R)) + sum(outStrengthPermFrank$R <= -abs(outStrengthF_rank$R)))/10

#out-strength Males
outStrengthPermM <- nodePerm(df = Males, n = 10, formula = logOutStrength ~ (1|ID) + (1|Season))
outStrengthPermM_pval <- (sum(outStrengthPermM$R >= abs(outStrengthM_log$R)) + sum(outStrengthPermM$R <= -abs(outStrengthM_log$R)))/10

#out-strength Males w/ rank
outStrengthPermMrank <- nodePerm(df = Males, n = 10, formula = logOutStrength ~ rank + (1|ID) + (1|Season))
outStrengthPermMrank_pval <- (sum(outStrengthPermMrank$R >= abs(outStrengthM_rank_log$R)) + sum(outStrengthPermMrank$R <= -abs(outStrengthM_rank_log$R)))/10


#Betweenness
btPerm <- nodePermGlmm(df = allMetrics, n = 10, formula = b ~ (1|ID) + (1|Season))
btPerm_pval <- (sum(btPerm$R >= abs(btM_glmm$R)) + sum(btPerm$R <= -abs(btM_glmm$R)))/10

#Female betweenness
btPermF <- nodePermGlmm(df = Females, n = 10, formula = b ~ (1|ID) + (1|Season))
btPermF_pval <- (sum(btPermF$R >= abs(btM_Females_glmm$R)) + sum(btPermF$R <= -abs(btM_Females_glmm$R)))/10

#Female betweenness w/ rank
btPermFrank <- nodePermGlmm(df = Females, n = 10, formula = b ~ rank + (1|ID) + (1|Season))
btPermFrank_pval <- (sum(btPermFrank$R >= abs(btM_Females_rank_glmm$R)) + sum(btPermFrank$R <= -abs(btM_Females_rank_glmm$R)))/10

#Male betweenness
btPermM <- nodePermGlmm(df = Males, n =10, formula = b ~ (1|ID) + (1|Season))
btPermM_pval <- (sum(btPermM$R >= abs(btM_Males_glmm$R)) + sum(btPermM$R <= -abs(btM_Males_glmm$R)))/10

#Male betweenness w/ rank
btPermMrank <- nodePermGlmm(df = Males, n = 10, formula = b ~ (1|ID) + (1|Season))
btPermMrank_pval <- (sum(btPermMrank$R >= abs(btM_Males_rank_glmm$R)) + sum(btPermMrank$R <= -abs(btM_Males_rank_glmm$R)))/10

#Eigenvector
ecPerm <- nodePerm(df = allMetrics, n = 10, formula = EC.vector ~ (1|ID) + (1|Season))
ecPerm_pval <- (sum(ecPerm$R >= abs(egCent$R)) + sum(ecPerm$R <= -abs(egCent$R)))/10

#Female eigenvector
ecPermF <- nodePerm(df = Females, n = 10, formula = EC.vector ~ (1|ID) + (1|Season))
ecPermF_pval <- (sum(ecPermF$R >= abs(egCentF$R)) + sum(ecPermF$R <= -abs(egCentF$R)))/10

#Female eigenvector w/ rank
ecPermFrank <- nodePerm(df = Females, n = 10, formula = EC.vector ~ rank + (1|ID) + (1|Season))
ecPermFrank_pval <- (sum(ecPermFrank$R >= abs(egCentF_rank$R)) + sum(ecPermFrank$R <= -abs(egCentF_rank$R)))/10

#Male eigenvector
ecPermM <- nodePerm(df = Males, n = 10, formula = logEC ~ (1|ID) + (1|Season))
ecPermM_pval <- (sum(ecPermM$R >= abs(egCentM_log$R)) + sum(ecPermM$R <= -abs(egCentM_log$R)))/10

#Male eigenvector w/ rank
ecPermMrank <- nodePerm(df = Males, n = 10, formula = logEC ~ rank + (1|ID) + (1|Season))
ecPermMrank_pval <- (sum(ecPermMrank$R >= abs(egCentM_rank_log$R)) + sum(ecPermMrank$R <= -abs(egCentM_rank_log$R)))/10

#Clustering Coefficient
ccPerm <- nodePerm(df = allMetrics, n = 10, formula = CC ~ (1|ID) + (1|Season))
ccPerm_pval <- (sum(ccPerm$R >= abs(ccM$R)) + sum(ccPerm$R <= -abs(ccM$R)))/10

#Female CC
ccPermF <- nodePerm(df = Females, n = 10, formula = CC ~ (1|ID) + (1|Season))
ccPermF_pval <- (sum(ccPermF$R >= abs(ccM_Females$R)) + sum(ccPermF$R <= -abs(ccM_Females$R)))/10

#Female CC w/ rank
ccPermFrank <- nodePerm(df = Females, n = 10, formula = CC ~ rank + (1|ID) + (1|Season))
ccPermFrank_pval <- (sum(ccPermFrank$R >= abs(ccM_Females_rank$R)) + sum(ccPermFrank$R <= -abs(ccM_Females_rank$R)))/10

#Male CC
ccPermM <- nodePerm(df = Males, n = 10, formula = CC ~ (1|ID) + (1|Season))
ccPermM_pval <- (sum(ccPermM$R >= abs(ccM_Males$R)) + sum(ccPermM$R <= -abs(ccM_Males$R)))/10

#Male CC w/ rank
ccPermMrank <- nodePerm(df = Males, n = 10, formula = CC ~ rank + (1|ID) + (1|Season))
ccPermMrank_pval <- (sum(ccPermMrank$R >= abs(ccM_Males_rank$R)) + sum(ccPermMrank$R <= -abs(ccM_Males_rank$R)))/10

allResults <- allResults |>
  mutate(p = c(inDegPval, inDegPermF_pval, inDegPermFrank_pval, inDegPermM_pval, inDegPermMrank_pval, outDegPval, outDegPermF_pval, outDegPermFrank_pval, outDegPermM_pval, outDegPermMrank_pval, inStrengthPerm_pval, inStrengthPermF_pval, inStrengthPermFrank_pval, inStrengthPermM_pval, inStrengthPermMrank_pval, outStrengthPerm_pval, outStrengthPermF_pval, outStrengthPermFrank_pval, outStrengthPermM_pval, outStrengthPermMrank_pval, ecPerm_pval, ecPermF_pval, ecPermFrank_pval, ecPermM_pval, ecPermMrank_pval, ccPerm_pval, ccPermF_pval, ccPermFrank_pval, ccPermM_pval, ccPermMrank_pval, btPerm_pval, btPermF_pval, btPermFrank_pval, btPermM_pval, btPermMrank_pval))
allResults


network_metrics <- data.frame(In.Degree = allMetrics$In.Degree, Out.Degree = allMetrics$Out.Degree, In.Strength = allMetrics$In.Strength, Out.Strength = allMetrics$Out.Strength, Eigenvector = allMetrics$EC.vector, Cluster = allMetrics$CC, Between = allMetrics$b)
spearCor <- cor(network_metrics, method = "spearman")

spearCor_p <- cor_pmat(
  data = network_metrics,
  vars = NULL,
  method = "spearman",
  alternative = "two.sided",
  conf.level = 0.95)

spearCor
spearCor_p