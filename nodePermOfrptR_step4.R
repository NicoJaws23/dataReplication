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
                    data = b)
    data.frame(R = m$R$ID)
  }
  return(permN)
}
#In-degree
inDegPerm <- nodePerm(df = IO, n = 2, formula = In ~ (1|ID) + (1|Season))
inDegPval <- (sum(inDegPerm$R >= IOm$R) + 1) / (length(inDegPerm$R) + 1)

#In Degree Females
inDegPermF <- nodePerm(df = ioFemales, n = 10, forumula = In ~ (1|ID) + (1|Season))
inDegPermF_pval <- (sum(inDegPermF$R >= IOmF$R) + 1) / (length(inDegPermF$R) + 1)

#In Degree Females w/ rank
inDegPermFrank <- nodePerm(df = ioFemales, n = 10, forumula = In ~ rank + (1|ID) + (1|Season))
inDegPermFrank_pval <- (sum(inDegPermFrank$R >= IOmF_rank$R) + 1) / (length(inDegPermFrank$R) + 1)

#In Degree Males
inDegPermM <- nodePerm(df = ioMales, n = 10, forumula = In ~ (1|ID) + (1|Season))
inDegPermM_pval <- (sum(inDegPermM$R >= IOmM$R) + 1) / (length(inDegPermM$R) + 1)

#In Degree Males w/ rank
inDegPermMrank <- nodePerm(df = ioMales, n = 10, forumula = In ~ rank + (1|ID) + (1|Season))
inDegPermMrank_pval <- (sum(inDegPermMrank$R >= IOmM_rank$R) + 1) / (length(inDegPermMrank$R) + 1)

#Out degree
outDegPerm <- nodePerm(df = IO, n = 2, formula = Out ~ (1|ID) + (1|Season))
inDegPval <- (sum(outDegPerm$R >= IOmOut$R) + 1) / (length(outDegPerm$R) + 1)

#Out degree females
outDegPermF <- nodePerm(df = ioFemales, n = 2, formula = Out ~ (1|ID) + (1|Season))
outDegPermF_pval <- (sum(outDegPermF$R >= ioOutFemales$R) + 1) / (length(outDegPermF$R) + 1)

#Out degree females w/ rank
outDegPermFrank <- nodePerm(df = ioFemales, n = 2, formula = Out ~ rank + (1|ID) + (1|Season))
outDegPermFrank_pval <- (sum(outDegPermFrank$R >= ioOutFemales_rank$R) + 1) / (length(outDegPermFrank$R) + 1)

#Out degree males
outDegPermM <- nodePerm(df = ioMales, n = 2, formula = Out ~ (1|ID) + (1|Season))
outDegPermM_pval <- (sum(outDegPermM$R >= ioOutMales$R) + 1) / (length(outDegPermM$R) + 1)

#Out degree males w/ rank
outDegPermMrank <- nodePerm(df = ioMales, n = 2, formula = Out ~ rank + (1|ID) + (1|Season))
outDegPermMrank_pval <- (sum(outDegPermMrank$R >= ioOutMales_rank$R) + 1) / (length(outDegPermMrank$R) + 1)

################################################################################
################################################################################
#In-strength & Out-strenght
#In-strength
inStrengthPerm <- nodePerm(df = strength, n = 2, formula = In ~ (1|ID) + (1|Season))
inStrengthPerm_pval <- (sum(inStrengthPerm$R >= inStrength$R) + 1) / (length(inStrengthPerm$R) + 1)

#In-strength females
inStrengthPermF <- nodePerm(df = strengthF, n = 2, formula = In ~ (1|ID) + (1|Season))
inStrengthPermF_pval <- (sum(inStrengthPermF$R >= inStrengthF$R) + 1) / (length(inStrengthPermF$R) + 1)

#In-strength females w/ rank
inStrengthPermFrank <- nodePerm(df = strengthF, n = 2, formula = In ~ rank + (1|ID) + (1|Season))
inStrengthPermFrank_pval <- (sum(inStrengthPermFrank$R >= inStrengthF_rank$R) + 1) / (length(inStrengthPermFrank$R) + 1)

#In-strength Males
inStrengthPermM <- nodePerm(df = strengthM, n = 2, formula = In ~ (1|ID) + (1|Season))
inStrengthPermM_pval <- (sum(inStrengthPermM$R >= inStrengthM$R) + 1) / (length(inStrengthPermM$R) + 1)

#In-strength Males w/ rank
inStrengthPermMrank <- nodePerm(df = strengthM, n = 2, formula = In ~ rank + (1|ID) + (1|Season))
inStrengthPermMrank_pval <- (sum(inStrengthPermMrank$R >= inStrengthM_rank$R) + 1) / (length(inStrengthPermMrank$R) + 1)

#Out-strength
outStrengthPerm <- nodePerm(df = strength, n = 2, formula = Out ~ (1|ID) + (1|Season))
outStrengthPerm_pval <- (sum(outStrengthPerm$R >= outStrength$R) + 1) / (length(outStrengthPerm$R) + 1)

#out-strength females
outStrengthPermF <- nodePerm(df = strengthF, n = 2, formula = Out ~ (1|ID) + (1|Season))
outStrengthPermF_pval <- (sum(outStrengthPermF$R >= outStrengthF$R) + 1) / (length(outStrengthPermF$R) + 1)

#out-strength females w/ rank
outStrengthPermFrank <- nodePerm(df = strengthF, n = 2, formula = Out ~ rank + (1|ID) + (1|Season))
outStrengthPermFrank_pval <- (sum(outStrengthPermFrank$R >= outStrengthF_rank$R) + 1) / (length(outStrengthPermFrank$R) + 1)

#out-strength Males
outStrengthPermM <- nodePerm(df = strengthM, n = 2, formula = logOut ~ (1|ID) + (1|Season))
outStrengthPermM_pval <- (sum(outStrengthPermM$R >= outStrengthM_log$R) + 1) / (length(outStrengthPermM$R) + 1)

#out-strength Males w/ rank
outStrengthPermMrank <- nodePerm(df = strengthM, n = 2, formula = logOut ~ rank + (1|ID) + (1|Season))
outStrengthPermMrank_pval <- (sum(outStrengthPermMrank$R >= outStrengthM_rank_log$R) + 1) / (length(outStrengthPermMrank$R) + 1)

#Betweenness
btPerm <- nodePermGlmm(df = b, n = 2, formula = bt ~ (1|ID) + (1|Season))
btPerm_pval <- (sum(btPerm$R >= btM_glmm$R) + 1)/(length(btPerm$R) + 1)

#Female betweenness
btPermF <- nodePermGlmm(df = bFemales, n = 2, formula = bt ~ (1|ID) + (1|Season))
btPermF_pval <- (sum(btPermF$R >= btM_Females_glmm$R) + 1)/(length(btPermF$R) + 1)

#Female betweenness w/ rank
btPermFrank <- nodePermGlmm(df = bFemales, n = 2, formula = bt ~ rank + (1|ID) + (1|Season))
btPermFrank_pval <- (sum(btPermFrank$R >= btM_Females_rank_glmm$R) + 1)/(length(btPermF$R) + 1)

#Male betweenness
btPermM <- nodePermGlmm(df = bMemales, n = 2, formula = bt ~ (1|ID) + (1|Season))
btPermM_pval <- (sum(btPermM$R >= btM_Males_glmm$R) + 1)/(length(btPermM$R) + 1)

#Male betweenness w/ rank
btPermMrank <- nodePermGlmm(df = bMales, n = 2, formula = bt ~ (1|ID) + (1|Season))
btPermMrank_pval <- (sum(btPermM$R >= btM_Males_glmm$R) + 1)/(length(btPermM$R) + 1)

#Eigenvector
ecPerm <- nodePerm(df = EC, n = 2, forumla = EC.vector ~ (1|ID) + (1|Season))
ecPerm_pval <- (sum(ecPerm$R >= egCent$R) + 1)/(length(ecPerm$R) + 1)

#Female eigenvector
ecPermF <- nodePerm(df = ecFemales, n = 2, forumla = EC.vector ~ (1|ID) + (1|Season))
ecPermF_pval <- (sum(ecPermF$R >= egCentF$R) + 1)/(length(ecPermF$R) + 1)

#Female eigenvector w/ rank
ecPermFrank <- nodePerm(df = ecFemales, n = 2, forumla = EC.vector ~ rank + (1|ID) + (1|Season))
ecPermFrank_pval <- (sum(ecPermFrank$R >= egCentF_rank$R) + 1)/(length(ecPermFrank$R) + 1)

#Male eigenvector
ecPermM <- nodePerm(df = ecMales, n = 2, forumla = logEC ~ (1|ID) + (1|Season))
ecPermM_pval <- (sum(ecPermM$R >= egCentM_log$R) + 1)/(length(ecPermM$R) + 1)

#Male eigenvector w/ rank
ecPermFrank <- nodePerm(df = ecMales, n = 2, forumla = logEC ~ rank + (1|ID) + (1|Season))
ecPermFrank_pval <- (sum(ecPermMrank$R >= egCentM_rank_log$R) + 1)/(length(ecPermMrank$R) + 1)

#Clustering Coefficient
ccPerm <- nodePerm(df = CC, n = 2, formula = CC ~ (1|ID) + (1|Season))
ccPerm_pval <- (sum(ccPerm$R >= ccM$R) + 1)/(length(ccPerm$R) + 1)

#Female CC
ccPermF <- nodePerm(df = CC_Females, n = 2, formula = CC ~ (1|ID) + (1|Season))
ccPermF_pval <- (sum(ccPermF$R >= ccM_Females$R) + 1)/(length(ccPermF$R) + 1)

#Female CC w/ rank
ccPermFrank <- nodePerm(df = CC_Females, n = 2, formula = CC ~ rank + (1|ID) + (1|Season))
ccPermFrank_pval <- (sum(ccPermFrank$R >= ccM_Females_rank$R) + 1)/(length(ccPermFrank$R) + 1)

#Male CC
ccPermM <- nodePerm(df = CC_Males, n = 2, formula = CC ~ (1|ID) + (1|Season))
ccPermM_pval <- (sum(ccPermM$R >= ccM_Males$R) + 1)/(length(ccPermM$R) + 1)

#Male CC w/ rank
ccPermMrank <- nodePerm(df = CC_Males, n = 2, formula = CC ~ rank + (1|ID) + (1|Season))
ccPermMrank_pval <- (sum(ccPermMrank$R >= ccM_Males_rank$R) + 1)/(length(ccPermMrank$R) + 1)



