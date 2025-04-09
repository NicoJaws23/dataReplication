#Sex and rank as fixed effects
library(tidyverse)
library(lme4)
#These two are for node based permutations for sex
nodePermSex <- function(df, n, formula){
  permN <- do(n) * {
    permd <- df
    permd$Sex = sample(permd$Sex)
    m <- lmer(formula,
              data = permd)
    coef_sex <- fixef(m)["SexM"]
    data.frame(perm_est = coef_sex)
  }
  return(permN)
}
nodePermGlmmSex <- function(df, n, formula){
  permN <- do(n) * {
    permd <- df
    permd$Sex = sample(permd$Sex)
    m <- glmer(formula,
               data = permd,
               family = poisson)
    coef_sex <- fixef(m)["SexM"]
    data.frame(perm_est = coef_sex)
  }
  return(permN)
}
#These 2 are for node based permutations of rank based on differences in sex
nodePermRank <- function(df, n, formula){
  permN <- do(n) * {
    permd <- df
    permd$rank = sample(permd$rank)
    m <- lmer(formula,
              data = permd)
    coef_rank <- fixef(m)["rank"]
    data.frame(perm_est = coef_rank)
  }
  return(permN)
}
nodePermGlmmRank <- function(df, n, formula){
  permN <- do(n) * {
    permd <- df
    permd$rank = sample(permd$rank)
    m <- glmer(formula,
               data = permd,
               family = poisson)
    coef_rank <- fixef(m)["rank"]
    data.frame(perm_est = coef_rank)
  }
  return(permN)
}


#sex as fixed effect
#In/Out Degree
inD_sex <- lmer(In.Degree ~ (1|ID) + (1|Season) + Sex,
                data = allMetrics)

x <- summary(inD_sex)
inD_c <- x$coefficients

est <- inD_c["SexM", "Estimate"]
p <- nodePermSex(allMetrics, n = 1000, formula = In.Degree ~ (1|ID) + (1|Season) + Sex)
inDp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

outD_sex <- lmer(Out.Degree ~ (1|ID) + (1|Season) + Sex,
                 data = allMetrics)
x <- summary(outD_sex)
outD_c <- x$coefficients
est <- outD_c["SexM", "Estimate"]
p <- nodePermSex(allMetrics, n = 1000, formula = Out.Degree ~ (1|ID) + (1|Season) + Sex)
outDp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#In/Out Strength
inS_sex <- lmer(In.Strength ~ (1|ID) + (1|Season) + Sex,
                data = allMetrics)
x <- summary(inS_sex)
inS_c <- x$coefficients
est <- inS_c["SexM", "Estimate"]
p <- nodePermSex(allMetrics, n = 1000, formula = In.Strength ~ (1|ID) + (1|Season) + Sex)
inSp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

outS_sex <- lmer(Out.Strength ~ (1|ID) + (1|Season) + Sex,
                 data = allMetrics)
x <- summary(outS_sex)
outS_c <- x$coefficients
est <- outS_c["SexM", "Estimate"]
p <- nodePermSex(allMetrics, n = 1000, formula = Out.Strength ~ (1|ID) + (1|Season) + Sex)
outSp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Eigenvector Centrality
eg_sex <- lmer(EC.vector ~ (1|ID) + (1|Season) + Sex,
               data = allMetrics)
x <- summary(eg_sex)
eg_c <- x$coefficients
est <- eg_c["SexM", "Estimate"]
p <- nodePermSex(allMetrics, n = 1000, formula = EC.vector ~ (1|ID) + (1|Season) + Sex)
EGp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Betweenness
allMetrics <- allMetrics |>
  mutate(observation.id = factor(1:nrow(allMetrics)))
bt_sex <- glmer(b ~ (1|ID) + (1|Season) + (1|observation.id) + Sex,
                data = allMetrics,
                family = poisson)
x <- summary(bt_sex)
bt_c <- x$coefficients
est <- bt_c["SexM", "Estimate"]
p <- nodePermGlmmSex(allMetrics, n = 1000, formula = b ~ (1|ID) + (1|Season) + Sex)
BTp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Clustering Coefficient
cc_sex <- lmer(CC ~ (1|ID) + (1|Season) + Sex,
               data = allMetrics)
x <- summary(cc_sex)
cc_c <- x$coefficients
est <- cc_c["SexM", "Estimate"]
p <- nodePermSex(allMetrics, n = 1000, formula = CC ~ (1|ID) + (1|Season) + Sex)
CCp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

print(paste("In-Degree p-value:", inDp, "; Out-Degree p-value:", outDp, "; In-Strength p-value:", inSp, "; Out-Strength p-value:", outSp, "; Eigenvector p-value:", EGp, "; Clustering Coefficient p-value:", CCp, "; Betweenness p-value:", BTp))

sexResults <- data.frame(Metric.Sample = c("In-Degree", "Out-Degree", "In-Strength", "Out.Strength", "Eigenvector", "Clustering Coefficient", "Betweennes"), Estimate = c(inD_c["SexM", "Estimate"], outD_c["SexM", "Estimate"], inS_c["SexM", "Estimate"], outS_c["SexM", "Estimate"], eg_c["SexM", "Estimate"], cc_c["SexM", "Estimate"], bt_c["SexM", "Estimate"]), Standard.Error = c(inD_c["SexM", "Std. Error"], outD_c["SexM", "Std. Error"], inS_c["SexM", "Std. Error"], outS_c["SexM", "Std. Error"], eg_c["SexM", "Std. Error"], cc_c["SexM", "Std. Error"], bt_c["SexM", "Std. Error"]), p = c(inDp, outDp, inSp, outSp, EGp, CCp, BTp))
sexResults



#Rank as fixed effect for Females
#In-Degree
inDF_rank <- lmer(In.Degree ~ (1|ID) + (1|Season) + rank,
                  data = Females)
x <- summary(inDF_rank)
inDF_rankc <- x$coefficients

est <- inDF_rankc["rank", "Estimate"]
p <- nodePermRank(Females, n = 1000, formula = In.Degree ~ (1|ID) + (1|Season) + rank)
inDFrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Out-Degree
outDF_rank <- lmer(Out.Degree ~ (1|ID) + (1|Season) + rank,
                   data = Females)
x <- summary(outDF_rank)
outDF_rankc <- x$coefficients

est <- outDF_rankc["rank", "Estimate"]
p <- nodePermRank(Females, n = 1000, formula = Out.Degree ~ (1|ID) + (1|Season) + rank)
outDFrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#In-Strength
inSF_rank <- lmer(In.Strength ~ (1|ID) + (1|Season) + rank,
                  data = Females)
x <- summary(inSF_rank)
inSF_rankc <- x$coefficients

est <- inSF_rankc["rank", "Estimate"]
p <- nodePermRank(Females, n = 1000, formula = In.Strength ~ (1|ID) + (1|Season) + rank)
inSFrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Out-Strength
outSF_rank <- lmer(Out.Strength ~ (1|ID) + (1|Season) + rank,
                   data = Females)
x <- summary(outSF_rank)
outSF_rankc <- x$coefficients

est <- outSF_rankc["rank", "Estimate"]
p <- nodePermRank(Females, n = 1000, formula = Out.Strength ~ (1|ID) + (1|Season) + rank)
outSFrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Eigenvector
egF_rank <- lmer(EC.vector ~ (1|ID) + (1|Season) + rank,
                 data = Females)
x <- summary(egF_rank)
egF_rankc <- x$coefficients

est <- egF_rankc["rank", "Estimate"]
p <- nodePermRank(Females, n = 1000, formula = EC.vector ~ (1|ID) + (1|Season) + rank)
egFrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Betweenness
Females <- Females |>
  mutate(observation.id = factor(1:nrow(Females)))
btF_rank <- glmer(b ~ (1|ID) + (1|Season) + (1|observation.id) + rank,
                  data = Females,
                  family = poisson)
x <- summary(btF_rank)
btF_rankc <- x$coefficients

est <- btF_rankc["rank", "Estimate"]
p <- nodePermGlmmRank(Females, n = 1000, formula = b ~ (1|ID) + (1|Season) + (1|observation.id) + rank)
btFrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Clustering coefficient
ccF_rank <- lmer(CC ~ (1|ID) + (1|Season) + rank,
                 data = Females)
x <- summary(ccF_rank)
ccF_rankc <- x$coefficients

est <- ccF_rankc["rank", "Estimate"]
p <- nodePermRank(Females, n = 1000, formula = CC ~ (1|ID) + (1|Season) + rank)
ccFrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

print(paste("Female p-values: In-Degree p-value:", inDFrankp, "; Out-Degree p-value:", outDFrankp, "; In-Strength p-value:", inSFrankp, "; Out-Strength p-value:", outSFrankp, "; Eigenvector p-value:", egFrankp, "; Betweenness p-value:", btFrankp, "; Clustering Coefficient p-value:", ccFrankp))
#########################################################################################################################################################################################################################################################
#Rank as fixed effect males
#In-degree
inDM_rank <- lmer(In.Degree ~ (1|ID) + (1|Season) + rank,
                  data = Males)
x <- summary(inDM_rank)
inDM_rankc <- x$coefficients

est <- inDM_rankc["rank", "Estimate"]
p <- nodePermRank(Males, n = 1000, formula = In.Degree ~ (1|ID) + (1|Season) + rank)
inDMrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Out-degree
outDM_rank <- lmer(Out.Degree ~ (1|ID) + (1|Season) + rank,
                   data = Males)
x <- summary(outDM_rank)
outDM_rankc <- x$coefficients

est <- outDM_rankc["rank", "Estimate"]
p <- nodePermRank(Males, n = 1000, formula = Out.Degree ~ (1|ID) + (1|Season) + rank)
outDMrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#In-Stength
inSM_rank <- lmer(In.Strength ~ (1|ID) + (1|Season) + rank,
                  data = Males)
x <- summary(inSM_rank)
inSM_rankc <- x$coefficients

est <- inDM_rankc["rank", "Estimate"]
p <- nodePermRank(Males, n = 1000, formula = In.Strength ~ (1|ID) + (1|Season) + rank)
inSMrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Out-Strength
outSM_rank <- lmer(Out.Strength ~ (1|ID) + (1|Season) + rank,
                   data = Males)
x <- summary(outSM_rank)
outSM_rankc <- x$coefficients

est <- outSM_rankc["rank", "Estimate"]
p <- nodePermRank(Males, n = 1000, formula = Out.Strength ~ (1|ID) + (1|Season) + rank)
outSMrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Eigenvector
egM_rank <- lmer(EC.vector ~ (1|ID) + (1|Season) + rank,
                 data = Males)
x <- summary(egM_rank)
egM_rankc <- x$coefficients

est <- egM_rankc["rank", "Estimate"]
p <- nodePermRank(Males, n = 1000, formula = EC.vector ~ (1|ID) + (1|Season) + rank)
egMrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Betweenness
Males <- Males |>
  mutate(observation.id = factor(1:nrow(Males)))
btM_rank <- glmer(b ~ (1|ID) + (1|Season) + (1|observation.id) + rank,
                  data = Males,
                  family = poisson)
x <- summary(btM_rank)
btM_rankc <- x$coefficients

est <- btM_rankc["rank", "Estimate"]
p <- nodePermGlmmRank(Males, n = 1000, formula = b ~ (1|ID) + (1|Season) + (1|observation.id) + rank)
btMrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

#Clustering coefficient
ccM_rank <- lmer(CC ~ (1|ID) + (1|Season) + rank,
                 data = Males)
x <- summary(ccM_rank)
ccM_rankc <- x$coefficients

est <- ccM_rankc["rank", "Estimate"]
p <- nodePermRank(Males, n = 1000, formula = CC ~ (1|ID) + (1|Season) + rank)
ccMrankp <- (sum(p$perm_est >= abs(est)) + sum(p$perm_est <= -abs(est)))/1000

print(paste("Male p-values: In-Degree p-value:", inDMrankp, "; Out-Degree p-value:", outDMrankp, "; In-Strength p-value:", inSMrankp, "; Out-Strength p-value:", outSMrankp, "; Eigenvector p-value:", egMrankp, "; Clustering Coefficient p-value:", ccMrankp, "; Betweenness p-value:", btMrankp))

rankResults <- data.frame(Metric.Sample = c("In-Degree Females", "In-Degree Males", "Out-Degree Females", "Out-Degree Males", "In-Strength Females", "In-Strength Males", "Out-Strength Females", "Out-Strength Males", "Eigenvector Females", "Eigenvector Males", "Clustering Coefficient Females", "Clustering Coefficient Males", "Betweenness", "Betweenness"), Estimate = c(inDF_rankc["rank", "Estimate"], inDM_rankc["rank", "Estimate"], outDF_rankc["rank", "Estimate"], outDM_rankc["rank", "Estimate"], inSF_rankc["rank", "Estimate"], inSM_rankc["rank", "Estimate"], outSF_rankc["rank", "Estimate"], outSM_rankc["rank", "Estimate"], egF_rankc["rank", "Estimate"], egM_rankc["rank", "Estimate"], ccF_rankc["rank", "Estimate"], ccM_rankc["rank", "Estimate"],btF_rankc["rank", "Estimate"], btM_rankc["rank", "Estimate"]), Standard.Error = c(inDF_rankc["rank", "Std. Error"], inDM_rankc["rank", "Std. Error"], outDF_rankc["rank", "Std. Error"], outDM_rankc["rank", "Std. Error"], inSF_rankc["rank", "Std. Error"], inSM_rankc["rank", "Std. Error"], outSF_rankc["rank", "Std. Error"], outSM_rankc["rank", "Std. Error"], egF_rankc["rank", "Std. Error"], egM_rankc["rank", "Std. Error"], ccF_rankc["rank", "Std. Error"], ccM_rankc["rank", "Std. Error"],btF_rankc["rank", "Std. Error"], btM_rankc["rank", "Std. Error"]), p = c(inDFrankp, inDMrankp, outDFrankp, outDMrankp, inSFrankp, inSMrankp, outSFrankp, outSMrankp, egFrankp, egMrankp, ccFrankp, ccMrankp, btFrankp, btMrankp))
rankResults