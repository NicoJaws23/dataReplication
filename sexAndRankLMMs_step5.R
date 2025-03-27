#Sex and rank as fixed effects
library(tidyverse)
library(lme4)

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