library(lme4)
library(stargazer)

df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/'
# make neutral the reference category in agency
df$agency <- as.factor(df$moral_agency)
df$agency <- relevel(df$agency, ref = 2)


model11 <- lmer(pos ~moral + agency + (1|participant_ID), data = dfnoneutral)
model1 <- lmer(pos ~ moral * agency + (1|participant_ID), data = dfnoneutral)
model22 <- lmer(neg ~ moral + agency + (1|participant_ID), data = dfnoneutral)
model2 <- lmer(neg ~ moral * agency + (1|participant_ID), data = dfnoneutral)

stargazer(model11,model1,model22,model2, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), covariate.labels = c("Moral", "AgencySelf", "Moral:AgencySelf"), out = file.path(outputpath, "OUS_MoralAgency.txt"))


model1 <- lmer(pos ~ OUS_IB + moral + agency + (1|participant_ID), data = df)
model11 <- lmer(pos ~ OUS_IB * moral * agency + (1|participant_ID), data = df)
model2 <- lmer(neg ~ OUS_IB + moral + agency + (1|participant_ID), data = df)
model22 <- lmer(neg ~ OUS_IB * moral * agency + (1|participant_ID), data = df)
model3 <- lmer(pos ~ OUS_IH + moral + agency + (1|participant_ID), data = df)
model33 <- lmer(pos ~ OUS_IH * moral * agency + (1|participant_ID), data = df)
model4 <- lmer(neg ~ OUS_IH + moral + agency + (1|participant_ID), data = df)
model44 <- lmer(neg ~ OUS_IH * moral * agency + (1|participant_ID), data = df)

stargazer(model1,model11,model2,model22, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), covariate.labels = c("IB", "Moral", "AgencySelf", "IB:Moral", "IB:AgencySelf", "Moral:AgencySelf","IB:Moral:AgencySelf"), out = file.path(outputpath, "OUS_IB_MoralAgency.txt"))
stargazer(model3,model33,model4,model44, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), covariate.labels = c("IH", "Moral", "AgencySelf", "IH:Moral", "IH:AgencySelf", "Moral:AgencySelf","IH:Moral:AgencySelf"), out = file.path(outputpath, "OUS_IH_MoralAgency.txt"))