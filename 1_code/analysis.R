library(lme4)
library(stargazer)

df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
# change to noneutural data
dfnoneutral <- df[df$moral_valence != 0,]
# make neutral the reference category in agency
df$agency <- as.factor(df$moral_agency)
df$agency <- relevel(df$agency, ref = 2)
# summary

############################################
###  OUS  ###
############################################

model11 <- lmer(pos ~moral + agency + (1|participant_ID), data = dfnoneutral)
model1 <- lmer(pos ~ moral * agency + (1|participant_ID), data = dfnoneutral)
model22 <- lmer(neg ~ moral + agency + (1|participant_ID), data = dfnoneutral)
model2 <- lmer(neg ~ moral * agency + (1|participant_ID), data = dfnoneutral)

stargazer(model11,model1,model22,model2, type = "text")

# pos
model1 <- lmer(pos ~ OUS_overall * valence * agency + (1|participant_ID), data = df)
model2 <- lmer(pos ~ OUS_IB * valence * agency + (1|participant_ID), data = df)
model3 <- lmer(pos ~ OUS_IH * valence * agency + (1|participant_ID), data = df)
# Print out the summary statistics of the fit
stargazer(model1,model2,model3, type = "text")

# neg
model1 <- lmer(neg ~ OUS_overall * valence * agency + (1|participant_ID), data = df)
model2 <- lmer(neg ~ OUS_IB * valence * agency + (1|participant_ID), data = df)
model3 <- lmer(neg ~ OUS_IH * valence * agency + (1|participant_ID), data = df)
# Print out the summary statistics of the fit
stargazer(model1,model2,model3, type = "text")

# pos
model11 <- lmer(pos ~ OUS_overall + valence + agency + (1|participant_ID), data = dfnoneutral)
model1 <- lmer(pos ~ OUS_overall * valence * agency + (1|participant_ID), data = dfnoneutral)
model22 <- lmer(pos ~ OUS_IB + valence + agency + (1|participant_ID), data = dfnoneutral)
model2 <- lmer(pos ~ OUS_IB * valence * agency + (1|participant_ID), data = dfnoneutral)
model33 <- lmer(pos ~ OUS_IH + valence + agency + (1|participant_ID), data = dfnoneutral)
model3 <- lmer(pos ~ OUS_IH * valence * agency + (1|participant_ID), data = dfnoneutral)

# Print out the summary statistics of the fit
stargazer(model11,model1,model22,model2,model33,model3, type = "text")

# neg
model11 <- lmer(neg ~ OUS_overall + valence + agency + (1|participant_ID), data = dfnoneutral)
model1 <- lmer(neg ~ OUS_overall * valence * agency + (1|participant_ID), data = dfnoneutral)
model22 <- lmer(neg ~ OUS_IB + valence + agency + (1|participant_ID), data = dfnoneutral)
model2 <- lmer(neg ~ OUS_IB * valence * agency + (1|participant_ID), data = dfnoneutral)
model33 <- lmer(neg ~ OUS_IH + valence + agency + (1|participant_ID), data = dfnoneutral)
model3 <- lmer(neg ~ OUS_IH * valence * agency + (1|participant_ID), data = dfnoneutral)

# Print out the summary statistics of the fit
stargazer(model11,model1,model22,model2,model33,model3, type = "text")

############################################
###  Moral Rightness  ###
############################################
model1 <- lmer(pos ~ OUS_IB + agency + moral + (1|participant_ID), data = df)
model11 <- lmer(pos ~ OUS_IB * agency * moral + (1|participant_ID), data = df)
model2 <- lmer(neg ~ OUS_IB + agency + moral + (1|participant_ID), data = df)
model22 <- lmer(neg ~ OUS_IB * agency * moral + (1|participant_ID), data = df)
model3 <- lmer(pos ~ OUS_IH + agency + moral + (1|participant_ID), data = df)
model33 <- lmer(pos ~ OUS_IH * agency * moral + (1|participant_ID), data = df)
model4 <- lmer(neg ~ OUS_IH + agency + moral + (1|participant_ID), data = df)
model44 <- lmer(neg ~ OUS_IH * agency * moral + (1|participant_ID), data = df)

stargazer(model1,model11,model2,model22, type = "text")
stargazer(model3,model33,model4,model44, type = "text")

library(nnet)
# Define and fit the model
model <- multinom(moral_valence ~ OUS_IB, data = df)

# Print out the summary statistics of the fit
stargazer(model, type = "text")
############################################
###  Moral Discounting  ###
############################################

dft <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df_discount.csv')
dfself <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/dfself.csv')
dfother <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/dfother.csv')

summary(lm(corr_coeff ~ OUS_IB, data = dft))
summary(lm(corr_coeff ~ OUS_IH, data = dft))

summary(lm(corr_coeff ~ OUS_IB+OUS_IH, data = dfself))
summary(lm(corr_coeff ~ OUS_IB+OUS_IH, data = dfother))

summary(lm(corr_coeff ~ OUS_IB, data = dfself))
summary(lm(corr_coeff ~ OUS_IB, data = dfother))
summary(lm(corr_coeff ~ OUS_IH, data = dfself))
summary(lm(corr_coeff ~ OUS_IH, data = dfother))

# pearson correlation between OUS_IB and corr_coeff_z
cor.test(dft$OUS_IB, dft$corr_coeff_z)
cor.test(dft$OUS_IH, dft$corr_coeff_z)
cor.test(dfself$OUS_IB, dfself$corr_coeff_z)
cor.test(dfself$OUS_IH, dfself$corr_coeff_z)
cor.test(dfother$OUS_IB, dfother$corr_coeff_z)
cor.test(dfother$OUS_IH, dfother$corr_coeff_z)
