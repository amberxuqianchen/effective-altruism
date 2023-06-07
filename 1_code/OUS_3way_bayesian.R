library(stargazer)
library(rstanarm)
set.seed(123)
df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/'
# make other the reference category in agency
df$agency <- as.factor(df$moral_agency)
df$agency <- relevel(df$agency, ref = 2)
dfnoneutral <- df[df$moral_agency != 0,]

# Models for positive emotions
pos_MA_noint <- stan_lmer(pos ~ moral + agency + (1|participant_ID), data = dfnoneutral, core = 30)
pos_MA_int <- stan_lmer(pos ~ moral * agency + (1|participant_ID), data = dfnoneutral, core = 30)

pos_IB_noint <- stan_lmer(pos ~ OUS_IB + moral + agency + (1|participant_ID), data = df, core = 30)
pos_IB_int <- stan_lmer(pos ~ OUS_IB * moral * agency + (1|participant_ID), data = df, core = 30)

pos_IH_noint <- stan_lmer(pos ~ OUS_IH + moral + agency + (1|participant_ID), data = df, core = 30)
pos_IH_int <- stan_lmer(pos ~ OUS_IH * moral * agency + (1|participant_ID), data = df, core = 30)

# Models for negative emotions
neg_MA_noint <- stan_lmer(neg ~ moral + agency + (1|participant_ID), data = dfnoneutral, core = 30)
neg_MA_int <- stan_lmer(neg ~ moral * agency + (1|participant_ID), data = dfnoneutral, core = 30)

neg_IB_noint <- stan_lmer(neg ~ OUS_IB + moral + agency + (1|participant_ID), data = df, core = 30)
neg_IB_int <- stan_lmer(neg ~ OUS_IB * moral * agency + (1|participant_ID), data = df, core = 30)

neg_IH_noint <- stan_lmer(neg ~ OUS_IH + moral + agency + (1|participant_ID), data = df, core = 30)
neg_IH_int <- stan_lmer(neg ~ OUS_IH * moral * agency + (1|participant_ID), data = df, core = 30)

# Combine all models into a list
models_pos <- list(pos_MA_noint, pos_MA_int, pos_IB_noint, pos_IB_int, pos_IH_noint, pos_IH_int)
models_neg <- list(neg_MA_noint, neg_MA_int, neg_IB_noint, neg_IB_int, neg_IH_noint, neg_IH_int)

# Use stargazer to create a summary table
stargazer(models_pos, type="text", title="Results for Positive Emotions", 
          model.names = c("Pos: Moral-Agency", "Pos: Moral-Agency-Interaction", "Pos: IB-Moral-Agency", "Pos: IB-Moral-Agency-Interaction", "Pos: IH-Moral-Agency", "Pos: IH-Moral-Agency-Interaction"), 
          ci.levels = .95) # 95% confidence intervals

stargazer(models_neg, type="text", title="Results for Negative Emotions", 
          model.names = c("Neg: Moral-Agency", "Neg: Moral-Agency-Interaction", "Neg: IB-Moral-Agency", "Neg: IB-Moral-Agency-Interaction", "Neg: IH-Moral-Agency", "Neg: IH-Moral-Agency-Interaction"), 
          ci.levels = .95) # 95% confidence intervals
stargazer(model11,model1,model22,model2, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), covariate.labels = c("Moral", "AgencySelf", "Moral:AgencySelf"), out = file.path(outputpath, "OUS_MoralAgency.txt"))

stargazer(model1,model11,model2,model22, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), covariate.labels = c("IB", "Moral", "AgencySelf", "IB:Moral", "IB:AgencySelf", "Moral:AgencySelf","IB:Moral:AgencySelf"), out = file.path(outputpath, "OUS_IB_MoralAgency.txt"))
stargazer(model3,model33,model4,model44, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), covariate.labels = c("IH", "Moral", "AgencySelf", "IH:Moral", "IH:AgencySelf", "Moral:AgencySelf","IH:Moral:AgencySelf"), out = file.path(outputpath, "OUS_IH_MoralAgency.txt"))