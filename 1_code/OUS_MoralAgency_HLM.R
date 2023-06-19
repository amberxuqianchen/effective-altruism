library(lme4)
library(stargazer)
library(sjPlot)
library(ggplot2)
df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/OUS_3way_HLM'

# makedir for output if not exist
if (!dir.exists(outputpath)) {
  dir.create(outputpath)
}

# make neutral the reference category in agency
df$agency <- as.factor(df$moral_agency)
levels(df$agency) <- c("Self", "Other")
df$agency <- relevel(df$agency, ref = "Other")


# Models for positive emotions
pos_MA_noint <- lmer(pos ~ moral + agency + (1|participant_ID), data = df)
pos_MA_int <- lmer(pos ~ moral * agency + (1|participant_ID), data = df)

pos_IB_noint <- lmer(pos ~ OUS_IB + moral + agency + (1|participant_ID), data = df)
pos_IB_int <- lmer(pos ~ OUS_IB * moral * agency + (1|participant_ID), data = df)

pos_IH_noint <- lmer(pos ~ OUS_IH + moral + agency + (1|participant_ID), data = df)
pos_IH_int <- lmer(pos ~ OUS_IH * moral * agency + (1|participant_ID), data = df)

# Models for negative emotions
neg_MA_noint <- lmer(neg ~ moral + agency + (1|participant_ID), data = df)
neg_MA_int <- lmer(neg ~ moral * agency + (1|participant_ID), data = df)

neg_IB_noint <- lmer(neg ~ OUS_IB + moral + agency + (1|participant_ID), data = df)
neg_IB_int <- lmer(neg ~ OUS_IB * moral * agency + (1|participant_ID), data = df)

neg_IH_noint <- lmer(neg ~ OUS_IH + moral + agency + (1|participant_ID), data = df)
neg_IH_int <- lmer(neg ~ OUS_IH * moral * agency + (1|participant_ID), data = df)

# Combine models into a list
model_MA <- list(pos_MA_noint, pos_MA_int, neg_MA_noint, neg_MA_int)
model_IB <- list(pos_IB_noint, pos_IB_int, neg_IB_noint, neg_IB_int)
model_IH <- list(pos_IH_noint, pos_IH_int, neg_IH_noint, neg_IH_int)


stargazer(model_MA, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), out = file.path(outputpath, "OUS_MoralAgency.txt"))
stargazer(model_IB, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), covariate.labels = c("IB", "Moral", "AgencySelf", "IB:Moral", "IB:AgencySelf", "Moral:AgencySelf","IB:Moral:AgencySelf"), out = file.path(outputpath, "OUS_IB_MoralAgency.txt"))
stargazer(model_IH, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), covariate.labels = c("IH", "Moral", "AgencySelf", "IH:Moral", "IH:AgencySelf", "Moral:AgencySelf","IH:Moral:AgencySelf"), out = file.path(outputpath, "OUS_IH_MoralAgency.txt"))

# Generate the moderator plot with error bar
# Adding predicted values to the dataframe
library(interactions)
p <-interact_plot(pos_MA_int, pred = moral, modx = agency, interval = TRUE)
ggsave(file.path(outputpath,"OUS_pos_MoralAgency_inter.png"), p, width = 6, height = 4)

plot <- plot_model(pos_MA_int,type = "int")
ggsave(file.path(outputpath, "OUS_pos_MoralAgency.png"), plot, width = 6, height = 4)
plot <- plot_model(neg_MA_int,type = "int")
ggsave(file.path(outputpath, "OUS_neg_MoralAgency.png"), plot, width = 6, height = 4)


plot <- plot_model(pos_IB_int,type = "pred", terms = c("moral","OUS_IB[meansd]","agency"))
ggsave(file.path(outputpath, "OUS_pos_IB_MoralAgency_3lines .png"), plot, width = 6, height = 4)

plot <- plot_model(neg_IB_int,type = "pred", terms = c("moral","OUS_IB[meansd]","agency"))
ggsave(file.path(outputpath, "OUS_neg_IB_MoralAgency_3lines .png"), plot, width = 6, height = 4)

plot <- plot_model(pos_IH_int,type = "pred", terms = c("moral","OUS_IH[meansd]","agency"))
ggsave(file.path(outputpath, "OUS_pos_IH_MoralAgency_3lines .png"), plot, width = 6, height = 4)

plot <- plot_model(neg_IH_int,type = "pred", terms = c("moral","OUS_IH[meansd]","agency"))
ggsave(file.path(outputpath, "OUS_neg_IH_MoralAgency_3lines .png"), plot, width = 6, height = 4)

