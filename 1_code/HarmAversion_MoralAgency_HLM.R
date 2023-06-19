library(lme4)
library(stargazer)
library(sjPlot)
library(ggplot2)
df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/HarmAversion_3way_HLM'

# make directory for output if not exist
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

pos_self_noint <- lmer(pos ~ k_self + moral + agency + (1|participant_ID), data = df)
pos_self_int <- lmer(pos ~ k_self * moral * agency + (1|participant_ID), data = df)

pos_other_noint <- lmer(pos ~ k_other + moral + agency + (1|participant_ID), data = df)
pos_other_int <- lmer(pos ~ k_other * moral * agency + (1|participant_ID), data = df)

# Models for negative emotions
neg_MA_noint <- lmer(neg ~ moral + agency + (1|participant_ID), data = df)
neg_MA_int <- lmer(neg ~ moral * agency + (1|participant_ID), data = df)

neg_self_noint <- lmer(neg ~ k_self + moral + agency + (1|participant_ID), data = df)
neg_self_int <- lmer(neg ~ k_self * moral * agency + (1|participant_ID), data = df)

neg_other_noint <- lmer(neg ~ k_other + moral + agency + (1|participant_ID), data = df)
neg_other_int <- lmer(neg ~ k_other * moral * agency + (1|participant_ID), data = df)

# Combine models into a list
model_MA <- list(pos_MA_noint, pos_MA_int, neg_MA_noint, neg_MA_int)
model_self <- list(pos_self_noint, pos_self_int, neg_self_noint, neg_self_int)
model_other <- list(pos_other_noint, pos_other_int, neg_other_noint, neg_other_int)

stargazer(model_MA, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), out = file.path(outputpath, "HarmAversion_MoralAgency.txt"))
stargazer(model_self, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), out = file.path(outputpath, "HarmAversion_k_self_MoralAgency.txt"))


stargazer(model_other, type = "text", dep.var.labels = c("Positive Emotions", "Negative Emotions"), out = file.path(outputpath, "HarmAversion_k_other_MoralAgency.txt"))

# Generate the moderator plot with error bar
# Adding predicted values to the dataframe
library(interactions)
p <- interact_plot(pos_MA_int, pred = moral, modx = agency, interval = TRUE)
ggsave(file.path(outputpath,"HarmAversion_pos_MoralAgency_inter.png"), p, width = 6, height = 4)

plot <- plot_model(pos_MA_int,type = "int")
ggsave(file.path(outputpath, "HarmAversion_pos_MoralAgency.png"), plot, width = 6, height = 4)
plot <- plot_model(neg_MA_int,type = "int")
ggsave(file.path(outputpath, "HarmAversion_neg_MoralAgency.png"), plot, width = 6, height = 4)


plot <- plot_model(pos_self_int,type = "pred", terms = c("moral","k_self[minmax]","agency"))
ggsave(file.path(outputpath, "HarmAversion_pos_k_self_MoralAgency_minmax.png"), plot, width = 6, height = 4)

plot <- plot_model(neg_self_int,type = "pred", terms = c("moral","k_self[minmax]","agency"))
ggsave(file.path(outputpath, "HarmAversion_neg_k_self_MoralAgency_minmax.png"), plot, width = 6, height = 4)

plot <- plot_model(pos_other_int,type = "pred", terms = c("moral","k_other[minmax]","agency"))
ggsave(file.path(outputpath, "HarmAversion_pos_k_other_MoralAgency_minmax.png"), plot, width = 6, height = 4)

plot <- plot_model(neg_other_int,type = "pred", terms = c("moral","k_other[minmax]","agency"))
ggsave(file.path(outputpath, "HarmAversion_neg_k_other_MoralAgency_minmax.png"), plot, width = 6, height = 4)

