library(rstanarm)
# library(rstantools)
library(report)
library(sjPlot)
library(ggplot2)
set.seed(123)
df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/OUS_3way_bayesian'
# make other the reference category in agency
df$agency <- as.factor(df$moral_agency)
levels(df$agency) <- c("Self", "Other")
df$agency <- relevel(df$agency, ref = "Other")

# Standardize the predictors
df$OUS_IB <- scale(df$OUS_IB)
df$OUS_IH <- scale(df$OUS_IH)
df$moral <- scale(df$moral)


# Models for positive emotions
pos_MA_noint <- stan_lmer(pos ~ moral + agency + (1|participant_ID), data = df, cores = 30)
pos_MA_int <- stan_lmer(pos ~ moral * agency + (1|participant_ID), data = df, cores = 30)

pos_IB_noint <- stan_lmer(pos ~ OUS_IB + moral + agency + (1|participant_ID), data = df, cores = 30)
pos_IB_int <- stan_lmer(pos ~ OUS_IB * moral * agency + (1|participant_ID), data = df, cores = 30)

pos_IH_noint <- stan_lmer(pos ~ OUS_IH + moral + agency + (1|participant_ID), data = df, cores = 30)
pos_IH_int <- stan_lmer(pos ~ OUS_IH * moral * agency + (1|participant_ID), data = df, cores = 30)

# Models for negative emotions
neg_MA_noint <- stan_lmer(neg ~ moral + agency + (1|participant_ID), data = df, cores = 30)
neg_MA_int <- stan_lmer(neg ~ moral * agency + (1|participant_ID), data = df, cores = 30)

neg_IB_noint <- stan_lmer(neg ~ OUS_IB + moral + agency + (1|participant_ID), data = df, cores = 30)
neg_IB_int <- stan_lmer(neg ~ OUS_IB * moral * agency + (1|participant_ID), data = df, cores = 30)

neg_IH_noint <- stan_lmer(neg ~ OUS_IH + moral + agency + (1|participant_ID), data = df, cores = 30)
neg_IH_int <- stan_lmer(neg ~ OUS_IH * moral * agency + (1|participant_ID), data = df, cores = 30)

# generate reports
report_pos_MA_int <- report(pos_MA_int)
report_pos_MA_noint <- report(pos_MA_noint)
report_pos_IB_int <- report(pos_IB_int)
report_pos_IB_noint <- report(pos_IB_noint)
report_pos_IH_int <- report(pos_IH_int)
report_pos_IH_noint <- report(pos_IH_noint)

report_neg_MA_int <- report(neg_MA_int)
report_neg_MA_noint <- report(neg_MA_noint)
report_neg_IB_int <- report(neg_IB_int)
report_neg_IB_noint <- report(neg_IB_noint)
report_neg_IH_int <- report(neg_IH_int)
report_neg_IH_noint <- report(neg_IH_noint)



# save the models
sink(file.path(outputpath, "pos_MA_noint.txt"))
summary(pos_MA_noint, prob = c(0.025, 0.975),digits = 2)
# print(posterior_interval(bmodel2_angdis, level = 0.95))
print(report_pos_MA_noint)
sink()

sink(file.path(outputpath, "pos_MA_int.txt"))
summary(pos_MA_int, prob = c(0.025, 0.975),digits = 2)
print(report_pos_MA_int)
sink()

sink(file.path(outputpath, "pos_IB_noint.txt"))
summary(pos_IB_noint, prob = c(0.025, 0.975),digits = 2)
print(report_pos_IB_noint)
sink()

sink(file.path(outputpath, "pos_IB_int.txt"))
summary(pos_IB_int, prob = c(0.025, 0.975),digits = 2)
print(report_pos_IB_int)
sink()

sink(file.path(outputpath, "pos_IH_noint.txt"))
summary(pos_IH_noint, prob = c(0.025, 0.975),digits = 2)
print(report_pos_IH_noint)
sink()

sink(file.path(outputpath, "pos_IH_int.txt"))
summary(pos_IH_int, prob = c(0.025, 0.975),digits = 2)
print(report_pos_IH_int)
sink()

sink(file.path(outputpath, "neg_MA_noint.txt"))
summary(neg_MA_noint, prob = c(0.025, 0.975),digits = 2)
print(report_neg_MA_noint)
sink()

sink(file.path(outputpath, "neg_MA_int.txt"))
summary(neg_MA_int, prob = c(0.025, 0.975),digits = 2)
print(report_neg_MA_int)
sink()

sink(file.path(outputpath, "neg_IB_noint.txt"))
summary(neg_IB_noint, prob = c(0.025, 0.975),digits = 2)
print(report_neg_IB_noint)
sink()

sink(file.path(outputpath, "neg_IB_int.txt"))
summary(neg_IB_int, prob = c(0.025, 0.975),digits = 2)
print(report_neg_IB_int)
sink()

sink(file.path(outputpath, "neg_IH_noint.txt"))
summary(neg_IH_noint, prob = c(0.025, 0.975),digits = 2)
print(report_neg_IH_noint)
sink()

sink(file.path(outputpath, "neg_IH_int.txt"))
summary(neg_IH_int, prob = c(0.025, 0.975),digits = 2)
print(report_neg_IH_int)
sink()


# plot the interactions and save the plots
# plot_model(pos_MA_int, type = "int", terms = c("moral", "agency"), title = "Positive Emotions: Moral Agency Interaction")
# ggsave(file.path(outputpath, "pos_MA_int.png"), width = 6, height = 6, dpi = 300)
