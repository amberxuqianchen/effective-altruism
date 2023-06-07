library(rstanarm)
library(report)
library(sjPlot)
library(ggplot2)
set.seed(123)
df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/HarmAversion_3way_bayesian'

# makedir if not exist
if (!dir.exists(outputpath)) {
  dir.create(outputpath)
}

# make other the reference category in agency
df$agency <- as.factor(df$moral_agency)
levels(df$agency) <- c("Self", "Other")
df$agency <- relevel(df$agency, ref = "Other")

# Standardize the predictors
df$k_self <- scale(df$k_self)
df$k_other <- scale(df$k_other)
df$moral <- scale(df$moral)


# Models for positive emotions
pos_kself_noint <- stan_lmer(pos ~ k_self + moral + agency + (1|participant_ID), data = df, cores = 30)
pos_kself_int <- stan_lmer(pos ~ k_self * moral * agency + (1|participant_ID), data = df, cores = 30)

pos_kother_noint <- stan_lmer(pos ~ k_other + moral + agency + (1|participant_ID), data = df, cores = 30)
pos_kother_int <- stan_lmer(pos ~ k_other * moral * agency + (1|participant_ID), data = df, cores = 30)

# Models for negative emotions
neg_kself_noint <- stan_lmer(neg ~ k_self + moral + agency + (1|participant_ID), data = df, cores = 30)
neg_kself_int <- stan_lmer(neg ~ k_self * moral * agency + (1|participant_ID), data = df, cores = 30)

neg_kother_noint <- stan_lmer(neg ~ k_other + moral + agency + (1|participant_ID), data = df, cores = 30)
neg_kother_int <- stan_lmer(neg ~ k_other * moral * agency + (1|participant_ID), data = df, cores = 30)

# generate reports
report_pos_kself_int <- report(pos_kself_int)
report_pos_kself_noint <- report(pos_kself_noint)
report_pos_kother_int <- report(pos_kother_int)
report_pos_kother_noint <- report(pos_kother_noint)

report_neg_kself_int <- report(neg_kself_int)
report_neg_kself_noint <- report(neg_kself_noint)
report_neg_kother_int <- report(neg_kother_int)
report_neg_kother_noint <- report(neg_kother_noint)

# save the models
sink(file.path(outputpath, "pos_kself_noint.txt"))
print(report_pos_kself_noint)
summary(pos_kself_noint, prob = c(0.025, 0.975),digits = 2)
sink()

sink(file.path(outputpath, "pos_kself_int.txt"))
print(report_pos_kself_int)
summary(pos_kself_int, prob = c(0.025, 0.975),digits = 2)
sink()

sink(file.path(outputpath, "pos_kother_noint.txt"))
print(report_pos_kother_noint)
summary(pos_kother_noint, prob = c(0.025, 0.975),digits = 2)
sink()

sink(file.path(outputpath, "pos_kother_int.txt"))
print(report_pos_kother_int)
summary(pos_kother_int, prob = c(0.025, 0.975),digits = 2)
sink()

sink(file.path(outputpath, "neg_kself_noint.txt"))
print(report_neg_kself_noint)
summary(neg_kself_noint, prob = c(0.025, 0.975),digits = 2)
sink()

sink(file.path(outputpath, "neg_kself_int.txt"))
print(report_neg_kself_int)
summary(neg_kself_int, prob = c(0.025, 0.975),digits = 2)
sink()

sink(file.path(outputpath, "neg_kother_noint.txt"))
print(report_neg_kother_noint)
summary(neg_kother_noint, prob = c(0.025, 0.975),digits = 2)
sink()

sink(file.path(outputpath, "neg_kother_int.txt"))
print(report_neg_kother_int)
summary(neg_kother_int,prob = c(0.025, 0.975),digits = 2)
sink()

