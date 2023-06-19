library(lme4)
library(stargazer)
library(sjPlot)
library(ggplot2)
df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/Tone_HarmAversion_3way_HLM'

# makedir for output if not exist
if (!dir.exists(outputpath)) {
  dir.create(outputpath)
}

# make neutral the reference category in agency
df$agency <- as.factor(df$moral_agency)
levels(df$agency) <- c("Self", "Other")
df$agency <- relevel(df$agency, ref = "Other")
df$moral <- df$moraljudge
# Models for positive emotions
tone_pos_MA_noint <- lmer(tone_pos ~ moral + agency + (1|participant_ID), data = df)
tone_pos_MA_int <- lmer(tone_pos ~ moral * agency + (1|participant_ID), data = df)

tone_pos_k_self_noint <- lmer(tone_pos ~ k_self + moral + agency + (1|participant_ID), data = df)
tone_pos_k_self_int <- lmer(tone_pos ~ k_self * moral * agency + (1|participant_ID), data = df)

tone_pos_k_other_noint <- lmer(tone_pos ~ k_other + moral + agency + (1|participant_ID), data = df)
tone_pos_k_other_int <- lmer(tone_pos ~ k_other * moral * agency + (1|participant_ID), data = df)

# Models for negative emotions
tone_neg_MA_noint <- lmer(tone_neg ~ moral + agency + (1|participant_ID), data = df)
tone_neg_MA_int <- lmer(tone_neg ~ moral * agency + (1|participant_ID), data = df)

tone_neg_k_self_noint <- lmer(tone_neg ~ k_self + moral + agency + (1|participant_ID), data = df)
tone_neg_k_self_int <- lmer(tone_neg ~ k_self * moral * agency + (1|participant_ID), data = df)

tone_neg_k_other_noint <- lmer(tone_neg ~ k_other + moral + agency + (1|participant_ID), data = df)
tone_neg_k_other_int <- lmer(tone_neg ~ k_other * moral * agency + (1|participant_ID), data = df)

# Combine models into a list
model_MA <- list(tone_pos_MA_noint, tone_pos_MA_int, tone_neg_MA_noint, tone_neg_MA_int)
model_k_self <- list(tone_pos_k_self_noint, tone_pos_k_self_int, tone_neg_k_self_noint, tone_neg_k_self_int)
model_k_other <- list(tone_pos_k_other_noint, tone_pos_k_other_int, tone_neg_k_other_noint, tone_neg_k_other_int)

stargazer(model_MA, type = "text", dep.var.labels = c("Positive Tone", "Negative Tone"), out = file.path(outputpath, "HarmAversion_MoralAgency.txt"))
stargazer(model_k_self, type = "text", dep.var.labels = c("Positive Tone", "Negative Tone"), out = file.path(outputpath, "Tone_k_self_MoralAgency.txt"))
stargazer(model_k_other, type = "text", dep.var.labels = c("Positive Tone", "Negative Tone"),  out = file.path(outputpath, "Tone_k_other_MoralAgency.txt"))

# Generate the moderator plot with error bar
# Adding predicted values to the dataframe
library(interactions)

p <-interact_plot(tone_pos_MA_int, pred = moral, modx = agency, interval = TRUE)+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank()
           )
ggsave(file.path(outputpath,"HarmAversion_tone_pos_MoralAgency_inter.png"), p, width = 6, height = 4)

plot <- plot_model(tone_pos_MA_int,type = "int")+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "HarmAversion_tone_pos_MoralAgency.png"), plot, width = 6, height = 4)

plot <- plot_model(tone_neg_MA_int,type = "int")+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "HarmAversion_tone_neg_MoralAgency.png"), plot, width = 6, height = 4)

plot <- plot_model(tone_pos_k_self_int,type = "pred", terms = c("moral","k_self[meansd]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "HarmAversion_tone_pos_k_self_MoralAgency_3lines .png"), plot, width = 6, height = 4)

plot <- plot_model(tone_neg_k_self_int,type = "pred", terms = c("moral","k_self[meansd]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "HarmAversion_tone_neg_k_self_MoralAgency_3lines .png"), plot, width = 6, height = 4)

plot <- plot_model(tone_pos_k_other_int,type = "pred", terms = c("moral","k_other[meansd]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
axis.title = element_text(size = 20),
axis.text = element_text(size = 18),
panel.background = element_rect(fill = "white"),
panel.grid = element_blank())
ggsave(file.path(outputpath, "HarmAversion_tone_pos_k_other_MoralAgency_3lines .png"), plot, width = 6, height = 4)

plot <- plot_model(tone_neg_k_other_int,type = "pred", terms = c("moral","k_other[meansd]","agency"))+
theme(text = element_text(size = 20),
title = element_text(size = 22),
axis.title = element_text(size = 20),
axis.text = element_text(size = 18),
panel.background = element_rect(fill = "white"),
panel.grid = element_blank())
ggsave(file.path(outputpath, "HarmAversion_tone_neg_k_other_MoralAgency_3lines .png"), plot, width = 6, height = 4)

# Minmax plots

plot <- plot_model(tone_pos_k_self_int,type = "pred", terms = c("moral","k_self[minmax]","agency"))+
theme(text = element_text(size = 20),
title = element_text(size = 22),
axis.title = element_text(size = 20),
axis.text = element_text(size = 18),
panel.background = element_rect(fill = "white"),
panel.grid = element_blank())
ggsave(file.path(outputpath, "HarmAversion_tone_pos_k_self_MoralAgency_minmax.png"), plot, width = 6, height = 4)

plot <- plot_model(tone_neg_k_self_int,type = "pred", terms = c("moral","k_self[minmax]","agency"))+
theme(text = element_text(size = 20),
title = element_text(size = 22),
axis.title = element_text(size = 20),
axis.text = element_text(size = 18),
panel.background = element_rect(fill = "white"),
panel.grid = element_blank())
ggsave(file.path(outputpath, "HarmAversion_tone_neg_k_self_MoralAgency_minmax.png"), plot, width = 6, height = 4)

plot <- plot_model(tone_pos_k_other_int,type = "pred", terms = c("moral","k_other[minmax]","agency"))+
theme(text = element_text(size = 20),
title = element_text(size = 22),
axis.title = element_text(size = 20),
axis.text = element_text(size = 18),
panel.background = element_rect(fill = "white"),
panel.grid = element_blank())
ggsave(file.path(outputpath, "HarmAversion_tone_pos_k_other_MoralAgency_minmax.png"), plot, width = 6, height = 4)

plot <- plot_model(tone_neg_k_other_int,type = "pred", terms = c("moral","k_other[minmax]","agency"))+
theme(text = element_text(size = 20),
title = element_text(size = 22),
axis.title = element_text(size = 20),
axis.text = element_text(size = 18),
panel.background = element_rect(fill = "white"),
panel.grid = element_blank())
ggsave(file.path(outputpath, "HarmAversion_tone_neg_k_other_MoralAgency_minmax.png"), plot, width = 6, height = 4)


