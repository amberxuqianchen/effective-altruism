library(lme4)
library(stargazer)
library(sjPlot)
library(ggplot2)
df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/Tone_OUS_3way_HLM'

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

tone_pos_IB_noint <- lmer(tone_pos ~ OUS_IB + moral + agency + (1|participant_ID), data = df)
tone_pos_IB_int <- lmer(tone_pos ~ OUS_IB * moral * agency + (1|participant_ID), data = df)

tone_pos_IH_noint <- lmer(tone_pos ~ OUS_IH + moral + agency + (1|participant_ID), data = df)
tone_pos_IH_int <- lmer(tone_pos ~ OUS_IH * moral * agency + (1|participant_ID), data = df)

# Models for negative emotions
tone_neg_MA_noint <- lmer(tone_neg ~ moral + agency + (1|participant_ID), data = df)
tone_neg_MA_int <- lmer(tone_neg ~ moral * agency + (1|participant_ID), data = df)

tone_neg_IB_noint <- lmer(tone_neg ~ OUS_IB + moral + agency + (1|participant_ID), data = df)
tone_neg_IB_int <- lmer(tone_neg ~ OUS_IB * moral * agency + (1|participant_ID), data = df)

tone_neg_IH_noint <- lmer(tone_neg ~ OUS_IH + moral + agency + (1|participant_ID), data = df)
tone_neg_IH_int <- lmer(tone_neg ~ OUS_IH * moral * agency + (1|participant_ID), data = df)

# Combine models into a list
model_MA <- list(tone_pos_MA_noint, tone_pos_MA_int, tone_neg_MA_noint, tone_neg_MA_int)
model_IB <- list(tone_pos_IB_noint, tone_pos_IB_int, tone_neg_IB_noint, tone_neg_IB_int)
model_IH <- list(tone_pos_IH_noint, tone_pos_IH_int, tone_neg_IH_noint, tone_neg_IH_int)


stargazer(model_MA, type = "text", dep.var.labels = c("Positive Tone", "Negative Tone"), out = file.path(outputpath, "OUS_MoralAgency.txt"))
stargazer(model_IB, type = "text", dep.var.labels = c("Positive Tone", "Negative Tone"), covariate.labels = c("IB", "Moral", "AgencySelf", "IB:Moral", "IB:AgencySelf", "Moral:AgencySelf","IB:Moral:AgencySelf"), out = file.path(outputpath, "Tone_OUS_IB_MoralAgency.txt"))
stargazer(model_IH, type = "text", dep.var.labels = c("Positive Tone", "Negative Tone"), covariate.labels = c("IH", "Moral", "AgencySelf", "IH:Moral", "IH:AgencySelf", "Moral:AgencySelf","IH:Moral:AgencySelf"), out = file.path(outputpath, "Tone_OUS_IH_MoralAgency.txt"))

# Generate the moderator plot with error bar
# Adding predicted values to the dataframe
library(interactions)
p <-interact_plot(pos_MA_int, pred = moral, modx = agency, interval = TRUE)+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath,"OUS_pos_MoralAgency_inter.png"), p, width = 6, height = 4)

plot <- plot_model(pos_MA_int,type = "int")+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "OUS_pos_MoralAgency.png"), plot, width = 6, height = 4)
plot <- plot_model(neg_MA_int,type = "int")+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "OUS_neg_MoralAgency.png"), plot, width = 6, height = 4)


plot <- plot_model(pos_IB_int,type = "pred", terms = c("moral","OUS_IB[meansd]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "OUS_pos_IB_MoralAgency_3lines .png"), plot, width = 6, height = 4)

plot <- plot_model(neg_IB_int,type = "pred", terms = c("moral","OUS_IB[meansd]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "OUS_neg_IB_MoralAgency_3lines .png"), plot, width = 6, height = 4)

plot <- plot_model(pos_IH_int,type = "pred", terms = c("moral","OUS_IH[meansd]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "OUS_pos_IH_MoralAgency_3lines .png"), plot, width = 6, height = 4)

plot <- plot_model(neg_IH_int,type = "pred", terms = c("moral","OUS_IH[meansd]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "OUS_neg_IH_MoralAgency_3lines .png"), plot, width = 6, height = 4)

# Minmax plots

plot <- plot_model(pos_IB_int,type = "pred", terms = c("moral","OUS_IB[minmax]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "OUS_pos_IB_MoralAgency_minmax.png"), plot, width = 6, height = 4)

plot <- plot_model(neg_IB_int,type = "pred", terms = c("moral","OUS_IB[minmax]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "OUS_neg_IB_MoralAgency_minmax.png"), plot, width = 6, height = 4)

plot <- plot_model(pos_IH_int,type = "pred", terms = c("moral","OUS_IH[minmax]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "OUS_pos_IH_MoralAgency_minmax.png"), plot, width = 6, height = 4)

plot <- plot_model(neg_IH_int,type = "pred", terms = c("moral","OUS_IH[minmax]","agency"))+
     theme(text = element_text(size = 20),
           title = element_text(size = 22),
           axis.title = element_text(size = 20),
           axis.text = element_text(size = 18),
           panel.background = element_rect(fill = "white"),
           panel.grid = element_blank())
ggsave(file.path(outputpath, "OUS_neg_IH_MoralAgency_minmax.png"), plot, width = 6, height = 4)