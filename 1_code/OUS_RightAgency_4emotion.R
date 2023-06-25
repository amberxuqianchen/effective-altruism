library(lme4)
library(stargazer)
library(sjPlot)
library(ggplot2)
df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/OUS_RightAgency_4emotion/'

# make directory for output if not exist
if (!dir.exists(outputpath)) {
  dir.create(outputpath)
}

# make neutral the reference category in agency
df$agency <- as.factor(df$moral_agency)
levels(df$agency) <- c("Self", "Other")
df$agency <- relevel(df$agency, ref = "Other")
df$moral_agency <- as.factor(df$moral_agency)
levels(df$moral_agency) <- c("Self", "Other")
df$moral_agency <- relevel(df$moral_agency, ref = "Self")
df$moral_valence <- as.factor(df$moral_valence)
levels(df$moral_valence) <- c("Moral", "Immoral")
df$valence <- as.factor(df$moral_valence)
levels(df$valence) <- c("Moral", "Immoral")
df$valence <- relevel(df$valence, ref = "Immoral")


dfbert <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Projects/BERT/ccr/dfbert.csv')
# add prefix to column names
colnames(dfbert) <- paste0("bert_", colnames(dfbert))
# convert to int
dfbert$bert_sig_order <- as.integer(dfbert$bert_sig_order)
df$sig_order <- as.integer(df$sig_order)
# merge based on sig_order
df <- merge(df, dfbert, by.x = "sig_order", by.y = "bert_sig_order", all = FALSE)
df$rightness <- df$moraljudge

# generate pos_self as the mean of proud
df$pos_self <- df$proud
# generate pos_other as the mean of grateful and elevated
df$pos_other <- (df$grateful + df$elevated)/2

# self-blaming (guilt, shame, embarrass), other-blaming (anger, disgust,contempt)
df$neg_self <- (df$guilt + df$shameful + df$embarrassed)/3
df$neg_other <- (df$angry + df$disgusted + df$contemptuous)/3

# Models for rightness and agency and their interaction on the 4 emotions
ps_ra_noint <- lmer(pos_self ~ rightness * agency + (1|participant_ID), data = df)
ps_ra_int <- lmer(pos_self ~ rightness * agency + (1|participant_ID), data = df)
pn_ra_noint <- lmer(neg_self ~ rightness * agency + (1|participant_ID), data = df)
pn_ra_int <- lmer(neg_self ~ rightness * agency + (1|participant_ID), data = df)
po_ra_noint <- lmer(pos_other ~ rightness * agency + (1|participant_ID), data = df)
po_ra_int <- lmer(pos_other ~ rightness * agency + (1|participant_ID), data = df)
nn_ra_noint <- lmer(neg_other ~ rightness * agency + (1|participant_ID), data = df)
nn_ra_int <- lmer(neg_other ~ rightness * agency + (1|participant_ID), data = df)

model4emotion <- list(ps_ra_noint, ps_ra_int, pn_ra_noint, pn_ra_int, po_ra_noint, po_ra_int, nn_ra_noint, nn_ra_int) 
stargazer(model4emotion, type = "text", out = paste0(outputpath, "model4emotion.txt"))


# plot the interaction
ggplot(df, aes(x = moral_valence, y = pos_self, fill = moral_agency)) + 
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "mean(proud)", title = "Self-prasing emotion")+
    theme_bw()
ggsave(paste0(outputpath, "pos_self.png"), width = 6, height = 4, units = "in")

ggplot(df, aes(x = moral_valence, y = neg_self, fill = moral_agency)) + 
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "mean(guilt, shame, embarrassed)", title = "Self-blaming emotion")+
    theme_bw()
ggsave(paste0(outputpath, "neg_self.png"), width = 6, height = 4, units = "in")

ggplot(df, aes(x = moral_valence, y = pos_other, fill = moral_agency)) + 
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "mean(grateful, elevated)", title = "Other-prasing emotion")+
    theme_bw()
ggsave(paste0(outputpath, "pos_other.png"), width = 6, height = 4, units = "in")

ggplot(df, aes(x = moral_valence, y = neg_other, fill = moral_agency)) + 
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "mean(angry, disgusted, contemptuous)", title = "Other-blaming emotion")+
    theme_bw()
ggsave(paste0(outputpath, "neg_other.png"), width = 6, height = 4, units = "in")

# Models for valence and agency and their interaction on the 4 emotions
ps_va_noint <- lmer(pos_self ~ valence + agency + (1|participant_ID), data = df)
ps_va_int <- lmer(pos_self ~ valence * agency + (1|participant_ID), data = df)
pn_va_noint <- lmer(neg_self ~ valence + agency + (1|participant_ID), data = df)
pn_va_int <- lmer(neg_self ~ valence * agency + (1|participant_ID), data = df)
po_va_noint <- lmer(pos_other ~ valence + agency + (1|participant_ID), data = df)
po_va_int <- lmer(pos_other ~ valence * agency + (1|participant_ID), data = df)
nn_va_noint <- lmer(neg_other ~ valence + agency + (1|participant_ID), data = df)
nn_va_int <- lmer(neg_other ~ valence * agency + (1|participant_ID), data = df)

model_valence_4emotion <- list(ps_va_noint, ps_va_int, pn_va_noint, pn_va_int, po_va_noint, po_va_int, nn_va_noint, nn_va_int)
stargazer(model_valence_4emotion, type = "text", out = paste0(outputpath, "model_valence_4emotion.txt"))
# Models for valence and agency and OUS_IB their interaction on the 4 emotions
ps_va_ousib_noint <- lmer(pos_self ~ valence + agency + OUS_IB + (1|participant_ID), data = df)
ps_va_ousib_int <- lmer(pos_self ~ valence * agency * OUS_IB + (1|participant_ID), data = df)
pn_va_ousib_noint <- lmer(neg_self ~ valence + agency + OUS_IB + (1|participant_ID), data = df)
pn_va_ousib_int <- lmer(neg_self ~ valence * agency * OUS_IB + (1|participant_ID), data = df)
po_va_ousib_noint <- lmer(pos_other ~ valence + agency + OUS_IB + (1|participant_ID), data = df)
po_va_ousib_int <- lmer(pos_other ~ valence * agency * OUS_IB + (1|participant_ID), data = df)
nn_va_ousib_noint <- lmer(neg_other ~ valence + agency + OUS_IB + (1|participant_ID), data = df)
nn_va_ousib_int <- lmer(neg_other ~ valence * agency * OUS_IB + (1|participant_ID), data = df)

model_valence_4emotion_ousib <- list(ps_va_ousib_noint, ps_va_ousib_int, pn_va_ousib_noint, pn_va_ousib_int, po_va_ousib_noint, po_va_ousib_int, nn_va_ousib_noint, nn_va_ousib_int)
stargazer(model_valence_4emotion_ousib, type = "text", out = paste0(outputpath, "model_valence_4emotion_ousib.txt"))

# Models for valence and agency and OUS_IH their interaction on the 4 emotions
ps_va_ousih_noint <- lmer(pos_self ~ valence + agency + OUS_IH + (1|participant_ID), data = df)
ps_va_ousih_int <- lmer(pos_self ~ valence * agency * OUS_IH + (1|participant_ID), data = df)
pn_va_ousih_noint <- lmer(neg_self ~ valence + agency + OUS_IH + (1|participant_ID), data = df)
pn_va_ousih_int <- lmer(neg_self ~ valence * agency * OUS_IH + (1|participant_ID), data = df)
po_va_ousih_noint <- lmer(pos_other ~ valence + agency + OUS_IH + (1|participant_ID), data = df)
po_va_ousih_int <- lmer(pos_other ~ valence * agency * OUS_IH + (1|participant_ID), data = df)
nn_va_ousih_noint <- lmer(neg_other ~ valence + agency + OUS_IH + (1|participant_ID), data = df)
nn_va_ousih_int <- lmer(neg_other ~ valence * agency * OUS_IH + (1|participant_ID), data = df)

model_valence_4emotion_ousih <- list(ps_va_ousih_noint, ps_va_ousih_int, pn_va_ousih_noint, pn_va_ousih_int, po_va_ousih_noint, po_va_ousih_int, nn_va_ousih_noint, nn_va_ousih_int)
stargazer(model_valence_4emotion_ousih, type = "text", out = paste0(outputpath, "model_valence_4emotion_ousih.txt"))

# Models for valence and agency and bert_ousih their interaction on the 4 emotions
ps_va_bert_ousih_noint <- lmer(pos_self ~ valence + agency + bert_ousih + (1|participant_ID), data = df)
ps_va_bert_ousih_int <- lmer(pos_self ~ valence * agency * bert_ousih + (1|participant_ID), data = df)
pn_va_bert_ousih_noint <- lmer(neg_self ~ valence + agency + bert_ousih + (1|participant_ID), data = df)
pn_va_bert_ousih_int <- lmer(neg_self ~ valence * agency * bert_ousih + (1|participant_ID), data = df)
po_va_bert_ousih_noint <- lmer(pos_other ~ valence + agency + bert_ousih + (1|participant_ID), data = df)
po_va_bert_ousih_int <- lmer(pos_other ~ valence * agency * bert_ousih + (1|participant_ID), data = df)
nn_va_bert_ousih_noint <- lmer(neg_other ~ valence + agency + bert_ousih + (1|participant_ID), data = df)
nn_va_bert_ousih_int <- lmer(neg_other ~ valence * agency * bert_ousih + (1|participant_ID), data = df)

model_valence_4emotion_bert_ousih <- list(ps_va_bert_ousih_noint, ps_va_bert_ousih_int, pn_va_bert_ousih_noint, pn_va_bert_ousih_int, po_va_bert_ousih_noint, po_va_bert_ousih_int, nn_va_bert_ousih_noint, nn_va_bert_ousih_int)
stargazer(model_valence_4emotion_bert_ousih, type = "text", out = paste0(outputpath, "model_valence_4emotion_bert_ousih.txt"))

# Models for valence and agency and bert_ousib their interaction on the 4 emotions
ps_va_bert_ousib_noint <- lmer(pos_self ~ valence + agency + bert_ousib + (1|participant_ID), data = df)
ps_va_bert_ousib_int <- lmer(pos_self ~ valence * agency * bert_ousib + (1|participant_ID), data = df)
pn_va_bert_ousib_noint <- lmer(neg_self ~ valence + agency + bert_ousib + (1|participant_ID), data = df)
pn_va_bert_ousib_int <- lmer(neg_self ~ valence * agency * bert_ousib + (1|participant_ID), data = df)
po_va_bert_ousib_noint <- lmer(pos_other ~ valence + agency + bert_ousib + (1|participant_ID), data = df)
po_va_bert_ousib_int <- lmer(pos_other ~ valence * agency * bert_ousib + (1|participant_ID), data = df)
nn_va_bert_ousib_noint <- lmer(neg_other ~ valence + agency + bert_ousib + (1|participant_ID), data = df)
nn_va_bert_ousib_int <- lmer(neg_other ~ valence * agency * bert_ousib + (1|participant_ID), data = df)

model_valence_4emotion_bert_ousib <- list(ps_va_bert_ousib_noint, ps_va_bert_ousib_int, pn_va_bert_ousib_noint, pn_va_bert_ousib_int, po_va_bert_ousib_noint, po_va_bert_ousib_int, nn_va_bert_ousib_noint, nn_va_bert_ousib_int)
stargazer(model_valence_4emotion_bert_ousib, type = "text", out = paste0(outputpath, "model_valence_4emotion_bert_ousib.txt"))

# Models for valence and agency and bert_guilt_nbe and their interaction on the 4 emotions
ps_va_bert_guilt_nbe_noint <- lmer(pos_self ~ valence + agency + bert_guilt_nbe + (1|participant_ID), data = df)
ps_va_bert_guilt_nbe_int <- lmer(pos_self ~ valence * agency * bert_guilt_nbe + (1|participant_ID), data = df)
pn_va_bert_guilt_nbe_noint <- lmer(neg_self ~ valence + agency + bert_guilt_nbe + (1|participant_ID), data = df)
pn_va_bert_guilt_nbe_int <- lmer(neg_self ~ valence * agency * bert_guilt_nbe + (1|participant_ID), data = df)
po_va_bert_guilt_nbe_noint <- lmer(pos_other ~ valence + agency + bert_guilt_nbe + (1|participant_ID), data = df)
po_va_bert_guilt_nbe_int <- lmer(pos_other ~ valence * agency * bert_guilt_nbe + (1|participant_ID), data = df)
nn_va_bert_guilt_nbe_noint <- lmer(neg_other ~ valence + agency + bert_guilt_nbe + (1|participant_ID), data = df)
nn_va_bert_guilt_nbe_int <- lmer(neg_other ~ valence * agency * bert_guilt_nbe + (1|participant_ID), data = df)

model_valence_4emotion_bert_guilt_nbe <- list(ps_va_bert_guilt_nbe_noint, ps_va_bert_guilt_nbe_int, pn_va_bert_guilt_nbe_noint, pn_va_bert_guilt_nbe_int, po_va_bert_guilt_nbe_noint, po_va_bert_guilt_nbe_int, nn_va_bert_guilt_nbe_noint, nn_va_bert_guilt_nbe_int)
stargazer(model_valence_4emotion_bert_guilt_nbe, type = "text", out = paste0(outputpath, "model_valence_4emotion_bert_guilt_nbe.txt"))

# Models for valence and agency and bert_guilt_repair and their interaction on the 4 emotions
ps_va_bert_guilt_repair_noint <- lmer(pos_self ~ valence + agency + bert_guilt_repair + (1|participant_ID), data = df)
ps_va_bert_guilt_repair_int <- lmer(pos_self ~ valence * agency * bert_guilt_repair + (1|participant_ID), data = df)
pn_va_bert_guilt_repair_noint <- lmer(neg_self ~ valence + agency + bert_guilt_repair + (1|participant_ID), data = df)
pn_va_bert_guilt_repair_int <- lmer(neg_self ~ valence * agency * bert_guilt_repair + (1|participant_ID), data = df)
po_va_bert_guilt_repair_noint <- lmer(pos_other ~ valence + agency + bert_guilt_repair + (1|participant_ID), data = df)
po_va_bert_guilt_repair_int <- lmer(pos_other ~ valence * agency * bert_guilt_repair + (1|participant_ID), data = df)
nn_va_bert_guilt_repair_noint <- lmer(neg_other ~ valence + agency + bert_guilt_repair + (1|participant_ID), data = df)
nn_va_bert_guilt_repair_int <- lmer(neg_other ~ valence * agency * bert_guilt_repair + (1|participant_ID), data = df)

model_valence_4emotion_bert_guilt_repair <- list(ps_va_bert_guilt_repair_noint, ps_va_bert_guilt_repair_int, pn_va_bert_guilt_repair_noint, pn_va_bert_guilt_repair_int, po_va_bert_guilt_repair_noint, po_va_bert_guilt_repair_int, nn_va_bert_guilt_repair_noint, nn_va_bert_guilt_repair_int)
stargazer(model_valence_4emotion_bert_guilt_repair, type = "text", out = paste0(outputpath, "model_valence_4emotion_bert_guilt_repair.txt"))

# Models for valence and agency and bert_shame_nse and their interaction on the 4 emotions
ps_va_bert_shame_nse_noint <- lmer(pos_self ~ valence + agency + bert_shame_nse + (1|participant_ID), data = df)
ps_va_bert_shame_nse_int <- lmer(pos_self ~ valence * agency * bert_shame_nse + (1|participant_ID), data = df)
pn_va_bert_shame_nse_noint <- lmer(neg_self ~ valence + agency + bert_shame_nse + (1|participant_ID), data = df)
pn_va_bert_shame_nse_int <- lmer(neg_self ~ valence * agency * bert_shame_nse + (1|participant_ID), data = df)
po_va_bert_shame_nse_noint <- lmer(pos_other ~ valence + agency + bert_shame_nse + (1|participant_ID), data = df)
po_va_bert_shame_nse_int <- lmer(pos_other ~ valence * agency * bert_shame_nse + (1|participant_ID), data = df)
nn_va_bert_shame_nse_noint <- lmer(neg_other ~ valence + agency + bert_shame_nse + (1|participant_ID), data = df)
nn_va_bert_shame_nse_int <- lmer(neg_other ~ valence * agency * bert_shame_nse + (1|participant_ID), data = df)

model_valence_4emotion_bert_shame_nse <- list(ps_va_bert_shame_nse_noint, ps_va_bert_shame_nse_int, pn_va_bert_shame_nse_noint, pn_va_bert_shame_nse_int, po_va_bert_shame_nse_noint, po_va_bert_shame_nse_int, nn_va_bert_shame_nse_noint, nn_va_bert_shame_nse_int)
stargazer(model_valence_4emotion_bert_shame_nse, type = "text", out = paste0(outputpath, "model_valence_4emotion_bert_shame_nse.txt"))

# Models for valence and agency and bert_shame_withdraw and their interaction on the 4 emotions
ps_va_bert_shame_withdraw_noint <- lmer(pos_self ~ valence + agency + bert_shame_withdraw + (1|participant_ID), data = df)
ps_va_bert_shame_withdraw_int <- lmer(pos_self ~ valence * agency * bert_shame_withdraw + (1|participant_ID), data = df)
pn_va_bert_shame_withdraw_noint <- lmer(neg_self ~ valence + agency + bert_shame_withdraw + (1|participant_ID), data = df)
pn_va_bert_shame_withdraw_int <- lmer(neg_self ~ valence * agency * bert_shame_withdraw + (1|participant_ID), data = df)
po_va_bert_shame_withdraw_noint <- lmer(pos_other ~ valence + agency + bert_shame_withdraw + (1|participant_ID), data = df)
po_va_bert_shame_withdraw_int <- lmer(pos_other ~ valence * agency * bert_shame_withdraw + (1|participant_ID), data = df)
nn_va_bert_shame_withdraw_noint <- lmer(neg_other ~ valence + agency + bert_shame_withdraw + (1|participant_ID), data = df)
nn_va_bert_shame_withdraw_int <- lmer(neg_other ~ valence * agency * bert_shame_withdraw + (1|participant_ID), data = df)

model_valence_4emotion_bert_shame_withdraw <- list(ps_va_bert_shame_withdraw_noint, ps_va_bert_shame_withdraw_int, pn_va_bert_shame_withdraw_noint, pn_va_bert_shame_withdraw_int, po_va_bert_shame_withdraw_noint, po_va_bert_shame_withdraw_int, nn_va_bert_shame_withdraw_noint, nn_va_bert_shame_withdraw_int)
stargazer(model_valence_4emotion_bert_shame_withdraw, type = "text", out = paste0(outputpath, "model_valence_4emotion_bert_shame_withdraw.txt"))

# Models for rightness and agency and OUS and their interaction on the 4 emotions
ps_ra_ousib_noint <- lmer(pos_self ~ rightness + agency + OUS_IB + (1|participant_ID), data = df)
ps_ra_ousib_int <- lmer(pos_self ~ rightness * agency * OUS_IB + (1|participant_ID), data = df)
pn_ra_ousib_noint <- lmer(neg_self ~ rightness + agency + OUS_IB + (1|participant_ID), data = df)
pn_ra_ousib_int <- lmer(neg_self ~ rightness * agency * OUS_IB + (1|participant_ID), data = df)
po_ra_ousib_noint <- lmer(pos_other ~ rightness + agency + OUS_IB + (1|participant_ID), data = df)
po_ra_ousib_int <- lmer(pos_other ~ rightness * agency * OUS_IB + (1|participant_ID), data = df)
nn_ra_ousib_noint <- lmer(neg_other ~ rightness + agency + OUS_IB + (1|participant_ID), data = df) 
nn_ra_ousib_int <- lmer(neg_other ~ rightness * agency * OUS_IB + (1|participant_ID), data = df)

model4emotion_ousib <- list(ps_ra_ousib_noint, ps_ra_ousib_int, pn_ra_ousib_noint, pn_ra_ousib_int, po_ra_ousib_noint, po_ra_ousib_int, nn_ra_ousib_noint, nn_ra_ousib_int)
stargazer(model4emotion_ousib, type = "text", out = paste0(outputpath, "model4emotion_ousib.txt"))

# Models for rightness and agency and OUS_IH and their interaction on the 4 emotions
ps_ra_ousih_noint <- lmer(pos_self ~ rightness + agency + OUS_IH + (1|participant_ID), data = df)
ps_ra_ousih_int <- lmer(pos_self ~ rightness * agency * OUS_IH + (1|participant_ID), data = df)
pn_ra_ousih_noint <- lmer(neg_self ~ rightness + agency + OUS_IH + (1|participant_ID), data = df)
pn_ra_ousih_int <- lmer(neg_self ~ rightness * agency * OUS_IH + (1|participant_ID), data = df)
po_ra_ousih_noint <- lmer(pos_other ~ rightness + agency + OUS_IH + (1|participant_ID), data = df)
po_ra_ousih_int <- lmer(pos_other ~ rightness * agency * OUS_IH + (1|participant_ID), data = df)
nn_ra_ousih_noint <- lmer(neg_other ~ rightness + agency + OUS_IH + (1|participant_ID), data = df)
nn_ra_ousih_int <- lmer(neg_other ~ rightness * agency * OUS_IH + (1|participant_ID), data = df)

model4emotion_ousih <- list(ps_ra_ousih_noint, ps_ra_ousih_int, pn_ra_ousih_noint, pn_ra_ousih_int, po_ra_ousih_noint, po_ra_ousih_int, nn_ra_ousih_noint, nn_ra_ousih_int)
stargazer(model4emotion_ousih, type = "text", out = paste0(outputpath, "model4emotion_ousih.txt"))

