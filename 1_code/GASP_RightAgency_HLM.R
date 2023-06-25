library(lme4)
library(stargazer)
library(sjPlot)
library(ggplot2)
df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')
outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/GASP_RightAgency_HLM/'

# make directory for output if not exist
if (!dir.exists(outputpath)) {
  dir.create(outputpath)
}

# make reference category
df$agency <- as.factor(df$moral_agency)
levels(df$agency) <- c("Self", "Other")
df$agency <- relevel(df$agency, ref = "Other")
df$moral_agency <- as.factor(df$moral_agency)
levels(df$moral_agency) <- c("Self", "Other")
df$moral_agency <- relevel(df$moral_agency, ref = "Self")
df$moral_valence <- as.factor(df$moral_valence)
levels(df$moral_valence) <- c("Moral", "Immoral")
df$moral_valence <- relevel(df$moral_valence, ref = "Moral")
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

# Models for valence and agency and OUS_IB on  "bert_guilt_nbe"      "bert_guilt_repair"   "bert_shame_nse"      "bert_shame_withdraw"
gn_ma_ousib_noint <- lmer(bert_guilt_nbe ~ moral_valence + agency + OUS_IB + (1|participant_ID), data = df)
gn_ma_ousib_int <- lmer(bert_guilt_nbe ~ moral_valence * agency * OUS_IB + (1|participant_ID), data = df)
gr_ma_ousib_noint <- lmer(bert_guilt_repair ~ moral_valence + agency + OUS_IB + (1|participant_ID), data = df)
gr_ma_ousib_int <- lmer(bert_guilt_repair ~ moral_valence * agency * OUS_IB + (1|participant_ID), data = df)
sn_ma_ousib_noint <- lmer(bert_shame_nse ~ moral_valence + agency + OUS_IB + (1|participant_ID), data = df)
sn_ma_ousib_int <- lmer(bert_shame_nse ~ moral_valence * agency * OUS_IB + (1|participant_ID), data = df)
sw_ma_ousib_noint <- lmer(bert_shame_withdraw ~ moral_valence + agency + OUS_IB + (1|participant_ID), data = df)
sw_ma_ousib_int <- lmer(bert_shame_withdraw ~ moral_valence * agency * OUS_IB + (1|participant_ID), data = df)

gaspmodel <- c(gn_ma_ousib_noint, gn_ma_ousib_int, gr_ma_ousib_noint, gr_ma_ousib_int, sn_ma_ousib_noint, sn_ma_ousib_int, sw_ma_ousib_noint, sw_ma_ousib_int)
stargazer(gaspmodel,type = "text", out = paste0(outputpath, "gaspmodel_OUSIB.text"), digits=3, no.space = TRUE, header = FALSE, omit.stat = "f")

# Models for valence and agency and OUS_IH on  "bert_guilt_nbe"      "bert_guilt_repair"   "bert_shame_nse"      "bert_shame_withdraw"
gn_ma_ousih_noint <- lmer(bert_guilt_nbe ~ moral_valence + agency + OUS_IH + (1|participant_ID), data = df)
gn_ma_ousih_int <- lmer(bert_guilt_nbe ~ moral_valence * agency * OUS_IH + (1|participant_ID), data = df)
gr_ma_ousih_noint <- lmer(bert_guilt_repair ~ moral_valence + agency + OUS_IH + (1|participant_ID), data = df)
gr_ma_ousih_int <- lmer(bert_guilt_repair ~ moral_valence * agency * OUS_IH + (1|participant_ID), data = df)
sn_ma_ousih_noint <- lmer(bert_shame_nse ~ moral_valence + agency + OUS_IH + (1|participant_ID), data = df)
sn_ma_ousih_int <- lmer(bert_shame_nse ~ moral_valence * agency * OUS_IH + (1|participant_ID), data = df)
sw_ma_ousih_noint <- lmer(bert_shame_withdraw ~ moral_valence + agency + OUS_IH + (1|participant_ID), data = df)
sw_ma_ousih_int <- lmer(bert_shame_withdraw ~ moral_valence * agency * OUS_IH + (1|participant_ID), data = df)

gaspmodel <- c(gn_ma_ousih_noint, gn_ma_ousih_int, gr_ma_ousih_noint, gr_ma_ousih_int, sn_ma_ousih_noint, sn_ma_ousih_int, sw_ma_ousih_noint, sw_ma_ousih_int)
stargazer(gaspmodel,type = "text", out = paste0(outputpath, "gaspmodel_OUSIH.text"), digits=3, no.space = TRUE, header = FALSE, omit.stat = "f")

# Models for rightness and agency on "bert_guilt_nbe"      "bert_guilt_repair"   "bert_shame_nse"      "bert_shame_withdraw"
gn_ra_noint <- lmer(bert_guilt_nbe ~ rightness + agency + (1|participant_ID), data = df)
gn_ra_int <- lmer(bert_guilt_nbe ~ rightness * agency + (1|participant_ID), data = df)
gr_ra_noint <- lmer(bert_guilt_repair ~ rightness + agency + (1|participant_ID), data = df)
gr_ra_int <- lmer(bert_guilt_repair ~ rightness * agency + (1|participant_ID), data = df)
sn_ra_noint <- lmer(bert_shame_nse ~ rightness + agency + (1|participant_ID), data = df)
sn_ra_int <- lmer(bert_shame_nse ~ rightness * agency + (1|participant_ID), data = df)
sw_ra_noint <- lmer(bert_shame_withdraw ~ rightness + agency + (1|participant_ID), data = df)
sw_ra_int <- lmer(bert_shame_withdraw ~ rightness * agency + (1|participant_ID), data = df)

gaspmodel <- c(gn_ra_noint, gn_ra_int, gr_ra_noint, gr_ra_int, sn_ra_noint, sn_ra_int, sw_ra_noint, sw_ra_int)
stargazer(gaspmodel,type = "text", out = paste0(outputpath, "gaspmodel.text"), digits=3, no.space = TRUE, header = FALSE, omit.stat = "f")

# Models for rightness and agency and ousib and their interaction on "bert_guilt_nbe"      "bert_guilt_repair"   "bert_shame_nse"      "bert_shame_withdraw"
gn_ra_ousib_noint <- lmer(bert_guilt_nbe ~ rightness + agency + bert_ousib + (1|participant_ID), data = df)
gn_ra_ousib_int <- lmer(bert_guilt_nbe ~ rightness * agency * bert_ousib + (1|participant_ID), data = df)
gr_ra_ousib_noint <- lmer(bert_guilt_repair ~ rightness + agency + bert_ousib + (1|participant_ID), data = df)
gr_ra_ousib_int <- lmer(bert_guilt_repair ~ rightness * agency * bert_ousib + (1|participant_ID), data = df)
sn_ra_ousib_noint <- lmer(bert_shame_nse ~ rightness + agency + bert_ousib + (1|participant_ID), data = df)
sn_ra_ousib_int <- lmer(bert_shame_nse ~ rightness * agency * bert_ousib + (1|participant_ID), data = df)
sw_ra_ousib_noint <- lmer(bert_shame_withdraw ~ rightness + agency + bert_ousib + (1|participant_ID), data = df)
sw_ra_ousib_int <- lmer(bert_shame_withdraw ~ rightness * agency * bert_ousib + (1|participant_ID), data = df)

gaspmodel_ousib <- c(gn_ra_ousib_noint, gn_ra_ousib_int, gr_ra_ousib_noint, gr_ra_ousib_int, sn_ra_ousib_noint, sn_ra_ousib_int, sw_ra_ousib_noint, sw_ra_ousib_int)
stargazer(gaspmodel_ousib,type = "text", out = paste0(outputpath, "gaspmodel_ousib.text"), digits=3, no.space = TRUE, header = FALSE, omit.stat = "f")

# Models for rightness and agency and ousih and their interaction on "bert_guilt_nbe"      "bert_guilt_repair"   "bert_shame_nse"      "bert_shame_withdraw"
gn_ra_ousih_noint <- lmer(bert_guilt_nbe ~ rightness + agency + bert_ousih + (1|participant_ID), data = df)
gn_ra_ousih_int <- lmer(bert_guilt_nbe ~ rightness * agency * bert_ousih + (1|participant_ID), data = df)
gr_ra_ousih_noint <- lmer(bert_guilt_repair ~ rightness + agency + bert_ousih + (1|participant_ID), data = df)
gr_ra_ousih_int <- lmer(bert_guilt_repair ~ rightness * agency * bert_ousih + (1|participant_ID), data = df)
sn_ra_ousih_noint <- lmer(bert_shame_nse ~ rightness + agency + bert_ousih + (1|participant_ID), data = df)
sn_ra_ousih_int <- lmer(bert_shame_nse ~ rightness * agency * bert_ousih + (1|participant_ID), data = df)
sw_ra_ousih_noint <- lmer(bert_shame_withdraw ~ rightness + agency + bert_ousih + (1|participant_ID), data = df)
sw_ra_ousih_int <- lmer(bert_shame_withdraw ~ rightness * agency * bert_ousih + (1|participant_ID), data = df)

gaspmodel_ousih <- c(gn_ra_ousih_noint, gn_ra_ousih_int, gr_ra_ousih_noint, gr_ra_ousih_int, sn_ra_ousih_noint, sn_ra_ousih_int, sw_ra_ousih_noint, sw_ra_ousih_int)
stargazer(gaspmodel_ousih,type = "text", out = paste0(outputpath, "gaspmodel_ousih.text"), digits=3, no.space = TRUE, header = FALSE, omit.stat = "f")

# Models for rightness and agency and OUS_IB and their interaction on "bert_guilt_nbe"      "bert_guilt_repair"   "bert_shame_nse"      "bert_shame_withdraw"
gn_ra_OUSIB_noint <- lmer(bert_guilt_nbe ~ rightness + agency + OUS_IB + (1|participant_ID), data = df)
gn_ra_OUSIB_int <- lmer(bert_guilt_nbe ~ rightness * agency * OUS_IB + (1|participant_ID), data = df)
gr_ra_OUSIB_noint <- lmer(bert_guilt_repair ~ rightness + agency + OUS_IB + (1|participant_ID), data = df)
gr_ra_OUSIB_int <- lmer(bert_guilt_repair ~ rightness * agency * OUS_IB + (1|participant_ID), data = df)
sn_ra_OUSIB_noint <- lmer(bert_shame_nse ~ rightness + agency + OUS_IB + (1|participant_ID), data = df)
sn_ra_OUSIB_int <- lmer(bert_shame_nse ~ rightness * agency * OUS_IB + (1|participant_ID), data = df)
sw_ra_OUSIB_noint <- lmer(bert_shame_withdraw ~ rightness + agency + OUS_IB + (1|participant_ID), data = df)
sw_ra_OUSIB_int <- lmer(bert_shame_withdraw ~ rightness * agency * OUS_IB + (1|participant_ID), data = df)

gaspmodel_OUSIB <- c(gn_ra_OUSIB_noint, gn_ra_OUSIB_int, gr_ra_OUSIB_noint, gr_ra_OUSIB_int, sn_ra_OUSIB_noint, sn_ra_OUSIB_int, sw_ra_OUSIB_noint, sw_ra_OUSIB_int)
stargazer(gaspmodel_OUSIB,type = "text", out = paste0(outputpath, "gaspmodel_OUSIB.text"), digits=3, no.space = TRUE, header = FALSE, omit.stat = "f")

# Models for rightness and agency and OUS_IH and their interaction on "bert_guilt_nbe"      "bert_guilt_repair"   "bert_shame_nse"      "bert_shame_withdraw"
gn_ra_OUSIH_noint <- lmer(bert_guilt_nbe ~ rightness + agency + OUS_IH + (1|participant_ID), data = df)
gn_ra_OUSIH_int <- lmer(bert_guilt_nbe ~ rightness * agency * OUS_IH + (1|participant_ID), data = df)
gr_ra_OUSIH_noint <- lmer(bert_guilt_repair ~ rightness + agency + OUS_IH + (1|participant_ID), data = df)
gr_ra_OUSIH_int <- lmer(bert_guilt_repair ~ rightness * agency * OUS_IH + (1|participant_ID), data = df)
sn_ra_OUSIH_noint <- lmer(bert_shame_nse ~ rightness + agency + OUS_IH + (1|participant_ID), data = df)
sn_ra_OUSIH_int <- lmer(bert_shame_nse ~ rightness * agency * OUS_IH + (1|participant_ID), data = df)
sw_ra_OUSIH_noint <- lmer(bert_shame_withdraw ~ rightness + agency + OUS_IH + (1|participant_ID), data = df)
sw_ra_OUSIH_int <- lmer(bert_shame_withdraw ~ rightness * agency * OUS_IH + (1|participant_ID), data = df)

gaspmodel_OUSIH <- c(gn_ra_OUSIH_noint, gn_ra_OUSIH_int, gr_ra_OUSIH_noint, gr_ra_OUSIH_int, sn_ra_OUSIH_noint, sn_ra_OUSIH_int, sw_ra_OUSIH_noint, sw_ra_OUSIH_int)
stargazer(gaspmodel_OUSIH,type = "text", out = paste0(outputpath, "gaspmodel_OUSIH.text"), digits=3, no.space = TRUE, header = FALSE, omit.stat = "f")

# Models for moral valence and agency and their interaction on "bert_guilt_nbe"      "bert_guilt_repair"   "bert_shame_nse"      "bert_shame_withdraw"
df$valence <- as.factor(df$moral_valence)
levels(df$valence) <- c("Moral", "Immoral")
df$valence <- relevel(df$valence, ref = "Moral")

gn_mv_ag_noint <- lmer(bert_guilt_nbe ~ valence + agency + (1|participant_ID), data = df)
gn_mv_ag_int <- lmer(bert_guilt_nbe ~ valence * agency + (1|participant_ID), data = df)
gr_mv_ag_noint <- lmer(bert_guilt_repair ~ valence + agency + (1|participant_ID), data = df)
gr_mv_ag_int <- lmer(bert_guilt_repair ~ valence * agency + (1|participant_ID), data = df)
sn_mv_ag_noint <- lmer(bert_shame_nse ~ valence + agency + (1|participant_ID), data = df)
sn_mv_ag_int <- lmer(bert_shame_nse ~ valence * agency + (1|participant_ID), data = df)
sw_mv_ag_noint <- lmer(bert_shame_withdraw ~ valence + agency + (1|participant_ID), data = df)
sw_mv_ag_int <- lmer(bert_shame_withdraw ~ valence * agency + (1|participant_ID), data = df)

gaspmodel_mv_ag <- c(gn_mv_ag_noint, gn_mv_ag_int, gr_mv_ag_noint, gr_mv_ag_int, sn_mv_ag_noint, sn_mv_ag_int, sw_mv_ag_noint, sw_mv_ag_int)
stargazer(gaspmodel_mv_ag,type = "text", out = paste0(outputpath, "gaspmodel_valence_agency.text"), digits=3, no.space = TRUE, header = FALSE, omit.stat = "f")