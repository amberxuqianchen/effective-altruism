outputpath <- '/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/3_output/GASP_RightAgency_HLM/'

df <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Github/effective-altruism/2_pipeline/out/df.csv')

dfbert <- read.csv('/home/local/PSYCH-ADS/xuqian_chen/Projects/BERT/ccr/dfbert.csv')
# add prefix to column names
colnames(dfbert) <- paste0("bert_", colnames(dfbert))
# convert to int
dfbert$bert_sig_order <- as.integer(dfbert$bert_sig_order)
df$sig_order <- as.integer(df$sig_order)
# merge based on sig_order
df <- merge(df, dfbert, by.x = "sig_order", by.y = "bert_sig_order", all = FALSE)

################# Descriptive statistics #################
# Descriptive statistics for bert score on valence x agency


df$moral_agency <- as.factor(df$moral_agency)
levels(df$moral_agency) <- c("Self", "Other")
df$moral_valence <- as.factor(df$moral_valence)
levels(df$moral_valence) <- c("Moral", "Immoral")

# "bert_guilt_nbe" mean table on factor moral_valence x moral agency
df_gnbe <- aggregate(df$bert_guilt_nbe, by = list(df$moral_valence, df$moral_agency), FUN = mean)
# # Reshape the data frame
gnbe_table <- reshape(df_gnbe, idvar = "Group.1", timevar = "Group.2", direction = "wide")
# # Rename the columns
colnames(gnbe_table) <- c("Guilt NBE", "Self", "Other")

# "bert_guilt_repair" mean table on factor moral_valence x moral agency
df_gr <- aggregate(df$bert_guilt_repair, by = list(df$moral_valence, df$moral_agency), FUN = mean)
# # Reshape the data frame
gr_table <- reshape(df_gr, idvar = "Group.1", timevar = "Group.2", direction = "wide")
# # Rename the columns
colnames(gr_table) <- c("Guilt Repair", "Self", "Other")

# "bert_shame_nse" mean table on factor moral_valence x moral agency
df_snse <- aggregate(df$bert_shame_nse, by = list(df$moral_valence, df$moral_agency), FUN = mean)
# # Reshape the data frame
snse_table <- reshape(df_snse, idvar = "Group.1", timevar = "Group.2", direction = "wide")
# # Rename the columns
colnames(snse_table) <- c("Shame NSE", "Self", "Other")

# "bert_shame_withdraw" mean table on factor moral_valence x moral agency
df_sw <- aggregate(df$bert_shame_withdraw, by = list(df$moral_valence, df$moral_agency), FUN = mean)
# # Reshape the data frame
sw_table <- reshape(df_sw, idvar = "Group.1", timevar = "Group.2", direction = "wide")
# # Rename the columns
colnames(sw_table) <- c("Shame Withdraw", "Self", "Other")

# "bert_ousib" mean table on factor moral_valence x moral agency
df_ousib <- aggregate(df$bert_ousib, by = list(df$moral_valence, df$moral_agency), FUN = mean)
# # Reshape the data frame
ousib_table <- reshape(df_ousib, idvar = "Group.1", timevar = "Group.2", direction = "wide")
# # Rename the columns
colnames(ousib_table) <- c("OUS IB", "Self", "Other")

# "bert_ousih" mean table on factor moral_valence x moral agency
df_ousih <- aggregate(df$bert_ousih, by = list(df$moral_valence, df$moral_agency), FUN = mean)
# # Reshape the data frame
ousih_table <- reshape(df_ousih, idvar = "Group.1", timevar = "Group.2", direction = "wide")
# # Rename the columns
colnames(ousih_table) <- c("OUS IH", "Self", "Other")

# # Combine the tables into a list
reshaped_tables <- list(gnbe_table, gr_table, snse_table, sw_table, ousib_table, ousih_table)
# print the reshaped tables 
print(reshaped_tables)


################# Descriptive plots #################
library(ggplot2)
ggplot(df, aes(x = moral_valence, y = bert_guilt_nbe, fill = moral_agency)) +
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "Guilt NBE", title = "Guilt NBE")+
    theme_bw()
ggsave(paste0(outputpath, "Guilt_NBE.png"), width = 6, height = 6, dpi = 300)

ggplot(df, aes(x = moral_valence, y = bert_guilt_repair, fill = moral_agency)) +
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "Guilt Repair", title = "Guilt Repair")+
    theme_bw()
ggsave(paste0(outputpath, "Guilt_Repair.png"), width = 6, height = 6, dpi = 300)

ggplot(df, aes(x = moral_valence, y = bert_shame_nse, fill = moral_agency)) +
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "Shame NSE", title = "Shame NSE")+
    theme_bw()
ggsave(paste0(outputpath, "Shame_NSE.png"), width = 6, height = 6, dpi = 300)

ggplot(df, aes(x = moral_valence, y = bert_shame_withdraw, fill = moral_agency)) +
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "Shame Withdraw", title = "Shame Withdraw")+
    theme_bw()
ggsave(paste0(outputpath, "Shame_Withdraw.png"), width = 6, height = 6, dpi = 300)

ggplot(df, aes(x = moral_valence, y = bert_ousib, fill = moral_agency)) +
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "OUS IB", title = "OUS IB_bert")+
    theme_bw()
ggsave(paste0(outputpath, "OUS_IB_bert.png"), width = 6, height = 6, dpi = 300)

ggplot(df, aes(x = moral_valence, y = bert_ousih, fill = moral_agency)) +
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "OUS IH", title = "OUS IH_bert")+
    theme_bw()
ggsave(paste0(outputpath, "OUS_IH_bert.png"), width = 6, height = 6, dpi = 300)

ggplot(df, aes(x = moral_valence, y = OUS_IB, fill = moral_agency)) +
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "OUS IB", title = "OUS IB")+
    theme_bw()
ggsave(paste0(outputpath, "OUS_IB.png"), width = 6, height = 6, dpi = 300)

ggplot(df, aes(x = moral_valence, y = OUS_IH, fill = moral_agency)) +
    stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.90),width=.2)+
    labs(x = "Moral Valence", y = "OUS IH", title = "OUS IH")+
    theme_bw()
ggsave(paste0(outputpath, "OUS_IH.png"), width = 6, height = 6, dpi = 300)
