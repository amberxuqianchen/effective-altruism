
# Correlation between guilt/shame and OUS_IH/IB in the valence x agency cell.
# moral valence x agency
df_moral_self <- df[df$moral_agency == "Self" & df$moral_valence == "Moral",]
df_moral_other <- df[df$moral_agency == "Other" & df$moral_valence == "Moral",]
df_immoral_self <- df[df$moral_agency == "Self" & df$moral_valence == "Immoral",]
df_immoral_other <- df[df$moral_agency == "Other" & df$moral_valence == "Immoral",]

# Correlation between guilt and OUS_IB in the valence x agency cell.
cor_moral_self <- cor.test(df_moral_self$bert_guilt_nbe, df_moral_self$OUS_IB)
cor_moral_other <- cor.test(df_moral_other$bert_guilt_nbe, df_moral_other$OUS_IB)
cor_immoral_self <- cor.test(df_immoral_self$bert_guilt_nbe, df_immoral_self$OUS_IB)
cor_immoral_other <- cor.test(df_immoral_other$bert_guilt_nbe, df_immoral_other$OUS_IB)

# Create a 2x2 table
cor_table_p <- matrix(c(cor_moral_self$p.value, cor_moral_other$p.value, cor_immoral_self$p.value, cor_immoral_other$p.value),nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("Moral", "Immoral"), c("Self (p-value)", "Other (p-value)")))
cor_table_t <- matrix(c(cor_moral_self$statistic, cor_moral_other$statistic, cor_immoral_self$statistic, cor_immoral_other$statistic),nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("Moral", "Immoral"), c("Self (t-value)", "Other (t-value)")))
# Print the correlation table
print("cor bwt bert_guilt_nbe and OUS_IB")
print(round(cor_table_p, 2))
print(round(cor_table_t, 2))

# Correlation between guilt and OUS_IH in the valence x agency cell.
cor_moral_self <- cor.test(df_moral_self$bert_guilt_nbe, df_moral_self$OUS_IH)
cor_moral_other <- cor.test(df_moral_other$bert_guilt_nbe, df_moral_other$OUS_IH)
cor_immoral_self <- cor.test(df_immoral_self$bert_guilt_nbe, df_immoral_self$OUS_IH)
cor_immoral_other <- cor.test(df_immoral_other$bert_guilt_nbe, df_immoral_other$OUS_IH)
# Create a 2x2 table
cor_table_p <- matrix(c(cor_moral_self$p.value, cor_moral_other$p.value, cor_immoral_self$p.value, cor_immoral_other$p.value),nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("Moral", "Immoral"), c("Self (p-value)", "Other (p-value)")))
# Print the correlation table
print("cor bwt bert_guilt_nbe and OUS_IH")
print(round(cor_table_p, 2))


cor_moral_self <- cor.test(df_moral_self$bert_guilt_repair, df_moral_self$OUS_IB)