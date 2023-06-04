library(readxl)
script.dir <- dirname(sys.frame(1)$ofile)
setwd("..")
d <- read_excel("./0_data/combined_groups_alldata_LU_update.xlsx",sheet = "complete")

dfintake <- read_excel("./0_data/combined_groups_alldata_LU_update.xlsx", sheet = "no empty entries")


dmoral <- read.csv("./0_data/LIWC-22 Results - EA_ema - moral.csv")
dimmoral <- read.csv(".0_/data/LIWC-22 Results - EA_ema - immoral.csv")
dneutral <- read.csv("./0_data/LIWC-22 Results - EA_ema - neutral.csv")

dmoral[dmoral$WC == 0, names(dmoral)] <- NA
dimmoral[dimmoral$WC == 0, names(dimmoral)] <- NA
dneutral[dneutral$WC == 0, names(dneutral)] <- NA
names(dmoral) <- paste0("t_moral_", names(dmoral))
names(dimmoral) <- paste0("t_immoral_", names(dimmoral))
names(dneutral) <- paste0("t_neutral_", names(dneutral))

dliwc <- cbind(dmoral,dimmoral,dneutral)
