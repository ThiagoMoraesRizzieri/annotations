# Libraries needed

library(readxl)
library(trend)
library(fpp2)
library(modifiedmk)
library(writexl)

# Function to create a .pdf file with the graphs

pdf('bowl.pdf')

# Using a modified 'tfpwmk' function from the 'modifiedmk' package to obtain
# the pre whitening time series

source("tfpw_mod.R")

# Reading data and transforming the date variable

bowl_df <- read_excel("bowl.xlsx")
View(bowl_df)

bowl_df$Data <- as.Date(bowl_df$Data, format= "%Y-%m-%d")

# TRMM chart

ggplot(bowl_df,
       aes(x=Data, y = TRMM, group = 1)) +
  geom_line() +
  ggtitle("TRMM from 2000 to 2014")

# Applying the Mann-Kendall test

mk.test(bowl_df$TRMM)

sens.slope(bowl_df$TRMM)

# Creating a data.frame to save the results

teste_mk <- data.frame(matrix(NA, nrow=2,ncol=6))

row.names(teste_mk) <- c('TRMM','TRMM_PW')

colnames(teste_mk) <- c('p_valor','Z','S','Sens slope','VarS','tau')

total <- c(mk.test(ts(as.numeric(bowl_df$TRMM)))$p.value,
           mk.test(ts(as.numeric(bowl_df$TRMM)))$statistic,
           mk.test(ts(as.numeric(bowl_df$TRMM)))$estimates['S'],
           sens.slope(ts(as.numeric(bowl_df$TRMM)))$estimates["Sen's slope"],
           mk.test(ts(as.numeric(bowl_df$TRMM)))$estimates['varS'],
           mk.test(ts(as.numeric(bowl_df$TRMM)))$estimates['tau'])

teste_mk[1,] <- total
View(teste_mk)

# Applying the Trend Free Pre Whitening

tfpw_bowl_df_TRMM <- bowl_df[c(1:179),]

View(tfpw_bowl_df_TRMM)

tfpw_bowl_df_TRMM$TRMM <- tfpw_mod(as.vector(bowl_df$TRMM))

# Visualizing the Pre Whitening time series

ggplot(tfpw_bowl_df_TRMM,
       aes(x=Data, y = TRMM, group = 1)) +
  geom_line() +
  ggtitle("TRMM_pre_whitening from 2000 to 2014")

# Applying and saving the new Mann-Kendall test

mk.test(tfpw_bowl_df_TRMM$TRMM)

tfpwmk(bowl_df$TRMM)

total <- c(tfpwmk(as.vector(bowl_df$TRMM))['P-value'],
           tfpwmk(as.vector(bowl_df$TRMM))['Z-Value'],
           tfpwmk(as.vector(bowl_df$TRMM))['S'],
           tfpwmk(as.vector(bowl_df$TRMM))["Sen's Slope"],
           tfpwmk(as.vector(bowl_df$TRMM))['Var(S)'],
           tfpwmk(as.vector(bowl_df$TRMM))['Tau'])

teste_mk[2,] <- total
View(teste_mk)

# Writing the results in a .xlsx file

write_xlsx(teste_mk,"results.xlsx")

# Closing the .pdf file

dev.off()