
## Operating Cashflow manipulation detection

setwd()
df = read.csv('cleaned_compu.csv')

library(magrittr)
library(dplyr)

## Overall Regression 
df$accurals = df$ni - df$oancf

df2 <- df[,c('gvkey','fyear','oancf')]
df3 <- mutate(df2, fyear = fyear - 1) %>%
  rename(., next_oancf = oancf)
df <- left_join(df, df3, by = c('gvkey',"fyear"))

oc_reg <- lm(df$next_oancf ~ df$accurals + df$oancf)
summary(oc_reg)

## Regression before and after 2002
df_88to02 <- df[df['fyear'] <= '2002',]
oc_reg_8802 <- lm(df_88to02$next_oancf ~ df_88to02$accurals + df_88to02$oancf)
summary(oc_reg_8802)

df_03to17 <- df[df['fyear'] > '2002',]
oc_reg_0317 <- lm(df_03to17$next_oancf ~ df_03to17$accurals + df_03to17$oancf)
summary(oc_reg_0317)
