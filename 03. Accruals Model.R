
library(dplyr)
library(ggplot2)
library(plm)

setwd()
df = read.csv('cleaned_compu.csv')

# Discretionary Accruals Model
accrual_df <- df %>%
  group_by(gvkey) %>%
  mutate(at_lag1 = dplyr::lag(at, k=1),
         rev_lag1 = dplyr::lag(revt, k=1),
         rec_lag1 = dplyr::lag(rect, k=1),
         oancf_lag1 = dplyr::lag(oancf, k=1)) %>%
  filter(at_lag1 != 0) %>% 
  mutate(accrual = (ni - oancf)/at_lag1, cash_revenue = (rev_lag1 - rec_lag1)/at_lag1,
         ppe = ppegt / at_lag1)

#sample_accru <- head(accrual_df, 1000)
fixed_effect.accrual <- plm(accrual ~ cash_revenue + ppe + sic, data= accrual_df,
                            index = c('sic'),
                            model = 'within')

summary(fixed_effect.accrual)

accrual_df$residual <- fixed_effect.accrual$residuals
accrual_df$residual <- abs(accrual_df$residual)
