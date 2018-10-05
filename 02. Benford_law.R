
# Benford's Law Analyses

library('benford.analysis')
library(dplyr)
library(ggplot2)

setwd()
df = read.csv('cleaned_compu.csv')

# Beford Law first digit analyses on [Revenue, Operating Cash Flow and Total Asset]
# Revenue
revenue_bf <- data.frame(benford(abs(df$revt), number.of.digits = 1, 
                                 sign = 'positive', discrete = TRUE, round = 3)$bfd)[,c("digits","data.dist", "benford.dist")]

colnames(revenue_bf) <- c('Digit', 'Sample_Revenue', 'Benford_Distribution')

ggplot(data = revenue_bf, aes(x = as.factor(Digit), y =Benford_Distribution)) +
  geom_bar(stat='identity') + 
  geom_line(aes(Digit, Sample_Revenue, col = 'Sample_Revenue'), linetype = 1) + 
  geom_point(aes(Digit, Sample_Revenue), size = 4, col = 'red')+
  ggtitle('Theoretical Distribution v.s. Sample Revenue Distribution - 1st Digits ')

# Operating Cash Flow
cash_bf <- data.frame(benford(abs(df1n$oancf), number.of.digits = 1, 
                              sign = 'positive', discrete = TRUE, round = 3)$bfd)[,c("digits","data.dist", "benford.dist")]

colnames(cash_bf) <- c('Digit', 'Sample_Cashflow', 'Benford_Distribution')

ggplot(data = cash_bf, aes(x = as.factor(Digit), y =Benford_Distribution)) +
  geom_bar(stat='identity') + 
  geom_line(aes(Digit, Sample_Cashflow, col = 'Sample_Cashflow'), linetype = 1) + 
  geom_point(aes(Digit, Sample_Cashflow), size = 4, col = 'red')+
  ggtitle('Theoretical Distribution v.s. Sample Cash Flow Distribution - 1st Digits ')

# Total Asset
asset_bf <- data.frame(benford(abs(df1n$at), number.of.digits = 1, 
                               sign = 'positive', discrete = TRUE, round = 3)$bfd)[,c("digits","data.dist", "benford.dist")]

colnames(asset_bf) <- c('Digit', 'Sample_Asset', 'Benford_Distribution')

ggplot(data = asset_bf, aes(x = as.factor(Digit), y =Benford_Distribution)) +
  geom_bar(stat='identity') + 
  geom_line(aes(Digit, Sample_Asset, col = 'Sample_Asset'), linetype = 1) + 
  geom_point(aes(Digit, Sample_Asset), size = 4, col = 'red')+
  ggtitle('Theoretical Distribution v.s. Sample TTL Asset Distribution - 1st Digits ')

# Chi square and p-value
c_revenue <- chisq(benford(abs(df$revt), number.of.digits = 1, sign = 'positive', discrete = TRUE, round = 3))$statistic['X-squared']
c_cash <- chisq(benford(abs(df$oancf), number.of.digits = 1, sign = 'positive', discrete = TRUE, round = 3))$statistic['X-squared']
c_asset <- chisq(benford(abs(df$at), number.of.digits = 1, sign = 'positive', discrete = TRUE, round = 3))$statistic['X-squared']

p_revenue <- chisq(benford(abs(df$revt), number.of.digits = 1, sign = 'positive', discrete = TRUE, round = 3))$p.value
p_cash <- chisq(benford(abs(df$oancf), number.of.digits = 1, sign = 'positive', discrete = TRUE, round = 3))$p.value
p_asset <- chisq(benford(abs(df$at), number.of.digits = 1, sign = 'positive', discrete = TRUE, round = 3))$p.value

chi <- data.frame(c_revenue, c_cash, c_asset)
p_value <- data.frame(p_revenue, p_cash, p_asset)

chi1 <- t(chi)
p_value1 <- t(p_value)
colnames(p_value1) <- 'p_value'
chi_test <- cbind(chi1, p_value1)
chi_test

# Beford Law second digit analyses on [Revenue, Operating Cash Flow and Total Asset]

# Theoretical Probability of Benford Law (Second Digit)
d2_prob <- sapply(0:9, p.this.digit.at.n, n = 2)
d2_prob2<- data.frame(d2_prob)

# Revenue
two_digit_rev <- extract.digits(abs(df$revt), number.of.digits = 2)['data.digits']
revenue_bf2 = two_digit_rev %% 10
revenue_bf3 <-revenue_bf2 %>% group_by(data.digits) %>% summarise(count = n())
revenue_bf3$Percentage <- revenue_bf3$count / sum(revenue_bf3$count)

revenue_bf4 <- cbind(revenue_bf3, d2_prob2)
colnames(revenue_bf4) <- c('Digit','Count' ,'Sample_Revenue', 'Benford_Distribution')

revenue_bf4$Digit <- as.factor(revenue_bf4$Digit)

ggplot(data = revenue_bf4, aes(x = Digit, y =Benford_Distribution)) +
  geom_bar(stat='identity') + 
  geom_line(aes(Digit, Sample_Revenue, col = 'Sample_Revenue', group = 1), linetype = 1, stat = 'identity')+ 
  geom_point(aes(Digit, Sample_Revenue), size = 4, col = 'red')+
  ggtitle('Theoretical Distribution v.s. Sample Revenue Distribution - 2nd Digits ')

# Operating Cash Flow
two_digit_cash <- extract.digits(abs(df$oancf), number.of.digits = 2)['data.digits']
cash_bf2 <- two_digit_cash %% 10
cash_bf3 <- cash_bf2 %>% group_by(data.digits) %>% summarise(count = n())
cash_bf3$Percentage <- cash_bf3$count / sum(cash_bf3$count)
cash_bf4 <- cbind(cash_bf3, d2_prob2)
colnames(cash_bf4) <- c('Digit','Count' ,'Sample_Cash', 'Benford_Distribution')

cash_bf4$Digit <- as.factor(cash_bf4$Digit)

ggplot(data = cash_bf4, aes(x = Digit, y =Benford_Distribution)) +
  geom_bar(stat='identity') + 
  geom_line(aes(Digit, Sample_Cash, col = 'Sample_Cash', group = 1), linetype = 1, stat = 'identity')+ 
  geom_point(aes(Digit, Sample_Cash), size = 4, col = 'red')+
  ggtitle('Theoretical Distribution v.s. Sample Cash Flow Distribution - 2nd Digits ')

# Total Asset
two_digit_at <- extract.digits(abs(df$at), number.of.digits = 2)['data.digits']
at_bf2 <- two_digit_at %% 10
at_bf3 <- at_bf2 %>% group_by(data.digits) %>% summarise(count = n())
at_bf3$Percentage <- at_bf3$count / sum(at_bf3$count)
at_bf4 <- cbind(at_bf3, d2_prob2)
colnames(at_bf4) <- c('Digit','Count' ,'Sample_TotalAsset', 'Benford_Distribution')

at_bf4$Digit <- as.factor(at_bf4$Digit)

ggplot(data = at_bf4, aes(x = Digit, y =Benford_Distribution)) +
  geom_bar(stat='identity') + 
  geom_line(aes(Digit, Sample_TotalAsset, col = 'Sample_TotalAsset', group = 1), linetype = 1, stat = 'identity')+ 
  geom_point(aes(Digit, Sample_TotalAsset), size = 4, col = 'red')+
  ggtitle('Theoretical Distribution v.s. Sample TTL Asset Distribution - 2nd Digits ')


# Chi square and p-value
c_rev2 <- chisq.test(revenue_bf4$Count, p = revenue_bf4$Benford_Distribution)$statistic['X-squared']
c_cash2 <- chisq.test(cash_bf4$Count, p = cash_bf4$Benford_Distribution)$statistic['X-squared']
c_at2 <- chisq.test(at_bf4$Count, p = at_bf4$Benford_Distribution)$statistic['X-squared']

p_rev2 <- chisq.test(revenue_bf4$Count, p = revenue_bf4$Benford_Distribution)$p.value
p_cash2 <- chisq.test(cash_bf4$Count, p = cash_bf4$Benford_Distribution)$p.value
p_at2 <- chisq.test(at_bf4$Count, p = at_bf4$Benford_Distribution)$p.value

chi2 <- data.frame(c_rev2, c_cash2, c_at2)
p_value2 <- data.frame(p_rev2, p_cash2, p_at2)

chi3 <- t(chi2)
p_value3 <- t(p_value2)
colnames(p_value3) <- 'p_value'
chi_test2 <- cbind(chi3, p_value3)
chi_test2