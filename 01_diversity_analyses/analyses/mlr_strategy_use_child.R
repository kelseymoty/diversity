# Multinomial logistic regression examining age-related changes in childrem's strategy use

# filtering out just child participants
top_strategies_child <- top_strategies %>% 
  filter(age_factor != "adult")

# making sure strategy is a factor
top_strategies_child$strategy_ll <- as.factor(top_strategies_child$strategy_ll)

# releveling so that extreme strategy is considered baseline strategy in analyses 
top_strategies_child$strategy_ll <- relevel(top_strategies_child$strategy_ll, ref = "extreme")

# multinomial logistic regression 
test <- multinom(strategy_ll ~ age_exact, data = top_strategies_child)
multinom_summary <- summary(test)

# Getting the odds ratios 
odds_ratios <- exp(coef(test))
odds_ratios

# Getting confidence intervals for it 
odds_CI <- as.data.frame(exp(confint(test)))

# Testing whether effect of age on choosing strategies is signficant
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
