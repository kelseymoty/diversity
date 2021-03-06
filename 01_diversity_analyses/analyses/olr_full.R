## Ordinal logistic regression on full dataset comparing condition by age 
# Ordinal logistic regression using Wald Statistic for comparison
# Testing for the effect of age_exact and condition on response choice
results_clmm <- clmm(response ~ condition + age_factor + age_factor:condition + (1|id)  + (1|item), 
                     data = dt2, 
                     link = "logit", 
                     Hess = TRUE, 
                     threshold = "flexible")
summary(results_clmm)

# Summary() uses the Wald statistic. A more accurate measure of significance is to use likelihood ratio
# To use LR, need to compare models with and without paramter of interest

# Testing if interaction effect is significant
ordinal_full_int <- drop1(results_clmm, test = "Chi")

# Testing if random effects are significant 
# testing if random effect of participant is significant
results_clmm_fixed <- clmm(response ~ condition + age_factor + age_factor:condition + (1|item), 
                           data = dt2, 
                           link = "logit", 
                           Hess = TRUE, 
                           threshold = "flexible")
anova(results_clmm, results_clmm_fixed)

# testing if random effect of item is significant
results_clmm_fixed1 <- clmm(response ~ condition + age_factor + age_factor:condition + (1|id), 
                            data = dt2, 
                            link = "logit", 
                            Hess = TRUE, 
                            threshold = "flexible")
anova(results_clmm, results_clmm_fixed1)

# Testing if main effects are signficant 
results_clmm_nointeract <- clmm(response ~ condition + age_factor + (1|id) + (1|item), 
                                data = dt2, 
                                link = "logit", 
                                Hess = TRUE, 
                                threshold = "flexible")

ordinal_full_main <- drop1(results_clmm_nointeract, test = "Chi")



