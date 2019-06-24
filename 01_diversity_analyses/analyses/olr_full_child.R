## Ordinal logistic regression on full dataset comparing condition by age (child data only)
# Ordinal logistic regression using Wald Statistic for comparison
# Testing for the effect of age_exact and condition on response choice
results_clmm_child <- clmm(response ~ condition + age_exact + age_exact:condition + (1|id)  + (1|item), 
                     data = dt2_c, 
                     link = "logit", 
                     Hess = TRUE, 
                     threshold = "flexible")
summary(results_clmm_child)

# Summary() uses the Wald statistic. A more accurate measure of significance is to use likelihood ratio
# To use LR, need to compare models with and without paramter of interest

# Testing if interaction effect is significant
ordinal_full_int_child <- drop1(results_clmm_child, test = "Chi")

# Testing if random effects are significant 
# testing if random effect of participant is significant
results_clmm_fixed_child <- clmm(response ~ condition + age_exact + age_exact:condition + (1|item), 
                           data = dt2_c, 
                           link = "logit", 
                           Hess = TRUE, 
                           threshold = "flexible")
anova(results_clmm_child, results_clmm_fixed_child)

# testing if random effect of item is significant
results_clmm_fixed1_child <- clmm(response ~ condition + age_exact + age_exact:condition + (1|id), 
                            data = dt2_c, 
                            link = "logit", 
                            Hess = TRUE, 
                            threshold = "flexible")
anova(results_clmm_child, results_clmm_fixed1_child)

# Testing if main effects are signficant 
results_clmm_nointeract_child <- clmm(response ~ condition + age_exact + (1|id) + (1|item), 
                                data = dt2_c, 
                                link = "logit", 
                                Hess = TRUE, 
                                threshold = "flexible")

ordinal_full_main_child <- drop1(results_clmm_nointeract_child, test = "Chi")

# Breaking down the interaction effect
# getting main effect of age exact for control (single condition) kids
results_clmm_child_single <- clmm(response ~ age_exact  + (1|id)  + item, 
                           data = dt2_control_c, 
                           link = "logit", 
                           Hess = TRUE, 
                           threshold = "flexible")
single_child <- drop1(results_clmm_child_single, test = "Chi")

# getting main effect of age exact for control (Set condition) kids
results_clmm_child_set <- clmm(response ~ age_exact  + (1|id)  + item, 
                                  data = dt2_div_c, 
                                  link = "logit", 
                                  Hess = TRUE, 
                                  threshold = "flexible")
set_child <- drop1(results_clmm_child_set, test = "Chi")

# computing simple slopes
ordinal_full_child_ss <- emtrends(results_clmm_child, pairwise ~ condition, var="age_exact")

ordinal_full_child_emtrends <- as.tibble(ordinal_full_child_ss$emtrends)
