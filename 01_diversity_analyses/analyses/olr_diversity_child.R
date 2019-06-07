### Analyses with just children's data, with age as a continuous variable
dt2_div_c <- dt2_div %>% 
  filter(age_factor != "adult")

# Ordinal logistic regression using Wald Statistic for comparison
results_clmm_div_c <- clmm(response ~ exp_choice + age_exact + age_exact:exp_choice + (1|id) + (1|item), 
                           data = dt2_div_c, 
                           link = "logit", 
                           Hess = TRUE, 
                           threshold = "flexible")
summary(results_clmm_div_c)

# Likelihood ratio tests
# Computing for interaction effect
diversity_int_children <- drop1(results_clmm_div_c, test = "Chi")

# Testing of random effect is significant 
# testing if random effect of participant is significant
results_clmm_div_fixed_c <- clmm(response ~ exp_choice + age_exact + age_exact:exp_choice + (1|item), 
                                 data = dt2_div_c, 
                                 link = "logit", 
                                 Hess = TRUE, 
                                 threshold = "flexible")
anova(results_clmm_div_c, results_clmm_div_fixed_c)

# testing if random effect of item is significant
results_clmm_div_fixed1_c <- clmm(response ~ exp_choice + age_exact + age_exact:exp_choice + (1|id), 
                                  data = dt2_div_c, 
                                  link = "logit", 
                                  Hess = TRUE, 
                                  threshold = "flexible")
anova(results_clmm_div_c, results_clmm_div_fixed1_c)

# Computing for main effects
results_clmm_div_nointeract_c <- clmm(response ~ exp_choice + age_exact + (1|id) + (1|item), 
                                      data = dt2_div_c, 
                                      link = "logit", 
                                      Hess = TRUE, 
                                      threshold = "flexible")
diversity_main_children <- drop1(results_clmm_div_nointeract_c, test = "Chi")