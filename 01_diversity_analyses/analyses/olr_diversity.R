## ANALYSES USING SUBSET IN DIVERSITY CONDITION ONLY ----
# Both adult and child data
# Make a subset of the data for the diversity condition for effect of experimenter choice
dt2_div <- dt2 %>%
  filter(condition == "diversity") %>% 
  arrange(id)

dt2_div$exp_choice <- factor(dt2_div$exp_choice)
dt2_div$age_factor <- as.factor(dt2_div$age_factor)

# Ordinal logistic regression using Wald Statistic for comparison
results_clmm_div <- clmm(response ~ exp_choice + age_factor + age_factor:exp_choice + (1|id) + (1|item), 
                         data = dt2_div, 
                         link = "logit", 
                         Hess = TRUE, 
                         threshold = "flexible")
summary(results_clmm_div)

# Likelihood ratio tests
# Computing for interaction effect
diversity_int <- drop1(results_clmm_div, test = "Chi")

# Testing of random effect is significant 
# testing if random effect of participant is significant
results_clmm_div_fixed <- clmm(response ~ exp_choice + age_factor + age_factor:exp_choice + (1|item), 
                               data = dt2_div, 
                               link = "logit", 
                               Hess = TRUE, 
                               threshold = "flexible")
anova(results_clmm_div, results_clmm_div_fixed)

# testing if random effect of item is significant
results_clmm_div_fixed1 <- clmm(response ~ exp_choice + age_factor + age_factor:exp_choice + (1|id), 
                                data = dt2_div, 
                                link = "logit", 
                                Hess = TRUE, 
                                threshold = "flexible")
anova(results_clmm_div, results_clmm_div_fixed1)

# Computing for main effects
results_clmm_div_nointeract <- clmm(response ~ exp_choice + age_factor + (1|id) + (1|item), 
                                    data = dt2_div, 
                                    link = "logit", 
                                    Hess = TRUE, 
                                    threshold = "flexible")
diversity_main <- drop1(results_clmm_div_nointeract, test = "Chi")

# Pairwise comparison of age groups 
diversity_pairwise <- summary(lsmeans(results_clmm_div, 
                                      pairwise~age_factor, 
                                      adjust="sidak", 
                                      mode = "mean.class"))