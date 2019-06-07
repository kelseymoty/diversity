## Analyzing control condition data
# both children and adults
# creating separate df with just control data
dt2_control <- dt2 %>%
  filter(condition == "control") %>% 
  arrange(id)

# making sure age is set to factor
dt2_control$age_factor <- as.factor(dt2_control$age_factor)

# column dichotomozing kids and adults (if we want to run separate OLR for adult vs kids)
dt2_control$adult <- ifelse(dt2_control$age_factor == "adult", "adult", "child")

# Ordinal logistic regression using Wald Statistic for comparison
results_clmm_div <- clmm(response ~ age_factor + (1|id) + (1|item), 
                         data = dt2_control, 
                         link = "logit", 
                         Hess = TRUE, threshold = "flexible")
summary(results_clmm_div)

# Likelihood ratio tests
# to get the likelihood ratio, you can't in R do it with only one fixed effect. To get around that, the best approximate is to treat one of the random effects as fixed and then do the drop1()
results_clmm_div1 <- clmm(response ~ age_factor + (1|id) + item, 
                          data = dt2_control, 
                          link = "logit", 
                          Hess = TRUE, 
                          threshold = "flexible")
control_main <- drop1(results_clmm_div1, test = "Chi")

# Testing of random effect is significant 
# testing if random effect of participant is significant
results_clmm_div_fixed <- clmm(response ~ age_factor + (1|item), data = dt2_control, 
                               link = "logit", Hess = TRUE, threshold = "flexible")
anova(results_clmm_div, results_clmm_div_fixed)

# testing if random effect of item is significant
results_clmm_div_fixed1 <- clmm(response ~ age_factor + (1|id), data = dt2_control, 
                                link = "logit", Hess = TRUE, threshold = "flexible")
anova(results_clmm_div, results_clmm_div_fixed1)
