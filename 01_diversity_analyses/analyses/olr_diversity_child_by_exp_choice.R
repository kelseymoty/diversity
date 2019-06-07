### Analyses for children's data broken down by experimenter choice
# Separate dfs for child data for each exp_choice
dt2_div_c5 <- dt2_div_c %>% 
  filter(exp_choice == "5")

dt2_div_c4 <- dt2_div_c %>% 
  filter(exp_choice == "4")

dt2_div_c3 <- dt2_div_c %>% 
  filter(exp_choice == "3")

dt2_div_c2 <- dt2_div_c %>% 
  filter(exp_choice == "2")

dt2_div_c1 <- dt2_div_c %>% 
  filter(exp_choice == "1")

# Looking at effect of continuous age on response separately for each experimenter response
# can't do LRT for single fixed effect so putting item as fixed effect instead of random
results_clmm_divc1 <- clm(response ~ age_exact + item, 
                          data = dt2_div_c1, 
                          link = "logit", 
                          Hess = TRUE, 
                          threshold = "flexible")
div_exp1 <- drop1(results_clmm_divc1, test = "Chi")

results_clmm_divc2 <- clm(response ~ age_exact + item, 
                          data = dt2_div_c2, 
                          link = "logit", 
                          Hess = TRUE, 
                          threshold = "flexible")
div_exp2 <- drop1(results_clmm_divc2, test = "Chi")

results_clmm_divc3 <- clm(response ~ age_exact + item, 
                          data = dt2_div_c3, 
                          link = "logit", Hess = TRUE, threshold = "flexible")
div_exp3 <- drop1(results_clmm_divc3, test = "Chi")

results_clmm_divc4 <- clm(response ~ age_exact + item, 
                          data = dt2_div_c4, 
                          link = "logit", 
                          Hess = TRUE, 
                          threshold = "flexible")
div_exp4 <- drop1(results_clmm_divc4, test = "Chi")

results_clmm_divc5 <- clm(response ~ age_exact + item, 
                          data = dt2_div_c5, 
                          link = "logit", 
                          Hess = TRUE, 
                          threshold = "flexible")
div_exp5 <- drop1(results_clmm_divc5, test = "Chi")