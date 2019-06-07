## Analyzing control condition data
# just child data

# separate df of just control data for kids; useful for analyzes where we want to explore age continuously
dt2_control_c <- dt2_control %>% 
  filter(age_factor != "adult")

# Testing with just kid data
results_clmm_div_control_c <- clmm(response ~ age_exact + (1|id) + item, 
                                   data = dt2_control_c, 
                                   link = "logit", 
                                   Hess = TRUE, 
                                   threshold = "flexible")
control_main_children <- drop1(results_clmm_div_control_c , test = "Chi")