# Generalized Linear Mixed Model (GLMM) with a binomial distribution for binary control data (only kids)

#Subset including only children to test age as a continuous variable
dt_control_binary_child <- dt_control_binary %>%
  filter(adult == "child")

binaryC<-glmer(cbind(response_binary_glmer, 1-response_binary_glmer) ~ age_exact + (1|id) + (1|item), data = dt_control_binary_child, family=binomial)
binaryC

#Likelihood ratio
binaryC_age <- drop1(binaryC, test = "Chi")
binaryC_age

round(binaryC_age[2,4],2)