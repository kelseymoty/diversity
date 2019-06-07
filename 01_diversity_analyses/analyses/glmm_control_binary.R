# Generalized Linear Mixed Model (GLMM) with a binomial distribution for binary control data (both adults and kids)

#recode binary responses (3 = 0, 5 = 1) for binomial model analysis
dt_control_binary <- dt2_control

dt_control_binary$response_binary_glmer <- ifelse(dt_control_binary$response == 1 | dt_control_binary$response == 5, 1, 0)
dt_control_binary$response_binary_glmer <- as.numeric(dt_control_binary$response_binary_glmer)

#results--age is treated categorically here to include adults
binary<-glmer(cbind(response_binary_glmer, 1-response_binary_glmer) ~ age_factor + (1|id) + (1|item), data = dt_control_binary, family=binomial)

#Likelihood ratio
binary_age <- drop1(binary, test = "Chi")
binary_age

round(binary_age[2,4],2)

#marginal means
means_binary<-effect("age_factor", binary)
means_binary<-as.data.frame(means_binary)
means_binary

pairwise_binary <- summary(lsmeans(binary, 
                                   pairwise~age_factor, 
                                   adjust="sidak", 
                                   mode = "mean.class"))
pairwise_binary