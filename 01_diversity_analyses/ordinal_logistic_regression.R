library(ordinal)
library(ucminf)
library(data.table)
library(plyr)
library(dplyr)
library(lsmeans)
library(emmeans)
library(tidyverse)
library(modelr)
library(ggplot2)
library(dplyr)
library(forcats)
library(lme4)
library(multcomp)
library(knitr)
library(eeptools)
library(lubridate)

# Read in the data file
dt2 <- read_csv("diversity_fulldataset_cleaned.csv") 

# Converting variables to integers 
dt2$exp_choice <- factor(dt2$exp_choice)
dt2$response <- factor(dt2$response)


# --------
# Ordinal logistic regression using Wald Statistic for comparison
# Testing for the effect of age and condition on response choice
results_clmm <- clmm(response ~ condition + age + age:condition + (1|SID)  + (1|item), data = dt2, 
                     link = "logit", Hess = TRUE, threshold = "flexible")
summary(results_clmm)

# Summary() uses the Wald statistic. A more accurate measure of significance is to use likelihood ratio
# To use LR, need to compare models with and without paramter of interest

# Testing if interaction effect is significant
drop1(results_clmm, test = "Chi")

# Testing if random effects are significant 
# testing if random effect of participant is significant
results_clmm_fixed <- clmm(response ~ condition + age + age:condition + (1|item), data = dt2, 
                          link = "logit", Hess = TRUE, threshold = "flexible")
anova(results_clmm, results_clmm_fixed)

# testing if random effect of item is significant
results_clmm_fixed1 <- clmm(response ~ condition + age + age:condition + (1|SID), data = dt2, 
                           link = "logit", Hess = TRUE, threshold = "flexible")
anova(results_clmm, results_clmm_fixed1)

# Testing if main effects are signficant 
results_clmm_nointeract <- clmm(response ~ condition + age + (1|SID) + (1|item), data = dt2, 
                                link = "logit", Hess = TRUE, threshold = "flexible")

drop1(results_clmm_nointeract, test = "Chi")



## ANALYSES USING SUBSET IN DIVERSITY CONDITION ONLY ----
# Make a subset of the data for the diversity condition for effect of experimenter choice
dt2_div <- dt2 %>%
  filter(condition == "diversity")

# Ordinal logistic regression using Wald Statistic for comparison
results_clmm_div <- clmm(response ~ exp_choice + age + age:exp_choice + (1|SID) + (1|item), data = dt2_div, 
                         link = "logit", Hess = TRUE, threshold = "flexible")
summary(results_clmm_div)

# Likelihood ratio tests
# Computing for interaction effect
drop1(results_clmm_div, test = "Chi")

# Testing of random effect is significant 
# testing if random effect of participant is significant
results_clmm_div_fixed <- clmm(response ~ exp_choice + age + age:exp_choice + (1|item), data = dt2_div, 
                              link = "logit", Hess = TRUE, threshold = "flexible")
anova(results_clmm_div, results_clmm_div_fixed)

# testing if random effect of item is significant
results_clmm_div_fixed1 <- clmm(response ~ exp_choice + age + age:exp_choice + (1|SID), data = dt2_div, 
                              link = "logit", Hess = TRUE, threshold = "flexible")
anova(results_clmm_div, results_clmm_div_fixed1)

# Computing for main effects
results_clmm_div_nointeract <- clmm(response ~ exp_choice + age + (1|SID) + (1|item), data = dt2_div, 
                                    link = "logit", Hess = TRUE, threshold = "flexible")
drop1(results_clmm_div_nointeract, test = "Chi")

# Assuming there's an interactiong: teasing apart interaction of exp_choice and age by comparing age groups 
test(pairs(lsmeans(results_clmm_div, ~ age|exp_choice)), adjust = "sidak", joint = TRUE)

div_pairwise <- summary(lsmeans(results_clmm_div, pairwise~age|exp_choice, adjust="sidak", mode = "mean.class"))



# if we want to break apart response within each group
# pairwise comparisons by experimenter choice for each age group
# 5 & 6-year-olds
# dt2_56 <- dt2_div %>%
#   filter(age == "five-six")
# 
# results_clmm_56 <- clmm(response ~ exp_choice + (1|SID), data = dt2_56, 
#                         link = "logit", Hess = TRUE, threshold = "flexible")
# summary(results_clmm_56)
# 
# test(pairs(lsmeans(results_clmm_56, ~ exp_choice)), adjust = "sidak", joint = FALSE)
# lsmeans(results_clmm_56, ~exp_choice, adjust="sidak")
# 
# # 7 & 8-year-olds
# dt2_78 <- dt2_div %>%
#   filter(age == "seven-eight")
# 
# results_clmm_78 <- clmm(response ~ exp_choice + (1|SID), data = dt2_78, 
#                         link = "logit", Hess = TRUE, threshold = "flexible")
# 
# test(pairs(lsmeans(results_clmm_78, ~ exp_choice)), adjust = "sidak", joint = FALSE)
# 
# # 9 & 10-year-olds
# dt2_910 <- dt2_div %>%
#   filter(age == "nine-ten")
# 
# results_clmm_910 <- clmm(response ~ exp_choice + (1|SID), data = dt2_910, 
#                          link = "logit", Hess = TRUE, threshold = "flexible")
# 
# test(pairs(lsmeans(results_clmm_910, ~ exp_choice)), adjust = "sidak", joint = FALSE)
# 
# # adults
# dt2_adult <- dt2_div %>%
#   filter(age == "older-adult")
# 
# results_clmm_adult <- clmm(response ~ exp_choice + (1|SID), data = dt2_adult, 
#                            link = "logit", Hess = TRUE, threshold = "flexible")
# 
# test(pairs(lsmeans(results_clmm_adult, ~ exp_choice)), adjust = "sidak", joint = FALSE)



## ANALYSES USING SUBSET IN CONTROL CONDITION ONLY ----
# Compare age in control condition only
dt2_con <- dt2 %>%
  filter(condition == "control") 

results_clmm_con <- clmm(response ~ age + (1|SID) + (1|item), data = dt2_con, 
                         link = "logit", Hess = TRUE, threshold = "flexible")

summary(results_clmm_con)

# testing if random effect of participant is significant
results_clmm_con_fixed <- clmm(response ~ age + (1|item), data = dt2_con, 
                           link = "logit", Hess = TRUE, threshold = "flexible")
anova(results_clmm_con, results_clmm_con_fixed)

# testing if random effect of item is significant
results_clmm_con_fixed1 <- clmm(response ~ age + (1|SID), data = dt2_con,
                            link = "logit", Hess = TRUE, threshold = "flexible")
anova(results_clmm_con, results_clmm_con_fixed1)

# Pairwise comparisons of age groups in control condiition
test(pairs(lsmeans(results_clmm_con, ~ age)), adjust = "sidak", joint = FALSE)






# Plotting all of the data -----------
# Making separate datasets for the graphs just so that I don't mess with anything by accident
dt_div_for_graphs <- dt2_div
dt_con_for_graphs <- dt2_con

# renaming variable names for the ages so that plots have better labels and so ages displayed in order
dt_div_for_graphs$age[dt_div_for_graphs$age == "five-six"] <- "5-6"
dt_div_for_graphs$age[dt_div_for_graphs$age == "seven-eight"] <- "7-8"
dt_div_for_graphs$age[dt_div_for_graphs$age == "nine-ten"] <- "9-10"
dt_div_for_graphs$age[dt_div_for_graphs$age == "older-adult"] <- "adult"
dt_con_for_graphs$age[dt_con_for_graphs$age == "five-six"] <- "5-6"
dt_con_for_graphs$age[dt_con_for_graphs$age == "seven-eight"] <- "7-8"
dt_con_for_graphs$age[dt_con_for_graphs$age == "nine-ten"] <- "9-10"
dt_con_for_graphs$age[dt_con_for_graphs$age == "older-adult"] <- "adult"

# setting responses to be integers instead of factors to calculate means
dt_div_for_graphs$response <- as.integer(dt_div_for_graphs$response)
dt_con_for_graphs$response <- as.integer(dt_con_for_graphs$response)

# Means by age and experimenter choice, when relevant
means_diversity <- aggregate(response ~ exp_choice+age_factor, dt_div_for_graphs, mean)
means_control <- aggregate(response ~ age, dt_con_for_graphs, mean)

# Making a summary table for diversity condition 
# calculating means, sds, etc
means_d <-  ddply(dt_div_for_graphs, c("age_exact", "exp_choice"), summarise,
                  N    = length(response),
                  mean = mean(response),
                  sd   = sd(response),
                  se   = sd / sqrt(N)
)

# pulling out CI values into single table
lowerCI <- as.data.frame(div_pairwise$lsmeans$asymp.LCL)
upperCI <- as.data.frame(div_pairwise$lsmeans$asymp.UCL)
ageCI <- as.data.frame(div_pairwise$lsmeans$age)
expCI <- as.data.frame(div_pairwise$lsmeans$exp_choice)

# Binding columns
div_CI <- bind_cols(ageCI, expCI, lowerCI, upperCI)
colnames(div_CI) <- c("age","exp_choice","lower 95% CI", "upper 95% CI")

# renaming ages to prep for merge
div_CI$age <- as.character(div_CI$age)
div_CI$age[div_CI$age == "five-six"] <- "5-6"
div_CI$age[div_CI$age == "seven-eight"] <- "7-8"
div_CI$age[div_CI$age == "nine-ten"] <- "9-10"
div_CI$age[div_CI$age == "older-adult"] <- "adult"

# Compiling summary of dataset
summary_diversity <- merge(div_CI, means_d, by=c("age","exp_choice"))
summary_diversity <- bind_cols(means_d, div_CI[3:4])


# setting theme for all plots 
theme_set(theme_classic())

# plotting diversity condition with age in different panels 
plot_diversity <- ggplot(data = means_diversity) + 
  facet_grid (
    . ~ age_factor
  ) +
  scale_y_continuous(expand = c(0, 0)
  ) +
  coord_cartesian(ylim=c(0.9,5.1)
  ) + 
  labs(x = "Experimenter Choice",
       y =" Average Response"
  ) +
  theme(text         = element_text(size = 14),
        axis.title.x = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA)
  ) +
  geom_point(data = dt_div_for_graphs, 
             aes(y = response, x = exp_choice),
             position = position_jitter(w = 0.25, h = 0),
             shape = 1,
             size = 2) +
  geom_point(aes(x = exp_choice, y = response, color=age_factor),
             shape = 16,
             size = 7
  ) 
plot_diversity

# exact same diversity data but plotted with age on all graphs 
plot_diversity_alt <- ggplot(aes(x = exp_choice, y = response, fill=age), data = means_diversity) + 
  scale_y_continuous(expand = c(0, 0)
  ) +
  coord_cartesian(ylim=c(0.95,5.05)
  ) + 
  labs(x = "Experimenter Choice",
       y =" Average Response"
  ) +
  theme(text         = element_text(size = 14),
        axis.title.x = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA)
  ) +
  geom_point(data = dt_div_for_graphs, 
             aes(y = response, x = exp_choice, color=age, shape=age),
             position = position_jitter(w = 0.3, h = 0.01),
             size = 2,
             alpha = .8)  + 
  geom_point(aes(color=age),
             shape = 16,
             size = 7)
plot_diversity_alt


# plotting the averages of the control condition 
plot_control <- ggplot(aes(x = age, y = response, fill=age), data = means_control) + 
scale_y_continuous(expand = c(0, 0)
) +
  coord_cartesian(ylim=c(0.95,5.05)
  ) + 
  labs(x = "Experimenter Choice",
       y =" Average Response"
  ) +
  theme(text         = element_text(size = 14),
        axis.title.x = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA)
  ) +
  geom_point(data = dt_con_for_graphs, 
             aes(y = response, x = age),
             position = position_jitter(w = 0.3, h = 0.01),
             shape = 1,
             size = 1,
             alpha = .8)  + 
  geom_point(aes(color=age),
             shape = 16,
             size = 7)
plot_control


# Adjusting dataset so that control data has been adjusted to be binary
# So all 1 responses are now 5s
# And 2s and 4s are now 3s 

# Copies dataset
dt_con_for_graphs_binary<- dt_con_for_graphs

# Adjusting response values to be binary
dt_con_for_graphs_binary$response[dt_con_for_graphs_binary$response == "1"] <- "5"
dt_con_for_graphs_binary$response[dt_con_for_graphs_binary$response == "2" | dt_con_for_graphs_binary$response == "4" ] <- "3"

# Making response a factor
dt_con_for_graphs_binary$response <- as.integer(dt_con_for_graphs_binary$response)

# Aggregating new set of means for binary dataset
means_control_binary <- aggregate(response ~ age, dt_con_for_graphs_binary, mean)

# Plotting the adjusted control data 
plot_control_adjusted <- ggplot(aes(x = age, y = response, fill=age), data = means_control_binary) + 
  scale_y_continuous(expand = c(0, 0)
  ) +
  coord_cartesian(ylim=c(0.95,5.05)
  ) + 
  labs(x = "Experimenter Choice",
       y =" Average Response"
  ) +
  theme(text         = element_text(size = 14),
        axis.title.x = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA)
  ) +
  geom_point(data = dt_con_for_graphs_binary, 
             aes(y = response, x = age),
             position = position_jitter(w = 0.3, h = 0.01),
             shape = 1,
             size = 1,
             alpha = .8)  + 
  geom_point(aes(color=age),
             shape = 16,
             size = 7)
plot_control_adjusted 





  
  
  
  
