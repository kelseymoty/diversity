## Running Fisher's exact test to compare frequencies of strategy use across age groups

chisq_table <- table(top_strategies$age_factor, top_strategies$strategy_ll)

fishers <- fisher.test(table(top_strategies$age_factor, top_strategies$strategy_ll), workspace = 2e8)

fishers_strategy <- fisher.test(table(top_strategies$strategy_ll, top_strategies$age_factor), workspace = 2e8)
# to get effect size (Cramer's V) (uses 'vcd' package)
cramers <- vcd::assocstats(chisq_table)

# pairwise Fisher's exact comparing age groups; Using Holm's method for p adjust
# function from 'rcompanion' package
pairwise_fishers <- rcompanion::pairwiseNominalIndependence(chisq_table,
                                                            fisher = TRUE,
                                                            gtest  = FALSE,
                                                            chisq  = FALSE,
                                                            method = "holm")
# This table can be used to analyze pairwise of stategies (above is pairwise of age)

chisq_table_rev <- table(top_strategies$strategy_ll, top_strategies$age_factor)

# These tables can be used to analyze just pairwise of average, diverse, and extreme either across all four age groups (first table: chisq_table_3_allages) or a collapsed dichotomy comparing younger kids (5 - 8) to older kids (9-10) and adults (second table: chisq_table_3_agedich)
chisq_table_3 <- top_strategies %>% 
  filter(strategy_ll == "diverse" | strategy_ll == "average" | strategy_ll == "extreme")

chisq_table_3$age_di <- ifelse(chisq_table_3$age_factor == "9s/10s" | chisq_table_3$age_factor == "adult", "old", "young")

chisq_table_3_top3 <- table(chisq_table_3$age_factor, chisq_table_3$strategy_ll)
chisq_table_3_allages <- table(chisq_table_3$strategy_ll, chisq_table_3$age_factor)
chisq_table_3_agedich <- table(chisq_table_3$strategy_ll, chisq_table_3$age_di)

pairwise_fishers_top3 <- rcompanion::pairwiseNominalIndependence(chisq_table_3_top3,
                                                                 fisher = TRUE,
                                                                 gtest  = FALSE,
                                                                 chisq  = FALSE,
                                                                 method = "holm")

pairwise_fishers_strategies <- rcompanion::pairwiseNominalIndependence(chisq_table_3_allages,
                                                                       fisher = TRUE,
                                                                       gtest  = FALSE,
                                                                       chisq  = FALSE,
                                                                       method = "holm")