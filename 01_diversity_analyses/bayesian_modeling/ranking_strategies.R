# Code for computing participant's top strategies 

# ranking strategies based on posteriors
likelihoods %<>% dplyr::group_by(agent_id) %>%
  dplyr::mutate(strategy_rank = rank(desc(posterior))) %>%
  dplyr::arrange(strategy_rank)

# converting columns to factors
likelihoods$strategy_rank <- as.factor(likelihoods$strategy_rank)
likelihoods$agent_id <- as.factor(likelihoods$agent_id)

# creating dataframe with top used strategies
top_strategies <- subset(likelihoods, strategy_rank == c("1"))

top_strategies$agent_id <- as.numeric(top_strategies$agent_id)

# Pulling age data in top strategy df
ages <- behavioral_data %>% 
  select(agent_id, age_exact, age_factor) %>% 
  filter(!duplicated(agent_id)) 

top_strategies <- full_join(top_strategies, ages)

# Assining adults exact age of 12 since we are interested in adults as a group
top_strategies <- top_strategies %>% 
  filter(!is.na(sum_ll)) 

top_strategies$age_exact <- ifelse(top_strategies$age_factor == "adult", 12, top_strategies$age_exact)

proportion_top_strategy <- top_strategies %>% 
  group_by(age_factor, strategy_ll) %>%
  dplyr::summarize (n = n()) %>%
  dplyr::mutate(freq = n / sum(n))

# Getting most used strategies average, extreme and diverse strategies
top_strategies_main3 <- top_strategies %>% 
  filter(strategy_ll == "average" |
           strategy_ll == "diverse" |
           strategy_ll == "extreme")