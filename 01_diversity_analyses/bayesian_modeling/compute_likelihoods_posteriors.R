# Code for computing likelihoods and posteriors 

## Defining a helper function
# This function creates a data frame will all possible choices and probabilities 
#associated with them, and matches it with participants' responses. This is used for computing likelihoods. 
get_function <- here::here("bayesian_modeling", "fill_in_probs.R")
source(get_function)

# template that is used to save probabilities of data (agent's selection) given a specific strategy 
probability_template <- expand.grid(prob_strategy = unique(expected_matrix$prob_strategy),
                                    exp_choice = unique(expected_matrix$exp_choice))

list_of_probabilities = list()

# splitting adult data into individual data frames by id to be processed below
list_of_participants <- split(behavioral_data, behavioral_data$agent_id)

# computing probabilities of data (agent's selection) given a specific strategy 
# Separate data frame for each agent
# Saving all to a list 
for(i in 1:length(list_of_participants)){
  z <- inner_join(expected_matrix, list_of_participants[[i]])
  z <- merge(probability_template, z, all = TRUE)
  id <- list_of_participants[[i]]$agent_id[1]
  list_of_probabilities[[i]] <- fill_in_probs(z, expected_matrix, id)
}

# combining list of probabilities into single data frame
probabilities <- plyr::rbind.fill(list_of_probabilities)

# list to store likelihood dfs
list_of_likelihoods <- list()

# function to compute likelihood of each strategy by individual
calculate_ll <- function(df, strategy){
  a <- strategy
  b <- df %>% 
    filter(agent_id == current_agent) %>% 
    filter(prob_strategy == a) %>% 
    summarize(likelihood = prod(probability), strategy_ll = a)
  return(b)
}

i <- 0
for(iteration in 1:(as.numeric(nrow(probabilities))/count(probabilities$agent_id)[1,2])){
  # initializing likelihood df template
  likelihoods <- as.data.frame(matrix(ncol = 3, nrow = 0))
  current_agent <- probabilities$agent_id[iteration*count(probabilities$agent_id)[1,2]]
  for(i in 1:length(strategies)){
    # initializing likelihood df template
    y <- calculate_ll(probabilities, strategies[i])
    y$agent_id <- probabilities$agent_id[iteration*count(probabilities$agent_id)[1,2]]
    likelihoods <- rbind(likelihoods, y)
    i <- i + 1
  }
  likelihoods$likelihoodXprior <- likelihoods$likelihood / length(strategies)
  list_of_likelihoods[[iteration]] <- likelihoods
  i <- 0 
  iteration <- iteration + 1
}

likelihoods <- plyr::rbind.fill(list_of_likelihoods)

# computing the sum of likelihoods (the denominator in Bayes' Rule)
sum_ll <- likelihoods %>% group_by(agent_id) %>% dplyr::summarise(sum_ll = sum(likelihoodXprior))
likelihoods <- merge(sum_ll, likelihoods, by = "agent_id")    

# computing the posterior probabilities 
# aka probability that agent was using a given strategy given the data observed
likelihoods$posterior <- likelihoods$likelihoodXprior / likelihoods$sum_ll