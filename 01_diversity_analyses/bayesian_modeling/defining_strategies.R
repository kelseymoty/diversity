## Defining the hypothesis space 
# This creates a matrix for each strategy (assigning probabilities to how a likely a choice would be if the person adopted a particular strategy). 
# These individual matrices are compiled into a single matrix that has all possible selections given experimenter choice. 
# This will be used to compare the participants' choices to. 
extreme_matrix = as.data.frame(matrix(c(c( 1, 1, 2, 2, 3, 3,4,5),
c( 5, 4, 5, 4, 5, 4,5,4),
c(.8,.2,.8,.2,.8,.2,1,1),
c( 2, 2, 2, 2, 2, 2,3,3)), 
ncol = 4))
extreme_matrix$prob_strategy <- "extreme"

diverse_matrix = as.data.frame(matrix(c(c(1,1,2,2,3,3,4,4,5,5),
c(5,4,5,4,1,5,1,2,1,2),
c(.8,.2,.8,.2,.5,.5,.8,.2,.8,.2),
c(2,2,2,2,2,2,2,2,2,2)), 
ncol = 4))
diverse_matrix$prob_strategy <- "diverse"

average_matrix = as.data.frame(matrix(c(c( 1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
c( 5, 4, 4, 5, 2, 4, 2, 1, 1, 2),
c(.8,.2,.8,.2,.5,.5,.8,.2,.8,.2),
c( 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)), 
ncol = 4))
average_matrix$prob_strategy <- "average"

middle_matrix = as.data.frame(matrix(c(c(1,2,3,3,4,5),
c(3,3,4,2,3,3),
c(1,1,.5,.5,1,1),
c(3,3,2,2,3,3)), 
ncol = 4))
middle_matrix$prob_strategy <- "middle"

pick1_matrix = as.data.frame(matrix(c( c( 1, 2,3,4,5),
c( 2, 1,1,1,1),
c( 1, 1,1,1,1),
c( 3 ,3,3,3,3)), 
ncol = 4))
pick1_matrix$prob_strategy <- "pick1"

pick2_matrix = as.data.frame(matrix(c( c(1, 2, 2,3,4,5),
c(2, 1, 3,2,2,2),
c(1,.5,.5,1,1,1),
c(3, 2, 2,3,3,3)), 
ncol = 4))
pick2_matrix$prob_strategy <- "pick2"

pick4_matrix = as.data.frame(matrix(c( c(1,2,3, 4, 4,5),
c(4,4,4, 3, 5,4),
c(1,1,1,.5,.5,1),
c(3,3,3, 2, 2,3)), 
ncol = 4))
pick4_matrix$prob_strategy <- "pick4"

adjacent_matrix = as.data.frame(matrix(c(c(1, 2, 2, 3, 3, 4, 4,5),
c(2, 1, 3, 2, 4, 3, 5,4),
c(1,.5,.5,.5,.5,.5,.5,1),
c(3, 2, 2, 2, 2, 2, 2,3)), 
ncol = 4))
adjacent_matrix$prob_strategy <- "adjacent"

# Listing all data frames and changing their names
dfs <- c("diverse_matrix", "extreme_matrix", "average_matrix", "middle_matrix", "pick1_matrix", "pick2_matrix", "pick4_matrix", "adjacent_matrix")

strategies <- c("diverse", "extreme",  "average", "middle", "pick1", "pick2", "pick4", "adjacent")

for(df in dfs) {
df_tmp <- get(df)
colnames(df_tmp) <- c("exp_choice","agent_choice", "probability","number_of_unexpected_choices","prob_strategy")
assign(df, df_tmp)
}

df_list <- list(diverse_matrix, extreme_matrix, average_matrix, middle_matrix, pick1_matrix, pick2_matrix, pick4_matrix, adjacent_matrix)


## Setting epsilon and adjusting probability values in accordance with epsilon
# Episilon is the value that defines how often someone behaves randomly despite having an adopted strategy. 
# Lower values of episilon means that the participant is assumed to perform less randomly, and higher values means 
# that participant is assumed to make random decisions more frequently. 
# Probabilities in the hypothesis space are adjusted for episilon. 

# Parameter to determine how often agent behaves randomly
# Lower values mean agent behaves less randomly
epsilon <- .15

# updating hypothesis space based on epsilon value
# will be needed for computing likelihoods later
expected_matrix <- bind_rows(df_list)
expected_matrix$prob_strategy <- as.factor(expected_matrix$prob_strategy)
expected_matrix$probability <- ifelse(expected_matrix$probability == .25, expected_matrix$probability, 
expected_matrix$probability - (epsilon/(4-expected_matrix$number_of_unexpected_choices))) 
expected_matrix$probability_of_unexpected <- ifelse(expected_matrix$number_of_unexpected_choices == 0, 0,
epsilon/expected_matrix$number_of_unexpected_choices)

