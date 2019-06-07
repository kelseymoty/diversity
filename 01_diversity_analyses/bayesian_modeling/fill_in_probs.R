# This function creates a data frame will all possible choices and 
# probabilities associated with them, and matches it with participants' responses. This is used for computing likelihoods. 

fill_in_probs <- function(df1, df2, id){
  a <- c("number_of_unexpected_choices", "probability_of_unexpected")
  # subsetting to have df with all strategies but random and then removing columns that will be added later
  temp1 <- subset(df1, prob_strategy != "random")
  temp1 <- temp1[,!(names(df1) %in% a)]
  # getting the columns removed from above from the main df that has all of them, so they can be merged back in
  temp2 <- subset(df2, prob_strategy != "random")
  temp2 <- temp2[, c(1,4:6)]
  # adding the columns back in so they are complete (no NA values)
  final <- unique(merge(temp1, temp2, by = c("exp_choice", "prob_strategy")))
  # filling in NAs for agent id and probabilities so template will be complete for computing likelihoods
  final$probability <- ifelse(is.na(final$probability), final$probability_of_unexpected, final$probability)
  final$agent_id <- id
  
  return(final)
}