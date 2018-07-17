## Function to determine if participants correctly answered attention check
## Return subset of data where children answered attention check correctly

attention_check <- function(data){
  # Initializing variable 
  data$attnCorrect <- 0
  
  # Assigning value of 1 to attention checks answered correctly
  data$attnCorrect[data$attnTrial == "horse" & data$attnResponse == "1"] <- 1 
  data$attnCorrect[data$attnTrial == "dog" & data$attnResponse == "2"] <- 1 
  data$attnCorrect[data$attnTrial == "cow" & data$attnResponse == "3"] <- 1 
  data$attnCorrect[data$attnTrial == "pig" & data$attnResponse == "4"] <- 1 
  data$attnCorrect[data$attnTrial == "bird" & data$attnResponse == "5"] <- 1 
  
  # Subsetting data to only those with correct attention check trials 
  data <- subset(data, attnCorrect == 1)
  
  # Passing data back up to global environment
  return(data)
}