library(tidyverse)
library(readr)

data <- read_csv("  ", col_names = TRUE) 

# initializing variables
# creating new participant ID variable
# we need a counter (x) to keep track of which participant number we are on
# we need a list to keep track of the animals that participants have seen
data$participant <- 1
x <- 1
animalList <- c()


# loop to assign participant number
for (i in 1:as.integer(nrow(data))){
  if (i == 1){
    # on first row, set participant to 1; append animalList with animal viewed
    data$participant[i] <- x
    animalList <- append(animalList, data$trial[i])
  } else if (data$trial[i] %in% animalList | data$age[i] != data$age[i-1] | 
             data$study[i] != data$study[i-1] | data$condition[i] != data$condition[i-1]){
    # if animal viewed on this trial has already been viewed (i.e., is in animalList)
    # or age, condition, or study is different from previous row
    # move counter up by 1, set participant ID with new x, reset and append animalList
    x <- x + 1
    data$participant[i] <- x
    animalList <- c()
    animalList <- append(animalList, data$trial[i])
  } else {
    # if same age, study, and condition AND animal hasn't already been viewed by participant
    # set participant ID with current x and append animalList
    data$participant[i] <- x
    animalList <- append(animalList, data$trial[i])
  }
}

# verifying that everyone is under 5 trials 
# this should return integer(0)
anyGreaterThan4 <- aggregate(data$keeper, by=list(Category=data$participant), FUN=sum)
which(anyGreaterThan4$x >= 5)


