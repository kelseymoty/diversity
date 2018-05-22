## Function to separate responses into two columns 

separate_response <- function(data){
  # Splitting response by ; delimiter 
  data <- separate(data = data, col = response, into = c("response1", "response2"), sep = "\\;")
  
  # Renaming columns
  names(data)[names(data) == "response.X1"] <- "response1"
  names(data)[names(data) == "response.X2"] <- "response2"
  
  # Removing old response column
  data <- data[, names(data) != "response"]
  
  # Passing data back up to global environment
  return(data)
}