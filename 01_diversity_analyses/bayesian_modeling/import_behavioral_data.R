# Reading in our behavioral data 

behavioral_data<- here::here("data", "diversity_fulldataset_cleaned.csv") %>% read_csv()

# specifying certain columns as numeric and remaning (participant's) response column to agent_choice
behavioral_data$exp_choice <- as.numeric(as.character(behavioral_data$exp_choice))
colnames(behavioral_data)[2] <- "agent_choice"
behavioral_data$agent_choice <- as.numeric(as.character(behavioral_data$agent_choice))

# assigning an agent_id separate from participant number. I did this because at one point I was having issues with a function because some IDs are numeric and others are not, and the function preferred numeric only.... 
i <- 0 
behavioral_data$agent_id <- 1
for (i in 2:nrow(behavioral_data)){
  if (behavioral_data$id[i] == behavioral_data$id[i-1] &&
      behavioral_data$condition[i] == behavioral_data$condition[i-1]){
    behavioral_data$agent_id[i] <- behavioral_data$agent_id[i-1]
  } else {
    behavioral_data$agent_id[i] <- behavioral_data$agent_id[i-1] + 1
  }
  i <- i + 1
}

# Separating diversity data into separate data frames for each age group
behavioral_data_adult <- behavioral_data %>% filter(age_factor == "adult" & condition == "diversity")
behavioral_data_56 <- behavioral_data %>% filter(age_factor == "5s/6s" & condition == "diversity")
behavioral_data_78 <- behavioral_data %>% filter(age_factor == "7s/8s" & condition == "diversity")
behavioral_data_910 <- behavioral_data %>% filter(age_factor == "9s/10s" & condition == "diversity")