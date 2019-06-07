# LIBRARIES ------------------
# First we need to load in the appropriate libraries 
library(data.table)
library(tidyverse)
library(lubridate)
library(here)

# LOADING IN DATASETS ------------------
# Next, we need to read in the correct data file (both the child and adult data files) and merge them together. 
# Currently, the files are merged testable files that need to be cleaned and saved in long format. 
# These files contains data from children and adults, in wide format
adult_data <- here::here("data", "diversity_adult_data.csv") %>% read_csv()
child_data <- here::here("data", "diversity_child_data.csv") %>% read_csv()

# CLEANING UP DATASETS ------------------
## ADULTS ------------------

# creating a temporary store of the data to work with and pulling out the relevant columns
temp_data <- adult_data
temp_data <- subset(temp_data, select = c(id, type, head, stim1, response))
temp_data$age <- "adult" # useful column for later data cleaning

# checking if participants answered check questions correctly (the 3 audio checks, lunch/laundry, and the warmups)
# laundry1 answer = 1 laundry2 answer = 3 lunch2 answer = not 2 lunch1 answer = not 3
# AudioCheck = 4 AudioCheck2 = 1 AudioCheck3 = 5 warmup = 2 warmup2 = 2

# assigning a value of 1 to new column "correct" if participants answered in accordance with key above
check <- temp_data %>%
  filter(stim1 %in% c(
    "AudioCheck", "AudioCheck2", "AudioCheck3", "warmup",
    "warmup2", "lunch1", "lunch2", "laundry1", "laundry2"
  )) %>%
  mutate(correct = ifelse(stim1 %in% c("laundry1", "AudioCheck2") & response == "1", 1,
                          ifelse(stim1 %in% c("warmup", "warmup2") & response == "2", 1,
                                 ifelse(stim1 == "laundry2" & response == "3", 1,
                                        ifelse(stim1 == "AudioCheck" & response == 4, 1,
                                               ifelse(stim1 == "AudioCheck3" & response == "5", 1,
                                                      ifelse(stim1 == "lunch2" & response != "2", 1,
                                                             ifelse(stim1 == "lunch1" & response != "3", 1, 0))))))))

# filtering out participants who answered incorrectly on Audiochecks
check <- subset(
  check,
  correct == "0" &
    (stim1 == "AudioCheck2" |
       stim1 == "AudioCheck3" |
       stim1 == "AudioCheck")
)

# removing participants who missed at least one attention check
temp_data <- anti_join(temp_data, check, by = "id") 


## CHILDREN ------------------
temp_data_c <- child_data
temp_data_c <- subset(temp_data_c, select = c(id, type, head, stim1, response))
temp_data_c$age <- "child" # useful column for later data cleaning

# checking if participants answered check questions correctly (lunch/laundry, and the warmups)
# download-4 (that's the audio check) = 2
# laundry11 answer = 1
# laundry21final answer = 3
# lunch2 answer = not 2
# lunch1 answer = not 3
# warmup = 2
# warmup2 = 2

# assigning a value of 1 to new column "correct" if participants answered in accordance with key above
check <- temp_data_c %>% 
  filter(stim1 %in% c("download-4", "warmup", "warmup2", "lunch1", "lunch2", "laundry11", "laundry21final")) %>% 
  mutate(correct = ifelse(stim1 %in% c("laundry11") & response == "1", 1, 
                          ifelse(stim1 %in% c("warmup", "warmup2", "download-4") & response == "2", 1,
                                 ifelse(stim1 == "laundry21final" & response == "3", 1,
                                        ifelse(stim1 == "lunch2" & response != "2", 1,
                                               ifelse(stim1 == "lunch1" & response != "3", 1, 0))))))

# WE ARE NOT EXCLUDING CHILDREN FOR WARM UP
# filtering out participants who answered incorrectly on lunch2 or laundry2
# check <- subset(
#   check,
#   correct == "0" &
#     (stim1 == "laundry2" |
#      stim1 == "lunch2")
# )

# removing participants who missed at least one attention check
# temp_data_c <- anti_join(temp_data_c, check, by = "id") 


# SEPARATING DEMOGRAPHICS FROM REPONSES ------------------
# Breaking data into demo datafile and test datafile 
# binding these two temp datasets together
data <- rbind(temp_data, temp_data_c)

# creating data file for demographic data only
data_demographics <- data %>% filter(type == "form") %>% dplyr::select(id, head, response, age)

# keeping only test trials in main dataset since we no longer need attention check trials
data %<>% filter(type == "test" & !stim1 %in% c("AudioCheck", "AudioCheck2", "AudioCheck3", "warmup", "warmup2", "lunch1", "lunch2", "laundry1", "laundry2", "download-4", "laundry11", "laundry21final")) %>% dplyr::select(id, stim1, response, age)


# RENAMING VARIABLES ------------------
# Creating column to specificy condition
data$condition <- ifelse(endsWith(data$stim1, "introno"), "control", "diversity")

# Creating column to specify item kind -- 
data$item <- ifelse(startsWith(data$stim1, "ch"), "cheetah",
                    ifelse(startsWith(data$stim1, "gi") | startsWith(data$stim1, "gf"), "giraffe",
                           ifelse(startsWith(data$stim1, "sk"), "skunk", 
                                  ifelse(startsWith(data$stim1, "sh"), "shark",
                                         ifelse(startsWith(data$stim1, "po"), "porcupine", "NA")))))

# Creating column to specify exp_choice, 0 for control condiion
data$exp_choice <- ifelse(data$condition == "control", 0,
                          ifelse(grepl("1", data$stim1), 1,
                                 ifelse(grepl("2", data$stim1), 2,
                                        ifelse(grepl("3", data$stim1), 3,
                                               ifelse(grepl("4", data$stim1), 4,
                                                      ifelse(grepl("5", data$stim1), 5, "NA"))))))

data <- data[,!names(data) %in% c("stim1")]


# CLEANING UP DEMOGRAPHICS ------------------

# creating separate demo for child vs adult data and converting to long
child_demographics <- data_demographics %>% 
  filter(age == "child") %>% 
  tidyr::spread(head, response)
adult_demographics <- data_demographics %>% 
  filter(age == "adult") %>% 
  mutate(head = ifelse(!is.na(head), head, 
                       ifelse(as.numeric(response) < 35 | is.na(response), "day", "year"))) %>% 
  spread(head, response)

## EDITING ADULT DEMO ------------------
# Getting adults birthdays into single column
# Converting month name to number
i <- 0
for (i in 1:nrow(adult_demographics)){
  adult_demographics[i,3] <- match(adult_demographics[i,3], month.name, nomatch = NA)
  i <- i + 1
}
colnames(adult_demographics)[3] <- "month"

adult_demographics$DOB <- as.character(as.Date.character(with(adult_demographics, paste(month, day, year, sep = "/")), "%m/%d/%Y"))

# Adding DOT column
adult_demographics$DOT <- "2017-10-26"

# removing old columns
adult_demographics <- adult_demographics[,!names(adult_demographics) %in% c("month", "day", "year")]

# computing child's age
adult_demographics$age_exact <- interval(start = adult_demographics$DOB, end = adult_demographics$DOT)/
  duration(n = 1, unit = "years")
adult_demographics$age_factor <- "adult"

#rename column containing racial ethnic info for reporting in paper
setnames(adult_demographics, old = 'Which categories describe you? (Select all boxes that apply)', new = 'RE')
setnames(adult_demographics, old = 'Gender:', new = 'Gender')

## EDITING CHILD DEMO ------------------
#rename DOB and DOT columns so easier to call
setnames(child_demographics, old = c('Date of Birth:','Test Date:'), new = c('DOB','DOT'))

# formatting date columns
child_demographics$DOB <- parse_date_time(child_demographics$DOB,
                                          orders = c("m/d/y", "m/d/Y"))
child_demographics$DOT <- parse_date_time(child_demographics$DOT,
                                          orders = c("m/d/y", "m/d/Y"))
# computing child's age
child_demographics$age_exact <- interval(start = child_demographics$DOB, end = child_demographics$DOT)/
  duration(n = 1, unit = "years")

#Create a new column called "ageGroup" with 3 different age groups 5-6, 7-8, 9-10 in div_dataForm_wide

child_demographics <- child_demographics %>% mutate(age_factor = case_when(age_exact < 7 ~ "5s/6s", 
                                                                           age_exact > 6.999 & age_exact < 9 ~ "7s/8s", 
                                                                           age_exact > 8.99 ~ "9s/10s"))


# MERGING DEMOGRAPHICS WITH TEST DATA ------------------
# Merging ages from demo back into main data file

# pulling out ages from both demos
a <- c("id", "age_exact", "age_factor")
ages <- rbind(adult_demographics[,names(adult_demographics) %in% a], child_demographics[,names(child_demographics) %in% a])

# merging ages back to main dataset
data <- full_join(data, ages, by = "id")


# FORMATTING AND EXPORTING DATA FILE ------------------
# order of columns for export
a <- c("id", "condition", "item", "exp_choice", "response", "age_exact", "age_factor")
data <- data[,names(data) %in% a]
data$age_factor <- as.factor(data$age_factor)

# writing csvs 
export_data <- here::here("data", "diversity_fulldataset_cleaned.csv")
write_csv(data, path = export_data)

# export_demo_child <- here::here("data", "testable_demographics_child.csv")
# write_csv(child_demographics, path = export_demo_child)

export_demo_adult <- here::here("data", "demographics_adult.csv")
write_csv(adult_demographics, path = export_demo_adult)

# We have finished! Let's clean up the environment
print("Datasets have been cleaned up!")
rm(list=ls())
