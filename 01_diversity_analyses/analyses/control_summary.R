# Summary of participants' responses in the Single Condition
dt2_single_numeric <- dt2_control
dt2_single_numeric$response <- as.numeric(dt2_single_numeric$response)
dt2_single_numeric$age_factor <- as.character(dt2_single_numeric$age_factor)
dt2_single_numeric$age_factor <- ifelse(dt2_single_numeric$age_factor == "7s/8s" | dt2_single_numeric$age_factor == "5s/6s", "5s/8s", dt2_single_numeric$age_factor)

data_summary_single <- summarySE(dt2_single_numeric, 
                                 measurevar = "response",
                                 groupvars = c("age_factor", 
                                               "exp_choice")
)