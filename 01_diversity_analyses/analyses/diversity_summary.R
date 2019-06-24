# Summary of participants' responses in the Set Condition
dt2_div_numeric <- dt2_div
dt2_div_numeric$response <- as.numeric(dt2_div_numeric$response)


data_summary <- summarySE(dt2_div_numeric, 
                          measurevar = "response",
                          groupvars = c("age_factor", 
                                        "exp_choice")
)

dt2_set_numeric <- dt2_div_numeric
dt2_set_numeric$age_factor <- as.character(dt2_set_numeric$age_factor)
dt2_set_numeric$age_factor <- ifelse(dt2_set_numeric$age_factor == "7s/8s" | dt2_set_numeric$age_factor == "5s/6s", "5s/8s", dt2_set_numeric$age_factor)
data_summary_set <- summarySE(dt2_set_numeric, 
                              measurevar = "response",
                              groupvars = c("age_factor")
)