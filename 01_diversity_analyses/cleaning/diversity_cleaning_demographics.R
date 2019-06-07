# tibbles for demo
dt2_c <- dt2 %>% 
  filter(age_factor != "adult")
dt2_a <- dt2 %>% 
  filter(age_factor == "adult")

# demographics info for kids was entered separately from parent report on paper demographics forms, add to dataset for demo reporting
diversity_demo_child <- here::here("data", "diversity_demo.csv") %>% read_csv() 

#select keepers
diversity_demo_child_keepers <- diversity_demo_child %>%
  filter(Keeper == 1)

diversity_demo_child_keepers_reported <- diversity_demo_child_keepers %>%
  filter(RE != "Unreported" )

diversity_demo5.6 <- diversity_demo_child_keepers %>%
  filter(age_factor == "5s/6s")
diversity_demo7.8 <- diversity_demo_child_keepers %>%
  filter(age_factor == "7s/8s")
diversity_demo9.10 <- diversity_demo_child_keepers %>%
  filter(age_factor == "9s/10s")

# read in adults demo file
diversity_demo_adult <- here::here("data", "demographics_adult.csv") %>% read_csv() 

#Recode racial/ethnic to separate out hispanic and combine for consistency
diversity_demo_adult$hispanic <- ifelse(grepl("Hispanic", diversity_demo_adult$RE),1,0)

diversity_demo_adult$RE[diversity_demo_adult$RE == "White;Hispanic; Latino; or Spanish origin"] <- "White"
diversity_demo_adult$RE[diversity_demo_adult$RE == "Hispanic; Latino; or Spanish origin"] <- "Unreported"
diversity_demo_adult$RE[diversity_demo_adult$RE == "White;American Indian or Alaska Native"] <- "Mixed"
diversity_demo_adult$RE[diversity_demo_adult$RE == "White;Asian"] <- "Mixed"

print("Demographics cleaned up!")