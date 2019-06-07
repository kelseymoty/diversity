#This plot plots the control data when recoded as binary  

# pulling control data into a new data frame to manipulate for designing plots
plot_continuous_data_control <- dt_control_binary
plot_continuous_data_control$response_binary_glmer <- as.integer(as.character(plot_continuous_data_control$response_binary_glmer))

# making adults exact 12 -- this will be used to graph adults categorically. Can make bigger or smaller number depending on if we want te adult data to be plotted closer or farther from children's data
plot_continuous_data_control$age_exact <- ifelse(plot_continuous_data_control$age_exact > 18, 12, plot_continuous_data_control$age_exact) 

# creating a separate grouping column that categories participants as adult or child; this is used later to assign different shapes to adult/child participants in plots
plot_continuous_data_control$adult <- ifelse(plot_continuous_data_control$age_factor == "adult", "adult", "child")

# one adult doesn't have an exact age, so filling it in
plot_continuous_data_control$age_exact <- ifelse(is.na(plot_continuous_data_control$age_exact), 12, plot_continuous_data_control$age_exact)

plot_cont_binary_adult <- plot_continuous_data_control %>% filter(age_exact >= 12)
plot_cont_binary_adult$age_exact <- as.numeric(plot_cont_binary_adult$age_exact)
plot_cont_binary_child <- plot_continuous_data_control %>% filter(age_exact != 12)

# the plot, binary as 0 and 1
plot_continuous_control_binary <- ggplot() + 
  geom_point(data = plot_cont_binary_child,
             aes(y = response_binary_glmer, x = age_exact),
             position = position_jitter(w = 0, h = 0.1),
             size = 2,
             shape = 1,
             alpha = .3
  ) + 
  geom_smooth(data = plot_cont_binary_child,
              aes(y = response_binary_glmer, x = age_exact),
              method = lm,
              color = "black",
              fill = "#009E73"
  ) +
  stat_summary(data = plot_cont_binary_adult,
               aes(y = response_binary_glmer, x = age_exact),
               fun.data = mean_cl_normal,
               geom = "errorbar",
               # position = position_dodge(0.5),
               width = 0.2,
               fun.args = list(mult = 1)
  ) +
  geom_point(data = plot_cont_binary_adult,
             aes(y = response_binary_glmer, x = age_exact),
             position = position_jitter(w = 0, h = 0.1),
             size = 2,
             shape = 2,
             alpha = .3
  ) + 
  stat_summary(data = plot_cont_binary_adult,
               aes(y = response_binary_glmer, x = age_exact),
               geom = "point",
               fun.y = "mean",
               shape = 24,
               fill = "#009E73",
               color = "black",
               size = 4
  ) +
  scale_y_continuous(expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = c(5, 7, 9, 11, 12), 
                     labels = c("5", "7", "9", "11", "Adults")
  ) +
  coord_cartesian(ylim=c(-.1, 1.1)
  ) + 
  labs(x = "Age",
       y = "P of selecting extreme exemplar"
  ) +
  theme(text         = element_text(size = 14),
        axis.title.x = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA)
  ) + 
  guides(shape = FALSE)

save_plot <- here::here("plots", "plot_continuous_control_binary.png")
ggsave(save_plot, plot = plot_continuous_control_binary, width = 12, height = 6, dpi = 600)