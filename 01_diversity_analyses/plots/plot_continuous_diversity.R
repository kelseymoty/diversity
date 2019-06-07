
# creating separate df for plotting
plot_continuous_data <- dt2_div
plot_continuous_data$response <- as.integer(plot_continuous_data$response)

# making adults exact 13 -- this will be used to graph adults categorically. Can make bigger or smaller number depending on if we want te adult data to be plotted closer or farther from children's data
plot_continuous_data$age_exact <- ifelse(plot_continuous_data$age_exact > 18, 13, plot_continuous_data$age_exact) 
# creating a separate grouping column that categories participants as adult or child; this is used later to assign different shapes to adult/child participants in plots
plot_continuous_data$adult <- ifelse(plot_continuous_data$age_factor == "adult", "adult", "child")

# one adult doesn't have an exact age, so filling it in
plot_continuous_data$age_exact <- ifelse(is.na(plot_continuous_data$age_exact), 13, plot_continuous_data$age_exact)

plot_cont_adult <- plot_continuous_data %>% filter(age_exact == 13)
plot_cont_child <- plot_continuous_data %>% filter(age_exact != 13)

# the plot
plot_continuous_diversity <- ggplot() + 
  facet_grid (
    . ~ exp_choice
  ) +
  geom_point(data = plot_cont_child,
             aes(y = response, x = age_exact),
             position = position_jitter(w = 0, h = 0.1),
             size = 2,
             shape = 1,
             alpha = .4
  ) + 
  geom_smooth(data = plot_cont_child,
              aes(y = response, x = age_exact),
              method = lm,
              color = "black",
              fill = "#009E73"
  ) +
  geom_point(data = plot_cont_adult,
             aes(y = response, x = age_exact),
             position = position_jitter(w = 0.2, h = 0.1),
             size = 2,
             shape = 2,
             alpha = .4
  ) + 
  stat_summary(data = plot_cont_adult,
               aes(y = response, x = age_exact),
               fun.data = mean_cl_normal, 
               geom = "errorbar", 
               fun.args = list(mult = 1)
  ) + 
  stat_summary(data = plot_cont_adult,
               aes(y = response, x = age_exact),
               geom = "point",
               fun.y = "mean",
               shape = 24,
               fill = "#009E73",
               color = "black",
               size = 4
  ) +
  scale_y_continuous(expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = c(5, 7, 9, 11, 13), 
                     labels = c("5", "7", "9", "11", "Adults")
  ) +
  coord_cartesian(ylim=c(0.9,5.1),
                  xlim=c(4.9,14)
  ) + 
  labs(x = "Age",
       y = "Participant Selections",
       title = "Character Choice"
       
  ) +
  theme(text         = element_text(size = 14),
        plot.title   = element_text(size = 14,
                                    face = "bold",
                                    hjust = .5),
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

save_plot <- here::here("plots", "plot_continuous.png")

ggsave(save_plot, plot = plot_continuous_diversity, width = 12, height = 6, dpi = 600)