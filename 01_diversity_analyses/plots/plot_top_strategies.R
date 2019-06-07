# Plotting strategy that participant likely used when selecting animals in diversity condition
plot_top_strategies <- ggplot() +
  theme_classic() +
  geom_violin(data = top_strategies_main3, 
              aes(x = strategy_ll, 
                  y = age_exact,
                  fill = strategy_ll),
              trim = FALSE,
              scale = "count") +
  geom_point(data = top_strategies, 
             aes(x = strategy_ll, 
                 y = age_exact),
             position = position_jitter(w = 0.1),
             shape = 1,
             alpha = .5) +
  scale_y_continuous(limits = c(4.9, 12.1),
                     breaks = c(5, 6, 7, 8, 9, 10, 12), 
                     labels = c("5", "6","7", "8","9", "10", "Adults")) +
  labs(x = "Predicted Strategy",
       y = "Age") +
  scale_x_discrete(limits=c("middle","adjacent","pick4",
                            "pick2", "pick1",
                            "diverse","average","extreme")) +
  coord_flip() +
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
        panel.background = element_rect(fill = NA),
        legend.position = "none") 