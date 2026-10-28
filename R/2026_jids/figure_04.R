source("R/2026_jids/setup.R")

violin_dom_data <- isaac_df %>%
  filter(!is.na(time_to_file), !is.na(domestic_suit)) %>%
  mutate(domestic_suit = factor(domestic_suit,
                                levels = c(0, 1),
                                labels = c("No domestic suit", "Domestic suit")))

fig <- ggplot(violin_dom_data,
              aes(domestic_suit, time_to_file, fill = domestic_suit)) +
  geom_violin(alpha = 0.45, color = NA, scale = "count", trim = FALSE) +
  geom_boxplot(width = 0.15, fill = "white", outlier.size = 0.7, outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5,
               fill = "white", color = "black") +
  scale_fill_manual(values = c("No domestic suit" = "steelblue",
                               "Domestic suit"    = "steelblue4")) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(x = NULL, y = "Time to File (Years)") +
  theme_bw() +
  theme(legend.position = "none")

fig
