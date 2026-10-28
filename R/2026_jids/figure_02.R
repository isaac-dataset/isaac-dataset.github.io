source("R/2026_jids/setup.R")

ttf_yearly <- isaac_df %>%
  group_by(year_init) %>%
  summarize(n = n(),
            mean_ttf = mean(time_to_file, na.rm = TRUE),
            median_ttf = median(time_to_file, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year_init >= 1992)

fig <- ggplot(data = ttf_yearly, aes(x = year_init)) +
  geom_line(aes(y = mean_ttf, color = "Mean")) +
  geom_line(aes(y = median_ttf, color = "Median"), linetype = "dashed") +
  scale_color_manual(values = c("Mean" = "steelblue", "Median" = "steelblue4"),
                     name = NULL) +
  geom_col(aes(y = n / max(n)),
           fill = "gray70", alpha = 0.4, width = 0.7) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  labs(x = "Year Filed",
       y = "Time to File (Years)") +
  theme_minimal() +
  theme(legend.position = "bottom")

fig
