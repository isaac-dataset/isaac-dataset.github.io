source("R/2026_jids/setup.R")

fig <- isaac_df %>%
  filter(!is.na(time_to_file)) %>%
  ggplot(aes(time_to_file)) +
  geom_histogram(binwidth = 1, fill = "#1f78b4", color = "white") +
  scale_x_continuous(breaks = seq(0, 30, 2)) +
  labs(x = "Time to File",
       y = "Number of cases") +
  theme_minimal()

fig
