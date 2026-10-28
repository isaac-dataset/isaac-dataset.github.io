source("R/2026_jids/setup.R")

red_pos <- redactions_df %>% filter(mean_coverage_percent > 0)

fig <- ggplot(red_pos, aes(mean_coverage_percent)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(x = "Mean Coverage (%)", y = "Number of Cases") +
  theme_bw()

fig
