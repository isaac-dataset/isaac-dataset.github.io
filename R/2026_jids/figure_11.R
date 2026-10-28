source("R/2026_jids/setup.R")

fig <- ggplot(award_full %>% filter(!is.na(prop_award)), aes(prop_award)) +
  geom_histogram(binwidth = 0.05, fill = "#1f78b4", color = "white") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey30") +
  annotate("text", x = 1.02, y = Inf, vjust = 1.5, hjust = 0,
           label = "claim = award", size = 3.5, color = "grey30") +
  scale_x_continuous(breaks = seq(0, 4, 0.5)) +
  labs(x = "Proportion Awarded",
       y = "Number of cases") +
  theme_minimal()

fig
