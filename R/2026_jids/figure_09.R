source("R/2026_jids/setup.R")

cov_yearly <- redactions_df %>%
  filter(mean_coverage_percent > 0, !is.na(year)) %>%
  group_by(year) %>%
  summarize(n          = n(),
            mean_cov   = mean(mean_coverage_percent, na.rm = TRUE),
            median_cov = median(mean_coverage_percent, na.rm = TRUE)) %>%
  filter(year >= 1995)

cov_scale_factor <- max(cov_yearly$mean_cov) / max(cov_yearly$n)

fig <- ggplot(cov_yearly, aes(x = year)) +
  geom_col(aes(y = n * cov_scale_factor),
           fill = "gray70", alpha = 0.4, width = 0.7) +
  geom_line(aes(y = mean_cov,   color = "Mean"),   linewidth = 0.9) +
  geom_line(aes(y = median_cov, color = "Median"), linewidth = 0.9, linetype = "dashed") +
  scale_color_manual(values = c("Mean" = "steelblue", "Median" = "steelblue4"),
                     name = NULL) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  scale_y_continuous(name     = "Coverage (%)",
                     sec.axis = sec_axis(~ . / cov_scale_factor, name = "Number of Cases")) +
  labs(x = "Year Filed") +
  theme_bw() +
  theme(legend.position = "bottom")

fig
