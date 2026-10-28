source("R/2026_jids/setup.R")

# Recent years have claims but no awards yet, so the series ends at the last year with an award proportion.
last_year <- max(award_full$year_init[!is.na(award_full$prop_award)], na.rm = TRUE)

award_prop_yearly <- award_full %>%
  group_by(year_init) %>%
  summarize(n = n(),
            mean_prop  = mean(prop_award, na.rm = TRUE),
            mean_claim = mean(claims, na.rm = TRUE),
            .groups = "drop") %>%
  filter(year_init >= 1992, year_init <= last_year)

scale_factor <- max(award_prop_yearly$mean_prop, na.rm = TRUE) /
  max(award_prop_yearly$mean_claim, na.rm = TRUE)

fig <- ggplot(award_prop_yearly, aes(x = year_init)) +
  geom_col(aes(y = n / max(n) * max(mean_prop, na.rm = TRUE) * 0.2),
           fill = "gray70", alpha = 0.4, width = 0.7) +
  geom_line(aes(y = mean_prop, color = "Mean Award Proportion"),
            linewidth = 0.9) +
  geom_line(aes(y = mean_claim * scale_factor, color = "Mean Claim Size"),
            linewidth = 0.9, linetype = "dashed") +
  scale_y_continuous(name = "Mean Award Proportion",
                     sec.axis = sec_axis(~ . / scale_factor,
                                         name = "Mean Claim Size (USD)")) +
  scale_color_manual(values = c("Mean Award Proportion" = "steelblue",
                                "Mean Claim Size" = "steelblue4"),
                     name = NULL) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  labs(x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom")

fig
