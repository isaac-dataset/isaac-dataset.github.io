source("R/2026_jids/setup.R")

type_labels_lookup <- c(total_inline       = "Inline",
                        total_partial_line = "Partial line",
                        total_full_line    = "Full line",
                        total_block        = "Block")
type_levels <- c("Inline", "Partial line", "Full line", "Block")

ts_types <- redactions_df %>%
  filter(!is.na(year)) %>%
  select(year, total_inline, total_partial_line, total_full_line, total_block) %>%
  pivot_longer(-year, names_to = "type", values_to = "count") %>%
  mutate(type = factor(type_labels_lookup[type], levels = type_levels)) %>%
  group_by(year, type) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  group_by(year) %>%
  filter(sum(total) > 0) %>%
  ungroup()

label_df <- ts_types %>%
  filter(year == max(year)) %>%
  arrange(desc(type)) %>%
  mutate(share = total / sum(total),
         ypos  = cumsum(share) - share / 2)

fig <- ggplot(ts_types, aes(year, total, fill = type)) +
  geom_area(position = "fill", alpha = 0.85, color = "white", linewidth = 0.2) +
  geom_text(data = label_df,
            aes(x = year, y = ypos, label = type),
            hjust = 0, nudge_x = 0.3, size = 3.5, color = "black") +
  scale_fill_grey(start = 0.8, end = 0.3) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  labs(x = NULL, y = "Share of Redactions", fill = NULL) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

fig
