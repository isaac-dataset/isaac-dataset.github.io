source("R/2026_jids/setup.R")

attrition_stages <- tribble(
  ~stage_idx, ~stage_short,
  1,          "Filed",
  2,          "Decided",
  3,          "Investor wins",
  4,          "≥ 50% of claim",
  5,          "≥ 75% of claim"
)

stage_filter <- function(d, idx) {
  if (idx == 1) return(d)
  if (idx == 2) return(d %>% filter(!outcome %in% c("Pending", "Data not available"),
                                    !is.na(outcome)))
  if (idx == 3) return(d %>% filter(outcome == "Decided in favour of investor"))
  if (idx == 4) return(d %>% filter(outcome == "Decided in favour of investor",
                                    !is.na(prop_award), prop_award >= 0.50))
  if (idx == 5) return(d %>% filter(outcome == "Decided in favour of investor",
                                    !is.na(prop_award), prop_award >= 0.75))
}

stage_summary <- function(d) {
  inv_win <- d %>% filter(outcome == "Decided in favour of investor")
  tibble(n_cases      = nrow(d),
         total_claims = sum(d$claims, na.rm = TRUE),
         total_awards = sum(inv_win$awards, na.rm = TRUE))
}

attrition <- attrition_stages %>%
  mutate(summary = map(stage_idx, ~ stage_summary(stage_filter(award_full, .x)))) %>%
  unnest(summary) %>%
  mutate(pct_cases = n_cases / first(n_cases))

case_max  <- max(attrition$n_cases)
claim_max <- max(attrition$total_claims)

base_layers <- list(
  geom_vline(xintercept = attrition$stage_idx, color = "grey92", linewidth = 0.3),
  geom_hline(yintercept = 0, color = "grey85", linewidth = 0.3)
)

frame_theme <- theme_minimal() +
  theme(axis.text.y  = element_blank(),
        panel.grid   = element_blank(),
        panel.border = element_rect(color = "grey85", fill = NA),
        plot.margin  = margin(5, 10, 5, 10))

stage_axis <- scale_x_continuous(breaks = attrition$stage_idx,
                                 labels = attrition$stage_short,
                                 expand = expansion(add = 0.4))

p_cases <- ggplot(attrition, aes(stage_idx)) +
  base_layers +
  geom_ribbon(aes(ymin = -n_cases / 2, ymax = n_cases / 2),
              fill = "#33373b", alpha = 0.9) +
  geom_text(aes(y = n_cases / 2, label = scales::comma(n_cases)),
            vjust = -0.6, size = 3.6, fontface = "bold") +
  geom_text(aes(y = -n_cases / 2,
                label = scales::percent(pct_cases, accuracy = 1)),
            vjust = 1.6, size = 3.2, color = "grey30") +
  stage_axis +
  coord_cartesian(ylim = c(-case_max * 0.7, case_max * 0.7)) +
  labs(x = NULL, y = NULL) +
  frame_theme

p_dollars <- ggplot(attrition, aes(stage_idx)) +
  base_layers +
  geom_ribbon(aes(ymin = -total_claims / 2, ymax = total_claims / 2),
              fill = "#D55E00", alpha = 0.35) +
  geom_ribbon(aes(ymin = -total_awards / 2, ymax = total_awards / 2),
              fill = "#0072B2", alpha = 0.95) +
  geom_text(aes(y = total_claims / 2,
                label = sprintf("$%sB claimed", scales::comma(round(total_claims / 1000)))),
            vjust = -0.6, size = 3.2, color = "#8C3D00") +
  geom_text(aes(y = -total_claims / 2,
                label = sprintf("$%sB awarded", scales::comma(round(total_awards / 1000)))),
            vjust = 1.6, size = 3.2, color = "#004E7C") +
  stage_axis +
  coord_cartesian(ylim = c(-claim_max * 0.7, claim_max * 0.7)) +
  labs(x = NULL, y = NULL) +
  frame_theme

fig <- p_cases / p_dollars

fig
