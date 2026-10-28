source("R/2026_jids/setup.R")

award_summary <- function(df, group_var) {
  df %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    summarize(
      mean_prop_award   = mean(prop_award, na.rm = TRUE),
      median_prop_award = median(prop_award, na.rm = TRUE),
      se         = sd(prop_award, na.rm = TRUE) / sqrt(n()),
      ci_lower   = mean_prop_award - 1.96 * se,
      ci_upper   = mean_prop_award + 1.96 * se
    )
}

award_panel <- function(group_var, xlab, size = 0.6) {
  ggplot(award_summary(isaac_df, group_var), aes(x = .data[[group_var]])) +
    geom_pointrange(aes(y = mean_prop_award, ymin = ci_lower, ymax = ci_upper, color = "Mean"),
                    size = size) +
    geom_point(aes(y = median_prop_award, color = "Median"), shape = 17, size = 2) +
    scale_color_manual(values = c("Mean" = "steelblue", "Median" = "steelblue4"),
                       name = NULL) +
    labs(x = xlab, y = "Award Proportion") +
    theme_bw() +
    theme(legend.position = "bottom")
}

fig <- arrangeGrob(
  award_panel("rule_of_law_3cats", "Rule of Law"),
  award_panel("income_group", "Respondent State GDP") + theme(axis.title.y = element_blank()),
  award_panel("sector", "Sector", size = 0.8),
  award_panel("indir_exp_factor", "Claim Type") + theme(axis.title.y = element_blank()),
  nrow = 2, ncol = 2
)

grid::grid.draw(fig)
