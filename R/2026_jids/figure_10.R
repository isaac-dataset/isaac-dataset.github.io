source("R/2026_jids/setup.R")

merged_pos <- redactions_df %>%
  filter(mean_coverage_percent > 0) %>%
  left_join(isaac_df %>% select(case_id, rule_of_law_3cats, income_group,
                                case_outcome, indir_exp_factor),
            by = "case_id")

cov_summary <- function(df, group_var) {
  df %>%
    filter(!is.na(mean_coverage_percent), !is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    summarize(
      n          = n(),
      mean_cov   = mean(mean_coverage_percent, na.rm = TRUE),
      median_cov = median(mean_coverage_percent, na.rm = TRUE),
      se         = sd(mean_coverage_percent, na.rm = TRUE) / sqrt(n()),
      ci_lower   = mean_cov - 1.96 * se,
      ci_upper   = mean_cov + 1.96 * se
    )
}

cov_panel <- function(group_var, xlab) {
  ggplot(cov_summary(merged_pos, group_var), aes(x = .data[[group_var]])) +
    geom_pointrange(aes(y = mean_cov, ymin = ci_lower, ymax = ci_upper, color = "Mean"),
                    size = 0.6) +
    geom_point(aes(y = median_cov, color = "Median"), shape = 17, size = 2) +
    scale_color_manual(values = c("Mean" = "steelblue", "Median" = "steelblue4"),
                       name = NULL) +
    labs(x = xlab, y = "Mean Coverage (%)") +
    theme_bw() +
    theme(legend.position = "bottom")
}

fig <- arrangeGrob(
  cov_panel("rule_of_law_3cats", "Rule of Law"),
  cov_panel("income_group", "Respondent State GDP") + theme(axis.title.y = element_blank()),
  cov_panel("case_outcome", "Case Outcome"),
  cov_panel("indir_exp_factor", "Claim Type") + theme(axis.title.y = element_blank()),
  nrow = 2, ncol = 2
)

grid::grid.draw(fig)
