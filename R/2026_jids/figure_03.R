source("R/2026_jids/setup.R")

ttf_summary <- function(df, group_var) {
  df %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    summarize(
      mean_ttf   = mean(time_to_file, na.rm = TRUE),
      median_ttf = median(time_to_file, na.rm = TRUE),
      se         = sd(time_to_file, na.rm = TRUE) / sqrt(n()),
      ci_lower   = mean_ttf - 1.96 * se,
      ci_upper   = mean_ttf + 1.96 * se
    )
}

ttf_panel <- function(group_var, xlab) {
  ggplot(ttf_summary(isaac_df, group_var), aes(x = .data[[group_var]])) +
    geom_pointrange(aes(y = mean_ttf, ymin = ci_lower, ymax = ci_upper, color = "Mean"),
                    size = 0.6) +
    geom_point(aes(y = median_ttf, color = "Median"), shape = 17, size = 2) +
    scale_color_manual(values = c("Mean" = "steelblue", "Median" = "steelblue4"),
                       name = NULL) +
    labs(x = xlab, y = "Time to File (Years)") +
    theme_bw() +
    theme(legend.position = "bottom")
}

fig <- arrangeGrob(
  ttf_panel("rule_of_law_3cats", "Rule of Law"),
  ttf_panel("income_group", "Respondent State GDP") + theme(axis.title.y = element_blank()),
  ttf_panel("case_outcome", "Case Outcome"),
  ttf_panel("indir_exp_factor", "Claim Type") + theme(axis.title.y = element_blank()),
  nrow = 2, ncol = 2
)

grid::grid.draw(fig)
