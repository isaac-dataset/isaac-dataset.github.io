source("R/2026_jids/setup.R")

prop_summary_dom <- function(df, group_var) {
  df %>%
    filter(!is.na(domestic_suit), !is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    summarize(
      n        = n(),
      prop_dom = mean(domestic_suit, na.rm = TRUE),
      se       = sqrt(prop_dom * (1 - prop_dom) / n),
      ci_lower = pmax(0, prop_dom - 1.96 * se),
      ci_upper = pmin(1, prop_dom + 1.96 * se),
      .groups  = "drop"
    )
}

dom_panel <- function(group_var, xlab) {
  ggplot(prop_summary_dom(isaac_df, group_var), aes(x = .data[[group_var]])) +
    geom_pointrange(aes(y = prop_dom, ymin = ci_lower, ymax = ci_upper),
                    size = 0.6, color = "steelblue") +
    labs(x = xlab, y = "Pr(Domestic Suit)") +
    theme_bw() +
    theme(legend.position = "bottom")
}

fig <- arrangeGrob(
  dom_panel("rule_of_law_3cats", "Rule of Law"),
  dom_panel("income_group", "Respondent State GDP") + theme(axis.title.y = element_blank()),
  dom_panel("case_outcome", "Case Outcome"),
  dom_panel("indir_exp_factor", "Claim Type") + theme(axis.title.y = element_blank()),
  nrow = 2, ncol = 2
)

grid::grid.draw(fig)
