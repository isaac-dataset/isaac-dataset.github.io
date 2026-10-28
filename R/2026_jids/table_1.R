source("R/2026_jids/setup.R")

N_total <- nrow(isaac_df)

n_ttf_coded <- sum(!is.na(isaac_df$time_to_file))
n_dom_coded <- sum(!is.na(isaac_df$domestic_suit))
n_cov_coded <- sum(!is.na(redactions_df$mean_coverage_percent))

# Award proportion is defined only where an award could be rendered, so its base is those cases.
award_outcomes  <- c("Decided in favour of investor", "Settled",
                     "Neither investor nor the State (liability found but no damages awarded)")
award_eligible  <- isaac_df$outcome_2023 %in% award_outcomes
n_award_base    <- sum(award_eligible)
n_award_coded   <- sum(award_eligible & !is.na(isaac_df$prop_award))

fmt_pct <- function(x) sprintf("%.1f%%", 100 * x)

table_1 <- tribble(
  ~variable,          ~definition,                                                ~n_coded,      ~n_missing,                   ~denom,
  "Time to file",     "Years between first alleged breach and claim filing",      n_ttf_coded,   N_total - n_ttf_coded,        N_total,
  "Prior remedies",   "1 = investor pursued domestic litigation pre-ISDS",        n_dom_coded,   N_total - n_dom_coded,        N_total,
  "Transparency",     "Mean % of case-document text redacted",                    n_cov_coded,   N_total - n_cov_coded,        N_total,
  "Award proportion", "Awarded / claimed damages (USD), among cases where an award could be rendered", n_award_coded, n_award_base - n_award_coded, n_award_base
) %>%
  mutate(
    pct_coded = fmt_pct(n_coded / denom),
    summary = c(
      sprintf("median %.1f, mean %.1f (sd %.1f)",
              median(isaac_df$time_to_file, na.rm = TRUE),
              mean(isaac_df$time_to_file, na.rm = TRUE),
              sd(isaac_df$time_to_file, na.rm = TRUE)),
      sprintf("%s coded 1 (n = %d)",
              fmt_pct(mean(isaac_df$domestic_suit, na.rm = TRUE)),
              sum(isaac_df$domestic_suit == 1, na.rm = TRUE)),
      sprintf("median %.2f%%, mean %.2f%% (%s exactly 0)",
              median(redactions_df$mean_coverage_percent, na.rm = TRUE),
              mean(redactions_df$mean_coverage_percent, na.rm = TRUE),
              fmt_pct(mean(redactions_df$mean_coverage_percent == 0, na.rm = TRUE))),
      sprintf("median %.2f, mean %.2f",
              median(isaac_df$prop_award[award_eligible], na.rm = TRUE),
              mean(isaac_df$prop_award[award_eligible], na.rm = TRUE))
    )
  ) %>%
  select(-denom)

table_1
