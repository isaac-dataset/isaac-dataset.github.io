source("R/2026_jids/setup.R")

ttf <- isaac_df$time_to_file
c(mean = mean(ttf, na.rm = TRUE),
  median = median(ttf, na.rm = TRUE),
  sd = sd(ttf, na.rm = TRUE),
  share_3_or_less = mean(ttf <= 3, na.rm = TRUE),
  share_4_or_more = mean(ttf >= 4, na.rm = TRUE),
  share_10_or_more = mean(ttf >= 10, na.rm = TRUE),
  count_15_or_more = sum(ttf >= 15, na.rm = TRUE))

isaac_df %>%
  mutate(decade = case_when(
    year_init >= 1990 & year_init <= 1999 ~ "1990s",
    year_init >= 2000 & year_init <= 2009 ~ "2000s",
    year_init >= 2010 & year_init <= 2019 ~ "2010s"
  )) %>%
  filter(!is.na(decade)) %>%
  group_by(decade) %>%
  summarize(mean_ttf = mean(time_to_file, na.rm = TRUE))

isaac_df %>%
  filter(!is.na(indir_exp_factor), !is.na(domestic_suit), !is.na(time_to_file)) %>%
  group_by(indir_exp_factor, domestic_suit) %>%
  summarize(n = n(), mean_ttf = mean(time_to_file), .groups = "drop")

c(share_domestic_suit = mean(isaac_df$domestic_suit, na.rm = TRUE))

c(no_public_docs = sum(isaac_df$no_public_docs == 1 & isaac_df$outcome != "Pending"))

cov <- redactions_df$mean_coverage_percent
cov_pos <- cov[cov > 0]
c(n = sum(!is.na(cov)),
  share_zero = mean(cov == 0, na.rm = TRUE),
  median = median(cov, na.rm = TRUE),
  mean = mean(cov, na.rm = TRUE),
  max = max(cov, na.rm = TRUE),
  n_positive = length(cov_pos),
  median_positive = median(cov_pos, na.rm = TRUE),
  mean_positive = mean(cov_pos, na.rm = TRUE),
  count_above_10 = sum(cov_pos > 10, na.rm = TRUE))

summary(award_full$prop_award)
