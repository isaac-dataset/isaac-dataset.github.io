library(tidyverse)
library(gridExtra)
library(patchwork)

plot_font <- "Times New Roman"

theme_bw <- function(...) {
  ggplot2::theme_bw(base_family = plot_font, base_size = 11, ...) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey92"),
      panel.border     = ggplot2::element_rect(color = "grey80", fill = NA),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      legend.position  = "bottom",
      plot.margin      = ggplot2::margin(5, 5, 5, 5)
    )
}

theme_minimal <- function(...) {
  ggplot2::theme_minimal(base_family = plot_font, base_size = 11, ...) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey92"),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      legend.position  = "bottom",
      plot.margin      = ggplot2::margin(5, 5, 5, 5)
    )
}

update_geom_defaults("text",  list(family = plot_font))
update_geom_defaults("label", list(family = plot_font))

isaac_df      <- read.csv("data/2026_jids/isaac_cases.csv", na.strings = "")
redactions_df <- read.csv("data/2026_jids/isaac_redactions.csv", na.strings = "")
award_full    <- read.csv("data/2026_jids/unctad_awards_2023.csv", na.strings = "")

isaac_df <- isaac_df %>%
  mutate(time_to_file = year_init - year_incid) %>%
  mutate(sector = factor(sector, levels = c("Primary", "Secondary", "Tertiary"))) %>%
  mutate(rule_of_law_3cats = case_when(
    v2x_rule < 0.33                    ~ "Low",
    v2x_rule >= 0.33 & v2x_rule < 0.66 ~ "Middle",
    v2x_rule >= 0.66                   ~ "High",
    TRUE ~ NA_character_
  )) %>%
  mutate(rule_of_law_3cats = factor(rule_of_law_3cats,
                                    levels = c("Low", "Middle", "High"))) %>%
  mutate(income_group = case_when(
    gdp_capita < 1135                       ~ "Low",
    gdp_capita >= 1135 & gdp_capita < 4466  ~ "Lower-Middle",
    gdp_capita >= 4466 & gdp_capita < 13846 ~ "Upper-Middle",
    gdp_capita >= 13846                     ~ "High",
    TRUE ~ NA_character_
  )) %>%
  mutate(income_group = factor(income_group,
                               levels = c("Low", "Lower-Middle",
                                          "Upper-Middle", "High"))) %>%
  mutate(case_outcome = case_when(
    str_detect(outcome, "Decided in favour of investor") ~ "Investor Win",
    str_detect(outcome, "favour of State|liability")     ~ "State Win",
    str_detect(outcome, "Settled")                       ~ "Settled",
    TRUE ~ NA_character_
  )) %>%
  mutate(case_outcome = factor(case_outcome,
                               levels = c("Settled", "Investor Win", "State Win"))) %>%
  mutate(indir_exp_factor = factor(indir_exp_alleged,
                                   levels = c(0, 1),
                                   labels = c("Other", "Indirect Expropriation")))
