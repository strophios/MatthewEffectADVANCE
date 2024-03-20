#' Title: citation_statistics.R
#' Author: Steven Lauterwasser
#' 
#' Summary: This script replicates the basic summary statistic results, including
#' figures 1 and 6, as well as table 1. 
#' 
#' Input: 
#' - data/citation_rank.csv
#' - data/top_wgs_cites_by_year.csv
#' - data/source_influence.csv
#' 
#' Output (does not write to disk by default, simply displays the outputs): 
#' - Figure 1: Citation Count by Rank, Sources
#' - Figure 6: Citations by Year, Top 12 WGS Sources
#' - Table 1: Which Documents Introduce New Sources?
#' 

library(tidyverse)
library(kableExtra)

# For portability, you can easily alter the first part of the path
# in case you are not using the repository as your working directory.
path_prefix <- ""


# ---- Figure 1: Citation Count by Rank, Sources ----

citation_rank <- read_csv(paste0(path_prefix, "data/citation_rank.csv"))

citation_rank %>%
  ggplot(aes(x = rank, y = cite_count)) + geom_point() +
  xlab("Rank") + ylab("Citation Count")


# ---- Figure 6: Citations by Year, Top 12 Women's and Gender Studies Sources ----

top_wgs_cites_by_year <- read_csv(paste0(path_prefix, "data/top_wgs_cites_by_year.csv"))

top_wgs_cites_by_year %>%
  mutate(source_cite = factor(source_cite, levels = unique(temp$source_cite))) %>%
  ggplot(aes(x = cite_year, y = cite_count)) + geom_line() +
  geom_vline(aes(xintercept = first_when, color = as_factor(first_where_comb), 
                 linetype = as_factor(first_where_comb))) + 
  geom_vline(aes(xintercept = second_when, color = as_factor(second_where_comb), 
                 linetype = as_factor(second_where_comb))) + 
  facet_wrap(~source_cite) + 
  xlab("Year") + ylab("Citation Count") + 
  scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 6)) +
  labs(color = "Appearance in...", linetype = "Appearance in...") + 
  theme(legend.position = "bottom")


# ---- Table 1: Which Documents Introduce New Sources? ----

source_influence <- read_csv(paste0(path_prefix, "data/source_influence.csv"))

options(knitr.kable.NA = "-")

source_influence %>%
  kbl(booktabs = TRUE, col.names = c("First Appearance", "Women's and Gender Studies Source", "Anywhere", 
                                     "Proposal", "Solicitation", "Outcome"),
      caption = "Which Documents Introduce New Sources?") %>% # need a better caption
  add_header_above(c(" " = 2, "Also Appears..." = 4)) %>%
  kable_classic()





