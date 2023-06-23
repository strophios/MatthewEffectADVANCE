#' Title:
#' Author: Steven Lauterwasser
#' 
#' Summary: This script replicates figures 7 through 9.
#' 
#' Input: 
#' - data/intersectional_sources.csv
#' - data/intersectional_proposals.csv
#' - data/intersectional_proposal_sources.csv
#' 
#' Output (does not write to disk by default, simply displays the outputs):
#' - Figure 7: Citations to Intersectional Sources, by Year
#' - Figure 8: Citations to Intersectional Sources in Proposals
#' - Figure 9: Citations to 2018 Proposals’ Intersectional Sources, by Year
#' 

library(tidyverse)

# For portability, you can easily alter the first part of the path
# in case you are not using the repository as your working directory.
path_prefix <- ""

# ---- Figure 7: Citations to Intersectional Sources, by Year ----

intersectional_sources <- read_csv(paste0(path_prefix, "data/intersectional_sources.csv"))

intersectional_sources %>%
  ggplot(aes(x = cite_year, y = n, color = intersectional, linetype = intersectional)) + geom_line() + 
  geom_vline(xintercept = 2016) + geom_vline(xintercept = 2018) +
  xlab("Year") + ylab("Citation Count") +
  labs(color = "Labeling Method", linetype = "Labeling Method") + theme(legend.position = c(.2, .75))


# ---- Figure 8: Citations to Intersectional Sources in Proposals ----

intersectional_proposals <- read_csv(paste0(path_prefix, "data/intersectional_proposals.csv"))

intersectional_proposals %>% 
  ggplot(aes(x = cite_year, y = cite_count, color = inter_type, linetype = inter_type)) + geom_line() + 
  geom_vline(xintercept = 2016) + 
  xlab("Year") + ylab("Citation Count") + 
  labs(color = "Labeling Method", linetype = "Labeling Method") + theme(legend.position = c(.2, .75))


# ---- Figure 9: Citations to 2018 Proposals’ Intersectional Sources, by Year ----

intersectional_proposal_sources <- read_csv(paste0(path_prefix, "data/intersectional_proposal_sources.csv"))

intersectional_proposal_sources %>%
  ggplot(aes(x = cite_year, y = int_count, color = int_type, linetype = int_type)) + geom_line() +
  xlab("Year") + ylab("Citation Count") + geom_vline(xintercept = 2016) + 
  geom_vline(xintercept = 2018) + labs(color = "Labeling Method", linetype = "Labeling Method") + 
  theme(legend.position = c(.2, .75))
