#' Title: 
#' Author: Steven Lauterwasser
#' 
#' Summary: This script replicates all the figures from appendix C. There is no 
#' replication code for the sole figure in appendix B as it simply repeats figure 2
#' for convenience. 
#' 
#' Note that very little explanation will be provided for the code in this script
#' since it largely reuses the same basic code already presented in the other scripts.
#' 
#' Input: 
#' - data/auth_citation_rank.csv
#' - data/auth_citation_rank.csv
#' - scripts/sim_f_net.R
#' - data/auth_top_wgs_by_year.csv
#' - data/auth_influence.csv
#' 
#' Output (does not write to disk by default, simply displays the outputs): 
#' - Figure C1: Citation Count by Rank, Authors
#' - Figure C2: Estimates of the PA function and Node Fitness distribution, Author Network
#' - Figure C3: Estimated contribution of PA for the Author Network, by year
#' - Figure C4: Estimated contribution of NF for the Author Network, by year
#' - Figure C5: Citations by rank for simulated and actual author network
#' - Figure C6: Citations by Year, Top 12 WGS Authors
#' - Table 2: Which Documents Introduce New Authors?
#' 
#' 
#' 
#' 

library(tidyverse)
library(kableExtra)
library(PAFit)

# For portability, you can easily alter the first part of the path
# in case you are not using the repository as your working directory.
path_prefix <- ""


# ---- Figure C1: Citation Count by Rank, Authors ----

auth_citation_rank <- read_csv(paste0(path_prefix, "data/auth_citation_rank.csv"))

auth_citation_rank %>%
  ggplot(aes(x = rank, y = cite_count)) + geom_point() +
  xlab("Rank") + ylab("Citation Count")


# ---- Figure C2: Estimates of the PA function and Node Fitness distribution, Author Network ----

auth_cites_network <- read_csv(paste0(path_prefix, "data/auth_cites_network.csv"))

auth_cites_network <- auth_cites_network %>%
  as.matrix() %>%
  as.PAFit_net()

# set the same seed as used in the paper
set.seed(5678)

auth_cites_network_stats <- get_statistics(auth_cites_network)

auth_cites_network_model <- joint_estimate(auth_cites_network, auth_cites_network_stats)


par(mfrow = c(1, 2))
plot(auth_cites_network_model, auth_cites_network_stats, plot = "A")
text(x = 2, y = 75, label = "A", cex = 1.5)
plot(auth_cites_network_model, auth_cites_network_stats, plot = "f")
text(x = 0.0035, y = .73, label = "B", cex = 1.5)
par(mfrow = c(1, 1))


# ---- Figures C3 and C4: Estimated contribution of PA/NF for the Author Network, by year ----

auth_cites_network_sim <- generate_simulated_data_from_estimated_model(auth_cites_network, 
                                                                       auth_cites_network_stats,
                                                                       auth_cites_network_model, 
                                                                       M = 100)

plot_contribution(auth_cites_network_sim, auth_cites_network_model, which_plot = "PA", 
                  legend_pos_x = .7)

plot_contribution(auth_cites_network_sim, auth_cites_network_model, which_plot = "f", 
                  legend_pos_x = .7)


# ---- Figure C5: Citation count by rank for simulated (line) and actual (points) data, Author Network ----

temp_net <- as_tibble(auth_cites_network$graph)

source(paste0(path_prefix, "scripts/sim_f_net.R"))

node_hist <- map(unique(temp_net$cite_year), function(x) {
  existing_nodes <- temp_net %>% 
    filter(cite_year <= x) %>%
    pull(cited_author) %>%
    unique()
  
  out <- matrix(rep(0, times = length(node_ids[["id"]])), nrow = 1, ncol = length(node_ids[["id"]]))
  out[1, sort(existing_nodes)] <- 1
  return(out)
}) %>%
  unlist() %>%
  matrix(nrow = 20, ncol = length(node_ids[["id"]]), byrow = TRUE)

m_t <- as_tibble(auth_cites_network$graph) %>% 
  count(cite_year) %>% pull(n)

# now let's run some simulations
f_net_sims <- replicate(1000, sim_f_net(auth_cites_network_model$estimate_result$f,
                                        node_hist,
                                        m_t), 
                        simplify = FALSE)

f_net_sims <- map(f_net_sims, ~.[["edge_count"]])
f_net_sims <- matrix(unlist(f_net_sims), nrow = length(node_ids[["id"]]), ncol = length(f_net_sims))

sims_summary <- tibble("node_id" = node_ids$id, 
                       "mean" = rowMeans(f_net_sims), 
                       "sd" = map_dbl(1:nrow(f_net_sims), ~sd(f_net_sims[.x, ])))

sims_summary <- sims_summary %>% 
  mutate(upr = mean + 2*sd, 
         lwr = mean - 2*sd) %>%
  arrange(desc(mean)) %>%
  mutate(rank = 1:nrow(sims_summary))

sims_to_plot <- left_join(sims_summary, temp_net %>%
                            count(cited_author) %>%
                            rename(node_id = cited_author) %>%
                            arrange(desc(n)) %>%
                            mutate(rank = 1:nrow(.)), 
                          by = "rank") 

sims_to_plot <- sims_to_plot %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>% # set any missing N's to 0 (i.e. nodes with no, in real life, ties)
  mutate(rank_equal = if_else(node_id.x == node_id.y, TRUE, FALSE)) %>%
  mutate(rank_equal = if_else(is.na(rank_equal), FALSE, rank_equal)) %>%
  mutate(rank_approx = map_lgl(node_id.x, function(x) {
    x_rank <- sims_to_plot[which(sims_to_plot[["node_id.x"]] == x), ][["rank"]]
    y_rank <- sims_to_plot[which(sims_to_plot[["node_id.y"]] == x), ][["rank"]]
    
    if (is_empty(y_rank)) {
      return(FALSE)
    } 
    
    if (x_rank == y_rank) {
      return(TRUE)
    }
    
    if (any(x_rank %in% seq(from = y_rank - 5, to = y_rank + 5))) {
      return(TRUE)
    }
    
    return(FALSE)
    
  })) %>%
  mutate(rank_approx = if_else(rank_approx, "Yes", "No")) %>%
  mutate(rank_approx = factor(rank_approx, levels = c("Yes", "No"))) 

# not labeling the first two points cause they're obvious regardless
x_arrows <- sims_to_plot[sims_to_plot$rank_approx == "Yes" &
                           sims_to_plot$rank > 20, ]$rank
x_arrow_ends <- sims_to_plot[sims_to_plot$rank_approx == "Yes" &
                               sims_to_plot$rank > 20, ]$rank
y_arrows <- sims_to_plot[sims_to_plot$rank_approx == "Yes" &
                           sims_to_plot$rank > 20, ]$n + 10
y_arrow_ends <- sims_to_plot[sims_to_plot$rank_approx == "Yes" &
                               sims_to_plot$rank > 20, ]$n + 30

sims_to_plot %>%
  ggplot(aes(x = rank)) + geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .3) + 
  geom_point(aes(y = n, color = rank_approx, shape = rank_approx)) + 
  ylab("Citations") + xlab("Rank") + labs(color = "Rank within 5 places of simulation", 
                                          shape = "Rank within 5 places of simulation") + 
  scale_shape_manual(values = c(8, 1)) +
  theme(legend.position = c(.7, .8)) + 
  annotate("segment", 
           x = x_arrows, 
           xend = x_arrow_ends,
           y = y_arrows, 
           yend = y_arrow_ends,
           arrow = arrow(angle = 30,
                         length = unit(2, "mm"), 
                         ends = "first"), 
           alpha = .8)

# ---- Figure C6: Citations by Year, Top 12 WGS Authors ----

auth_top_wgs_by_year <- read_csv(paste0(path_prefix, "data/auth_top_wgs_by_year.csv"))

auth_top_wgs_by_year %>%
  ggplot(aes(x = cite_year, y = cite_count)) + geom_line() +
  geom_vline(aes(xintercept = first_when, color = as_factor(first_where_comb), 
                 linetype = as_factor(first_where_comb))) + 
  geom_vline(aes(xintercept = second_when, color = as_factor(second_where_comb), 
                 linetype = as_factor(second_where_comb))) +
  facet_wrap(~factor(full_name, levels = c("Valian", "Acker", "Rosser", "O'Meara", "Fox", 
                                           "Correll", "Ridgeway", "Bird", "Britton", "Kanter",
                                           "Terosky", "Williams"))) + 
  xlab("Year") + ylab("Citation Count") + 
  scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 6)) +
  labs(color = "Appearance in...", linetype = "Appearance in...") + 
  theme(legend.position = "bottom")


# ---- Table C1: Which Documents Introduce New Authors? ----

auth_influence <- read_csv(paste0(path_prefix, "data/auth_influence.csv"))

options(knitr.kable.NA = "-")

auth_influence %>%
  kbl(booktabs = TRUE, col.names = c("First Appearance", "Women's and Gender Studies Source", "Anywhere", 
                                     "Proposal", "Solicitation", "Outcome"),
      caption = "Which Documents Introduce New Authors?") %>% # need a better caption
  add_header_above(c(" " = 2, "Also Appears..." = 4)) %>%
  kable_classic()




