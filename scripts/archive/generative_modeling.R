#' Title: generative_modeling.R
#' Author: Steven Lauterwasser
#' 
#' Summary: This script replicates figures 2 through 5, performing the underlying 
#' modeling and running the necessary simulations. For details on the methods, 
#' see the paper and/or the package PAFit and its documentation. 
#' 
#' Input: 
#' - data/cites_item_network.csv
#' - scripts/sim_f_net.R
#' 
#' Output (does not write to disk by default, simply displays the outputs):
#' - Figure 2: Estimates of the Preferential Attachment function and Node Fitnesses
#' - Figure 3: Estimated contributions of Preferential Attachment, by year
#' - Figure 4: Estimated contributions of Node Fitness, by year
#' - Figure 5: Citation count by rank for simulated (line) and actual (points) data
#' 

library(tidyverse)
library(PAFit)

# For portability, you can easily alter the first part of the path
# in case you are not using the repository as your working directory.
path_prefix <- ""


# ---- Load the Data and Run the Model----

cites_item_network <- read_csv(paste0(path_prefix, "data/cites_item_network.csv"))

# From a dataframe to a PAFit network object
cites_item_network <- cites_item_network %>% 
  as.matrix() %>%
  as.PAFit_net()

# set the same seed as used in the paper
set.seed(2884)

# Create a summary statistics object which is used in model estimation
# (Note: `joint_estimate()` will create this on its own by default; this
# way is just more explicit.)
cites_item_network_stats <- get_statistics(cites_item_network)

# Estimate the joint preferential attachment and node fitness model. ~10 seconds
cites_item_network_model <- joint_estimate(cites_item_network, cites_item_network_stats)


# ---- Figure 2: Estimates of the Preferential Attachment function and Node Fitness distribution ----
# Note, the calls to par() and text() are purely for purposes of formatting/labeling; the two 
# plot() calls are all that's actually necessary to display PA (`plot(..., plot = "A")`) and 
# NF (`plot(..., p = "f")`) estimates. 

par(mfrow = c(1, 2))
plot(cites_item_network_model, cites_item_network_stats, plot = "A")
text(x = 2, y = 75, label = "A", cex = 1.5)
plot(cites_item_network_model, cites_item_network_stats, plot = "f")
text(x = 0.0035, y = .73, label = "B", cex = 1.5)
par(mfrow = c(1, 1))


# ---- Figures 3 and 4: Estimated contributions of Preferential Attachment/Node Fitness, by year ----
# First we generate the simulated data necessary for estimating uncertainty around 
# the contribution statistics (for further explanation see appendix B and/or PAFit's 
# documentation). 
# Note: this takes roughly 10 minutes on an M1 Pro MacBook Pro.
cites_item_network_sim <- generate_simulated_data_from_estimated_model(cites_item_network, 
                                                                       cites_item_network_stats,
                                                                       cites_item_network_model,
                                                                       M = 100)

# Preferential attachment estimated contributions
plot_contribution(cites_item_network_sim, cites_item_network_model, which_plot = "PA", 
                  legend_pos_x = .7)

# Node fitness estimated contributions
plot_contribution(cites_item_network_sim, cites_item_network_model, which_plot = "f", 
                  legend_pos_x = .7)


# ---- Figure 5: Citation count by rank for simulated (line) and actual (points) data ---- 
# To create the simulated distribution(s), we imagine a world where new edges formed based
# *only* on node fitness, but everything else is the same: each node enters the network 
# at the same time-step as in the real network and the same number of new edges are created
# at each time-step as in the real network. Thus the simulated and real networks are 
# all identical in terms of the number of nodes, the number of edges, and the rate of growth. 
# All that's different is which nodes those edges end up connecting. 


# Reconvert the edge list to a tibble for convenience
temp_net <- as_tibble(cites_item_network$graph)

# Load a custom function which simulates final node degrees assuming
# a purely node fitness driven network. 
source(paste0(path_prefix, "scripts/sim_f_net.R"))
# Doing this  requires: 
# - node fitnesses
# - node history (when each node enters the network)
# - edge history (the number of new edges at each time step)


#' @param node_fitness a vector of node fitnesses. If named, the names will be
#' used as node ids.
#' @param node_history a matrix with one row per time step to simulate and one  
#' column per node, with a cell value of 1 indicating a given node exists at a 
#' given time step and value of 0 indicating the opposite.
#' @param m_t a vector where the t-th element is the number of new edges at time t.
#' @param node_ids a vector of node ids. If `NULL` (the default) then either the 
#' names (if present) or indices of the `node_fitness` vector are used instead.

# Create a matrix representing the node history: one row per time-step, one column
# per node, with a cell value of 1 if a given node exists at that time step and 0
# otherwise.
node_hist <- map(unique(temp_net$cite_year), function(x) {
  existing_nodes <- temp_net %>% 
    filter(cite_year <= x) %>%
    pull(cited_doc) %>%
    unique()
  
  out <- matrix(rep(0, times = length(node_ids[["id"]])), nrow = 1, ncol = length(node_ids[["id"]]))
  out[1, sort(existing_nodes)] <- 1
  return(out)
  }) %>%
  unlist() %>%
  matrix(nrow = 20, ncol = length(node_ids[["id"]]), byrow = TRUE)

# Create a vector of the count of new edges per time-step
m_t <- as_tibble(cites_item_network$graph) %>% 
  count(cite_year) %>% pull(n)

# Now we run some simulations (~10 seeconds)
f_net_sims <- replicate(1000, sim_f_net(cites_item_network_model$estimate_result$f, # using the node fitnesses estimated by the model
                                        node_hist,
                                        m_t), 
                        simplify = FALSE)

# We now have 1000 simulated networks
f_net_sims <- map(f_net_sims, ~.[["edge_count"]]) # We get just the edge counts (dropping the node_ids)
f_net_sims <- matrix(unlist(f_net_sims), nrow = length(node_ids[["id"]]), ncol = length(f_net_sims)) # And convert to a matrix

# And then summarize into a tibble: taking the mean and standard deviation for the 
# final degree of each node across the 1000 simulations
sims_summary <- tibble("node_id" = node_ids$id, 
                       "mean" = rowMeans(f_net_sims), 
                       "sd" = map_dbl(1:nrow(f_net_sims), ~sd(f_net_sims[.x, ])))

# Create 2 sd confidence intervals and rank for graphing
sims_summary <- sims_summary %>% 
  mutate(upr = mean + 2*sd, 
         lwr = mean - 2*sd) %>%
  arrange(desc(mean)) %>%
  mutate(rank = 1:nrow(sims_summary))

# Rejoin with the original (actual) network, in the same citation-rank form
sims_to_plot <- left_join(sims_summary, temp_net %>%
                            count(cited_doc) %>%
                            rename(node_id = cited_doc) %>%
                            arrange(desc(n)) %>%
                            mutate(rank = 1:nrow(.)), 
                          by = "rank")

# And then create the graph (Figure 5)
sims_to_plot <- sims_to_plot %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>% # set any missing N's to 0 (i.e. nodes with no, in real life, no ties)
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


# labeling the otherwise obscured points which match rank
# excepting the first couple, cause they're easily legibly without
x_arrows <- sims_to_plot[sims_to_plot$rank_approx == "Yes" &
                           sims_to_plot$rank > 100, ]$rank
x_arrow_ends <- sims_to_plot[sims_to_plot$rank_approx == "Yes" &
                               sims_to_plot$rank > 100, ]$rank
y_arrows <- sims_to_plot[sims_to_plot$rank_approx == "Yes" &
                           sims_to_plot$rank > 100, ]$n + 5
y_arrow_ends <- sims_to_plot[sims_to_plot$rank_approx == "Yes" &
                               sims_to_plot$rank > 100, ]$n + 10

sims_to_plot %>%
  ggplot(aes(x = rank)) + geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .3) + 
  geom_point(aes(y = n, color = rank_approx, shape = rank_approx)) + 
  ylab("Citations") + xlab("Rank") + 
  labs(color = "Rank within 5 places of simulation",
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



