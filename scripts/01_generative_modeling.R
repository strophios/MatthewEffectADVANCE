#' Title: 01_generative_modeling.R
#' Author: Steven Lauterwasser
#' 
#' Summary: This script performs the generative network modeling
#' with `PAFit` on which the modeling results are based. It also 
#' replicates figure 6.1.
#' 
#' Note: This does perform the simulations needed to assess the 
#' PA and NF contributions, but it does not do any of the treatment
#' effect estimation or simulation. That is in `03_estimate_treatment_effects.R`.
#' 
#' Input: 
#' - `data/edge_list_df.rds`
#' 
#' Output: 
#' - `model_list`: A container for the modeling and related results. Includes: 
#'  - `network_obj`: the PAFit network object created from `cites_df`.
#'  - `node_ids`: a two column dataframe of the correspondence between the IDs of the input (from `source_col` and
#'    `target_col`) and sequential integer IDs guaranteed to work with PAFit.
#'  - `stats_obj`: the PAFit stats object returned by `get_statistics()`. 
#'  - `model_obj`: the fit model object produced by `joint_estimate()`.
#' - `simulation_obj`: Simulated networks created based on `model_list$model_obj` and used to get 
#'   confidence intervals for the estimated PA and NF contributions. 
#' - `contrib_summary`: A dataframe containing a summary of the estimated contributions. 
#' 
#' Note that, due to time required to run the model and simulations, and their use as 
#' inputs for later results, both `model_list` and `simulation_obj` are written to disk
#' by default (in `gen/`). `contrib_summary` and the plotted figure are not.
#' 

library(tidyverse)
library(PAFit)
library(future)

path_prefix <- "MatthewEffectADVANCE/" # Set path as appropriate
source(paste0(path_prefix, "scripts/00_function_library.R"))

edge_list_df <- read_rds(paste0(path_prefix, "data/edge_list_df.rds"))

set.seed(2884)

# Running the model takes 20-30 second on an M1 Pro MacBook Pro
model_list <- PAFit_model_pipeline(edge_list_df, "source_col", "target_col", "cite_year")

write_rds(model_list, paste0(path_prefix, "gen/model_list.rds")) # Write to disk

#' For the paper, we ran 1000 simulations. This takes a little under 45 minutes on an M1 Pro MacBook Pro
#' running 8 simulations in parallel (`workers = 8` below). Thus, for convenience this replication script 
#' defaults to just 10, which takes a little under 30 seconds on the same machine.
M <- 10

start <- proc.time()
simulation_obj <- generate_simulated_data_from_estimated_model_parallel(model_list[["network_obj"]], 
                                                                        model_list[["stats_obj"]], 
                                                                        model_list[["model_obj"]], 
                                                                        M = M, 
                                                                        workers = 8)

# write to disk
write_rds(simulation_obj, paste0(path_prefix, "gen/simulation_obj.rds")) # Write to disk

end <- proc.time()
end - start

# Create a dataframe with a summary of the contribution stats
contrib_summary <- est_contrib_uncertainty(simulation_obj, model_list[["model_obj"]]) %>%
  group_by(mechanism) %>%
  summarize(error_sd = sqrt(mean((y_obs - y)^2)), 
            across(contains("y"), mean))



# ---- The Plotting ----

par(mfrow = c(1, 2))
plot(model_list$model_obj, model_list$stats_obj, plot = "A", confidence = TRUE)
text(x = 1.5, y = 40, label = "A", cex = 1.5)
plot(model_list$model_obj, model_list$stats_obj, plot = "f", confidence = TRUE)
text(x = .5, y = 2, label = "B", cex = 1.5)
par(mfrow = c(1, 1))