#' Title: 06_item_network_results.R
#' Author: Steven Lauterwasser
#' 
#' Summary: This script replicates the results for the item level network
#' (the results from the body of the paper are for the author level network).
#' These results are presented in appendix E. 
#' 
#' One upshot of this is that this one script basically consists of scripts 
#' one through four combined. As such, it will take about as long to run as 
#' all four of those. We have tried to catch places for obvious shortcuts, 
#' e.g., checking to see if the bootstrap simulations have already been 
#' written to disk before taking the time to simulate them again. However, 
#' we're not sure we've gotten all such places, so it may be advisable to 
#' be somewhat selective about what portions of the script to run, when 
#' possible. 
#' 
#' Additionally, note that this script occupies the same namespaces as the
#' previous five. That is, while it loads the `item_edge_list_df.rds`, it 
#' still uses `edge_list_df` as the variable name. As such, it is possible 
#' to create exciting bugs and errors if care is not taken when running both
#' any part of this script and any of the other replication scripts. 
#' 
#' Input: 
#' - `data/item_edge_list_df.rds`
#' - `data/item_cite_df.rds`
#' 
#' Output: 
#' - 
#' 


# ---- Generative Modeling ----
library(tidyverse)
library(PAFit)
library(future)

path_prefix <- "MatthewEffectADVANCE/" # Set path as appropriate
source(paste0(path_prefix, "scripts/00_function_library.R"))

edge_list_df <- read_rds(paste0(path_prefix, "data/item_edge_list_df.rds"))

set.seed(2884)

if (!file.exists(paste0(path_prefix, "gen/item_model_list.rds"))) {
  # Running the model takes 20-30 second on an M1 Pro MacBook Pro
  model_list <- PAFit_model_pipeline(edge_list_df, "source_col", "target_col", "cite_year")
  
  write_rds(model_list, paste0(path_prefix, "gen/item_model_list.rds")) # Write to disk
} else {
  model_list <- read_rds(paste0(path_prefix, "gen/item_model_list.rds"))
}

if (!file.exists(paste0(path_prefix, "gen/item_simulation_obj.rds"))) {
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
  write_rds(simulation_obj, paste0(path_prefix, "gen/item_simulation_obj.rds")) # Write to disk
  
  end <- proc.time()
  end - start
} else {
  simulation_obj <- read_rds(paste0(path_prefix, "gen/item_simulation_obj.rds"))
}

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


# ---- Cumulative Citation Treatment Comparison ----
library(tidyverse)
library(MatchIt)
library(patchwork)

model_list <- read_rds(paste0(path_prefix, "gen/item_model_list.rds"))
cite_df <- read_rds(paste0(path_prefix, "data/item_cite_df.rds"))

# Add necessary metadata
cite_df <- add_PAFit_metadata(cite_df, model_pipeline_output = model_list, node_id_col = "target_col") %>%
  mutate(year_fac = as_factor(cite_year))

# ---- Prep Data for Graphing ---- 
# Create matched data for the solicits
temp_solicit <- create_matched_data_for_graphing(cite_df, "solicit", "exact")[["data"]]

# And create data for intersectionality
temp_intersectionality <- cite_df

temp_intersectionality <- temp_intersectionality %>%
  filter(!is.na(node_fitness))

levels(temp_solicit$solicit_ref) <- c("Authors Not Referenced in a Solicit", "Solicit References")

# ---- Do the Plotting ----
# accumulation of cites over time, centered on solicit appearance (or equivalent), faceted by solicit_ref
solicit_g1 <- temp_solicit %>% 
  ggplot(aes(x = centered_year, y = cumsum_cites, group = post_center)) + geom_jitter(alpha = .25) +
  facet_wrap(~solicit_ref) + geom_smooth(method = "lm", formula = y ~ x) + 
  xlab("Centered Year") + ylab("Cumulative Citations") + 
  geom_vline(xintercept = 0) + 
  ylim(0, 60) +
  custom_theme + 
  ggtitle("Treatment: Solicit Reference")

intersectional_g1 <- temp_intersectionality %>%
  mutate(node_fitness_group = case_when(node_fitness < 1 ~ "NF < 1", 
                                        node_fitness == 1 ~ "NF = 1", 
                                        node_fitness > 1 ~ "NF > 1")) %>%
  mutate(node_fitness_group = factor(node_fitness_group, levels = c("NF < 1", "NF = 1", "NF > 1")),
         intersectional = if_else(intersectional, "Intersectional Authors", "Non-Intersectional Authors")) %>%
  mutate(intersectional = factor(intersectional, levels = c("Non-Intersectional Authors", "Intersectional Authors"))) %>%
  mutate(centered_year = cite_year - 2016) %>%
  ggplot(aes(x = centered_year, y = cumsum_cites, group = post_2016)) + geom_jitter(alpha = .25) + 
  geom_smooth(formula = y ~ x, method = "lm", na.rm = TRUE) + geom_vline(xintercept = 0) + 
  facet_grid( ~ intersectional) + xlab("Centered Year") + ylab("Cumulative Citations") + 
  custom_theme + 
  ggtitle("Treatment: Intersectionality Requirement") 

# ---- Arrange and Display ----
(solicit_g1 / intersectional_g1) + 
  plot_layout(guides = "collect")


# ---- Estimate Treatment Effects ----
library(future)

source(paste0(path_prefix, "scripts/00_function_library.R"))

model_list <- read_rds(paste0(path_prefix, "gen/item_model_list.rds"))
cite_df <- read_rds(paste0(path_prefix, "data/item_cite_df.rds"))

# ---- Setup the Data ----
# Add necessary metadata
cite_df <- add_PAFit_metadata(cite_df, model_pipeline_output = model_list, node_id_col = "target_col") %>%
  mutate(year_fac = as_factor(cite_year))

#' Set up concurrency
plan(multisession, workers = 8)
future_seed <- 1234

# ---- Treatment Effect: Solicit Reference ----
n <- 1000 # number of bootstrap simulations for the confidence intervals

if (!file.exists(paste0(path_prefix, "gen/item_solicit_treat_sims.rds"))) {
  start <- proc.time()
  treatment_df <- create_treatment_df(cite_df = cite_df, pipeline_output = model_list, id_col = "target_col", treatment_col = "solicit_year")
  
  treated_stats <- treat_PAFit_model(model_list, treatment_df)
  treated_model <- fit_treated_model(treated_stats, pretreatment_model = model_list[["model_obj"]])
  
  treated_model$cv_data <- model_list[["model_obj"]][["cv_data"]]
  treated_model$cv_result <- model_list[["model_obj"]][["cv_result"]]
  
  treatment_df$f_pre <- treated_model$estimate_result$f[treatment_df$pre_id]
  treatment_df$f_post <- treated_model$estimate_result$f[treatment_df$post_id]
  
  # treated model sims 
  out <- generate_simulated_data_from_estimated_model_parallel(model_list$network_obj, model_list$stats_obj, model_list$model_obj, treatment_df, M = n)
  out <- sims_result_proc(out, treatment_df) # condense to save space
  
  end <- proc.time()
  
  write_rds(out, paste0(path_prefix, "gen/item_solicit_treat_sims.rds"))
  
  print(str_glue("{end[3] - start[3]} second elapsed."))
  
} 



# ---- Treatment Effect: Intersectionality ----
if (!file.exists(paste0(path_prefix, "gen/item_intersectional_treat_sims.rds"))) {
  cite_df <- cite_df %>% 
    filter(intersectional) %>%
    mutate(intersectional_year = 2016) # set the treatment year
  
  start <- proc.time()
  treatment_df <- create_treatment_df(cite_df = cite_df, pipeline_output = model_list, id_col = "target_col", treatment_col = "intersectional_year")
  
  treated_stats <- treat_PAFit_model(model_list, treatment_df)
  treated_model <- fit_treated_model(treated_stats, pretreatment_model = model_list[["model_obj"]])
  
  # treated_net <- treat_PAFit_network(model_list[["network_obj"]], treatment_df)
  
  treated_model$cv_data <- model_list[["model_obj"]][["cv_data"]]
  treated_model$cv_result <- model_list[["model_obj"]][["cv_result"]]
  
  treatment_df$f_pre <- treated_model$estimate_result$f[treatment_df$pre_id]
  treatment_df$f_post <- treated_model$estimate_result$f[treatment_df$post_id]
  
  # treated model sims # 1161 seconds
  out <- generate_simulated_data_from_estimated_model_parallel(model_list$network_obj, model_list$stats_obj, model_list$model_obj, treatment_df, M = n)
  out <- sims_result_proc(out, treatment_df) # condense to save space
  
  end <- proc.time()
  
  write_rds(out, paste0(path_prefix, "gen/item_intersectional_treat_sims.rds"))
  
  print(str_glue("{end[3] - start[3]} second elapsed."))
}


# ---- Treatment Effect Plots ----
# ---- Load Data ----
model_list <- read_rds(paste0(path_prefix, "gen/item_model_list.rds"))
cite_df <- read_rds(paste0(path_prefix, "data/item_cite_df.rds"))
solicit_sims <- read_rds(paste0(path_prefix, "gen/item_solicit_treat_sims.rds"))
intersectional_sims <- read_rds(paste0(path_prefix, "gen/item_intersectional_treat_sims.rds"))

cite_df <-  add_PAFit_metadata(cite_df, model_pipeline_output = model_list, node_id_col = "target_col") %>%
  mutate(year_fac = as_factor(cite_year))

# ---- Effect: Solicit ----
treatment_df <- create_treatment_df(cite_df, "target_col", "solicit_year", model_list)
treated_stats <- treat_PAFit_model(model_list, treatment_df) 
treated_model <- fit_treated_model(treated_stats, pretreatment_model = model_list[["model_obj"]])


nf_df <- cite_df %>% distinct(target_col, node_fitness, .keep_all = TRUE)

solicit_sims$career_comp <- solicit_sims$career_comp %>%
  mutate(post_career = if_else(post_career == 0, 1, post_career), 
         mean_post_cites = post_treat_cites / post_career) %>%
  mutate(cite_treat_effect = mean_post_cites - mean_pre_cites)

solicit_sims_summary <- summarize_treatment_effects(solicit_sims, treated_model, treatment_df)

solicit_sims_summary$original_nf <- nf_df[match(solicit_sims_summary$treated_id, nf_df$target_col), ]$node_fitness
solicit_cite_treat <- cite_treat_effect(treated_stats, treatment_df)
solicit_sims_summary$cite_treat_raw <- solicit_cite_treat[match(solicit_sims_summary$treated_id, solicit_cite_treat$treated_id), ]$cite_treat_effect

solicit_sims_summary <- solicit_sims_summary %>%
  arrange(desc(original_nf)) %>%
  mutate(index = 1:n())

# Create the plot
shapes_values <- c("Observed Effect" = 16, "Simulation Mean" = 4)

solicit_g3 <- solicit_sims_summary %>%
  ggplot(aes(x = factor(index), y = treat_diff, ymin = lwr, ymax = upr)) + geom_pointrange(aes(shape = "Observed Effect")) + 
  geom_point(aes(y = mean, shape = "Simulation Mean")) + 
  labs(shape = NULL) + scale_shape_manual(values = shapes_values) + 
  xlab("Authors by Decreasing Node Fitness") + ylab("Treatment Effect") + 
  custom_theme +
  theme(legend.position = "inside", legend.position.inside = c(.85, .9))

# Display the plot
solicit_g3


# ---- Effect: Intersectionality ----
cite_df <- cite_df %>% 
  mutate(intersectional_year = if_else(intersectional, 2016, NA))

treatment_df <- create_treatment_df(cite_df, "target_col", "intersectional_year", model_list)
treated_stats <- treat_PAFit_model(model_list, treatment_df) 
treated_model <- fit_treated_model(treated_stats, pretreatment_model = model_list[["model_obj"]])

intersectional_sims$career_comp <- intersectional_sims$career_comp %>%
  mutate(post_career = if_else(post_career == 0, 1, post_career), 
         mean_post_cites = post_treat_cites / post_career) %>%
  mutate(cite_treat_effect = mean_post_cites - mean_pre_cites)

intersectional_sims_summary <- summarize_treatment_effects(intersectional_sims, treated_model, treatment_df)

intersectional_sims_summary$original_nf <- nf_df[match(intersectional_sims_summary$treated_id, nf_df$target_col), ]$node_fitness
intersectional_cite_treat <- cite_treat_effect(treated_stats, treatment_df)
intersectional_sims_summary$cite_treat_raw <- intersectional_cite_treat[match(intersectional_sims_summary$treated_id, intersectional_cite_treat$treated_id), ]$cite_treat_effect

intersectional_sims_summary <- intersectional_sims_summary %>%
  arrange(desc(original_nf)) %>%
  mutate(index = 1:n())

# Create the plot
shapes_values <- c("Observed Effect" = 16, "Simulation Mean" = 4)

intersectional_g3 <- intersectional_sims_summary %>%
  ggplot(aes(x = factor(index), y = treat_diff, ymin = lwr, ymax = upr)) + geom_pointrange(aes(shape = "Observed Effect")) + 
  geom_point(aes(y = mean, shape = "Simulation Mean")) + 
  labs(shape = NULL) + scale_shape_manual(values = shapes_values) + 
  xlab("Authors by Decreasing Node Fitness") + ylab("Treatment Effect") + 
  custom_theme +
  theme(legend.position = "inside", legend.position.inside = c(.85, .9))

# Display the plot
intersectional_g3


# ---- Treatment Effect Summaries ----
solicit_diff_dist <- map2(solicit_sims_summary$treated_id, solicit_sims_summary$rand_diff, function(node_id, vals) {
  out <- tibble("vals" = vals)
  colnames(out) <- node_id
  return(out)
}) %>%
  list_cbind()

solicit_mean_diff_dist <- solicit_diff_dist %>%
  rowwise() %>%
  mutate(mean_diff = mean(c_across(everything()))) %>%
  pull(mean_diff)

solicit_diff_dist_cite <- map2(solicit_sims_summary$treated_id, solicit_sims_summary$cite_treat_effect, function(node_id, vals) {
  out <- tibble("vals" = vals)
  colnames(out) <- node_id
  return(out)
}) %>%
  list_cbind()

solicit_mean_diff_dist_cite <- solicit_diff_dist_cite %>%
  rowwise() %>%
  mutate(mean_diff = mean(c_across(everything()))) %>%
  pull(mean_diff)

intersectional_diff_dist <- map2(intersectional_sims_summary$treated_id, intersectional_sims_summary$rand_diff, function(node_id, vals) {
  out <- tibble("vals" = vals)
  colnames(out) <- node_id
  return(out)
}) %>%
  list_cbind()

intersectional_mean_diff_dist <- intersectional_diff_dist %>%
  rowwise() %>%
  mutate(mean_diff = mean(c_across(everything()))) %>%
  pull(mean_diff)

intersectional_diff_dist_cite <- map2(intersectional_sims_summary$treated_id, intersectional_sims_summary$cite_treat_effect, function(node_id, vals) {
  out <- tibble("vals" = vals)
  colnames(out) <- node_id
  return(out)
}) %>%
  list_cbind()

intersectional_mean_diff_dist_cite <- intersectional_diff_dist_cite %>%
  rowwise() %>%
  mutate(mean_diff = mean(c_across(everything()))) %>%
  pull(mean_diff)


treatment_nf_means <- tibble("intervention" = c("solicit", "intersectional"),
                             "observed_mean" = c(mean(solicit_sims_summary$treat_diff), mean(intersectional_sims_summary$treat_diff)), 
                             "lwr" = c(quantile(solicit_mean_diff_dist, .025), quantile(intersectional_mean_diff_dist, .025)), 
                             "upr" = c(quantile(solicit_mean_diff_dist, .975), quantile(intersectional_mean_diff_dist, .975)), 
                             "simulation_mean" = c(mean(solicit_mean_diff_dist), mean(intersectional_mean_diff_dist)))

treatment_cite_means <- tibble("intervention" = c("solicit", "intersectional"),
                               "observed_mean" = c(mean(solicit_sims_summary$cite_treat_raw), mean(intersectional_sims_summary$cite_treat_raw)), 
                               "lwr" = c(quantile(solicit_mean_diff_dist_cite, .025, na.rm = TRUE), quantile(intersectional_mean_diff_dist_cite, .025, na.rm = TRUE)), 
                               "upr" = c(quantile(solicit_mean_diff_dist_cite, .975, na.rm = TRUE), quantile(intersectional_mean_diff_dist_cite, .975, na.rm = TRUE)), 
                               "simulation_mean" = c(mean(solicit_mean_diff_dist_cite, na.rm = TRUE), mean(intersectional_mean_diff_dist_cite, na.rm = TRUE)))

treat_cors <- tibble("intervention" = c("solicit", "intersectional"),
                     "observed_cor" = c(cor(solicit_sims_summary$treat_diff, solicit_sims_summary$cite_treat_raw), 
                                        cor(intersectional_sims_summary$treat_diff, intersectional_sims_summary$cite_treat_raw)),
                     "lwr" = c(solicit_sims_summary$cor_lwr[1], intersectional_sims_summary$cor_lwr[1]),
                     "upr" = c(solicit_sims_summary$cor_upr[1], intersectional_sims_summary$cor_upr[1]), 
                     "simulation_mean" = c(solicit_sims_summary$cor_mean[1], intersectional_sims_summary$cor_mean[1]))

