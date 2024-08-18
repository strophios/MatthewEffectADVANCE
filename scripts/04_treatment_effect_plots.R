#' Title: treatment_effect_plots.R
#' Author: Steven Lauterwasser
#' 
#' Summary: This script summarizes the treatment effects on node fitness and citations per 
#' year. The node fitness effects are used to replicate figures 6.3 and 6.4, plotting the 
#' effects due to the solicit reference and intersectionality intervention, respectively.
#' It also estimates the correlation between citation treatment effects and the node fitness 
#' treatment effects, which are presented in section 6.2.4 of the findings.
#' 
#' Input: 
#' - `gen/model_list.rds` (created by `01_generative_modeling.R`)
#' - `gen/solicit_treat_sims.rds` (created by `03_estimate_treatment_effects.R`)
#' - `gen/intersectional_treat_sims.rds` (created by `03_estimate_treatment_effects.R`)
#' - `data/cite_df.rds`
#' - `script/00_function_library.R`
#' 
#' Output: (not saved by default)
#' - Figures 6.3 and 6.4
#' - Summary dataframes of the treatment effects and their correlation, including the
#' observed values, confidence intervals based on the simulations, and the simulation mean: 
#'   - `treatment_nf_means`: node fitness treatment effect means.
#'   - `treatment_cite_means`: citations per year treatment effect means. 
#'   - `treat_cors`: correlation between the node fitness and citations per year treatment effects.
#' 

library(tidyverse)
library(PAFit)

path_prefix <- "MatthewEffectADVANCE/" # Set path as appropriate

source(paste0(path_prefix, "scripts/00_function_library.R"))

# ---- Load Data ----
model_list <- read_rds(paste0(path_prefix, "gen/model_list.rds"))
cite_df <- read_rds(paste0(path_prefix, "data/cite_df.rds"))
solicit_sims <- read_rds(paste0(path_prefix, "gen/solicit_treat_sims.rds"))
intersectional_sims <- read_rds(paste0(path_prefix, "gen/intersectional_treat_sims.rds"))

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



