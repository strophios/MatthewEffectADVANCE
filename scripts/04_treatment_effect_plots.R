#' Title: treatment_effect_plots.R
#' Author: Steven Lauterwasser
#' 
#' Summary: This script replicates figures 3 and 4, which plot the estimated treatment 
#' effects due to the solicit reference and intersectionality intervention, respectively.
#' 
#' Input: 
#' - `gen/model_list.rds`
#' - `data/cite_df.rds`
#' - `script/generate_simulated_data_from_estimated_model_parallel.R`
#' 
#' Output: 
#' - Figures 3 and 4
#' - `treatment_means_df`: a summary dataframe of treatment effect means, with confidence intervals. Not saved by default. 

library(tidyverse)
library(PAFit)

path_prefix <- "~/ADVANCE_postdoc/wgs_corpus/01_citational_inequality_paper/MatthewEffectADVANCE/" # *remember to change/omit this

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

solicit_sims_summary <- f_summarize(solicit_sims[["f"]], treated_model, treatment_df)

nf_df <- cite_df %>% distinct(target_col, node_fitness, .keep_all = TRUE)

solicit_sims_summary$original_nf <- nf_df[match(solicit_sims_summary$treated_id, nf_df$target_col), ]$node_fitness
solicit_nfs <- NF_prepost_comp(solicit_sims_summary)

solicit_diff_dist <- map2(solicit_nfs$treated_id, solicit_nfs$rand_diff, function(node_id, vals) {
  out <- tibble("vals" = vals)
  colnames(out) <- node_id
  return(out)
  }) %>%
  list_cbind()

solicit_mean_diff_dist <- solicit_diff_dist %>%
  rowwise() %>%
  mutate(mean_diff = mean(c_across(everything()))) %>% 
  pull(mean_diff)

solicit_nfs <- solicit_nfs %>%
  arrange(by = desc(original_nf)) %>%
  mutate(index = 1:n())

shapes_values <- c("Observed Effect" = 16, "Simulation Mean" = 4)

# Create the plot
solicit_g3 <- solicit_nfs %>%
  ggplot(aes(x = factor(index), y = treat_diff, ymin = lwr, ymax = upr)) + geom_pointrange(aes(shape = "Observed Effect")) + 
  geom_point(aes(y = mean, shape = "Simulation Mean")) + 
  labs(shape = NULL) + scale_shape_manual(values = shapes_values) + 
  xlab("Authors by Decreasing Node Fitness") + ylab("Treatment Effect") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.position.inside = c(.85, .9), 
        panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

# Display the plot
solicit_g3

# ---- Effect: Intersectionality ----
cite_df <- cite_df %>% 
  mutate(intersectional_year = if_else(intersectional, 2016, NA))

treatment_df <- create_treatment_df(cite_df, "target_col", "intersectional_year", model_list)
treated_stats <- treat_PAFit_model(model_list, treatment_df) 
treated_model <- fit_treated_model(treated_stats, pretreatment_model = model_list[["model_obj"]])

intersectional_sims_summary <- f_summarize(intersectional_sims[["f"]], treated_model, treatment_df)

intersectional_sims_summary$original_nf <- nf_df[match(intersectional_sims_summary$treated_id, nf_df$target_col), ]$node_fitness

intersectional_nfs <- NF_prepost_comp(intersectional_sims_summary)

intersectional_diff_dist <- map2(intersectional_nfs$treated_id, intersectional_nfs$rand_diff, function(node_id, vals) {
  out <- tibble("vals" = vals)
  colnames(out) <- node_id
  return(out)
  }) %>%
  list_cbind()

intersectional_mean_diff_dist <- intersectional_diff_dist %>%
  rowwise() %>%
  mutate(mean_diff = mean(c_across(everything()))) %>% 
  pull(mean_diff)

intersectional_nfs <- intersectional_nfs %>%
  arrange(by = desc(original_nf)) %>%
  mutate(index = 1:n())

# Create the Plot
shapes_values <- c("Observed Effect" = 16, "Simulation Mean" = 4)

intersectional_g3 <- intersectional_nfs %>%
  ggplot(aes(x = factor(index), y = treat_diff, ymin = lwr, ymax = upr)) + geom_pointrange(aes(shape = "Observed Effect")) + 
  geom_point(aes(y = mean, shape = "Simulation Mean")) + 
  labs(shape = NULL) + scale_shape_manual(values = shapes_values) + 
  xlab("Authors by Decreasing Node Fitness") + ylab("Treatment Effect") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.position.inside = c(.85, .9), 
        panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

# Display the Plot
intersectional_g3

# ---- Summary DF ----
treatment_means_df <- tibble("intervention" = c("solicit", "intersectional"),
                             "observed_mean" = c(mean(solicit_nfs$treat_diff), mean(intersectional_nfs$treat_diff)), 
                             "lwr" = c(quantile(solicit_mean_diff_dist, .025), quantile(intersectional_mean_diff_dist, .025)), 
                             "upr" = c(quantile(solicit_mean_diff_dist, .975), quantile(intersectional_mean_diff_dist, .975)), 
                             "simulation_mean" = c(mean(solicit_mean_diff_dist), mean(intersectional_mean_diff_dist)))



