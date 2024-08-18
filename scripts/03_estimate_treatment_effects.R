#' Title: 03_estimate_treatment_effects.R
#' Author: Steven Lauterwasser
#' 
#' Summary: This script estimates the treatment effects and runs bootstrap re-simulations
#' for confidence intervals. Creating the bootstrap resimulations takes between 30 minutes
#' and an hour on a well spec'd M1 MacBook Pro.
#' 
#' Input: 
#' - `gen/model_list.rds`
#' - `data/cite_df.rds`
#' - `script/00_function_library.R`
#' 
#' Output: 
#' - `gen/solicit_treat_sims.rds`
#' - `gen/intersectional_treat_sims.rds`
#' 

library(tidyverse)
library(PAFit)
library(future)

path_prefix <- "MatthewEffectADVANCE/" # Set path as appropriate

source(paste0(path_prefix, "scripts/00_function_library.R"))

model_list <- read_rds(paste0(path_prefix, "gen/model_list.rds"))
cite_df <- read_rds(paste0(path_prefix, "data/cite_df.rds"))

# ---- Setup the Data ----
# Add necessary metadata
cite_df <- add_PAFit_metadata(cite_df, model_pipeline_output = model_list, node_id_col = "target_col") %>%
  mutate(year_fac = as_factor(cite_year))

#' Set up concurrency
plan(multisession, workers = 8)
future_seed <- 1234

# ---- Treatment Effect: Solicit Reference ----
n <-  1000 # number of bootstrap simulations for the confidence intervals

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

write_rds(out, paste0(path_prefix, "gen/solicit_treat_sims.rds"))

print(str_glue("{end[3] - start[3]} second elapsed."))


# ---- Treatment Effect: Intersectionality ----
cite_df <- cite_df %>% 
  filter(intersectional) %>%
  mutate(intersectional_year = 2016) # set the treatment year

start <- proc.time()
treatment_df <- create_treatment_df(cite_df = cite_df, pipeline_output = model_list, id_col = "target_col", treatment_col = "intersectional_year")

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

write_rds(out, paste0(path_prefix, "gen/intersectional_treat_sims.rds"))

print(str_glue("{end[3] - start[3]} second elapsed."))

