#' Title: 05_cite_effect_plots.R
#' Author: Steven Lauterwasser
#' 
#' Summary: This script replicates figures D.1-4 from Appendix D, which visualize the 
#' correlations between node fitness and citation treatment effects. D.1 and D.2 simply
#' display the effects and correlations already estimated. D.3 and D.4, however, show 
#' the results of an alternative method for calculating the citation effects, a 
#' difference-in-difference with matched controls design (additional details in appendix D). 
#' 
#' Input: 
#' - `script/04_treatment_effect_plots.R`: this script needs simulation summary dataframes
#'   and node fitness treatment effects estimated in `04_treatment_effects_plots.R`. 
#' - The only other required inputs are those required by the previous script. 
#' 
#' Output: 
#' - Figures D.1-4
#' 

library(MatchIt)
library(marginaleffects)

path_prefix <- "MatthewEffectADVANCE/" # Set path as appropriate

#' Check for outputs from `04_treatment_effect_plots.R`. 
#' If present, the script assumes all its required inputs
#' are presenet. Otherwise, it sources `04_treatment_effect_plots.R`.
if (!exists("treatment_nf_means")) {
  source(paste0(path_prefix, "scripts/04_treatment_effect_plots.R"))
}



# ---- Pre-/Post-Treatment Citation Treatment Effects ----
# For Solicit Appearance Intervention
solicit_sims_summary %>%
  select(-any_of(c("pre", "post", "cite_treat_effect", "rand_diff"))) %>%
  ggplot(aes(x = treat_diff, y = cite_treat_raw)) + geom_point() + geom_smooth(method = "lm") +
  custom_theme + 
  theme(axis.text.x = element_text(), 
        axis.ticks.x = element_line()) + 
  xlab("Node Fitness Effect") + ylab("Citation Effect")


# For Intersectionality Requirement Intervention
intersectional_sims_summary %>%
  select(-any_of(c("pre", "post", "cite_treat_effect", "rand_diff"))) %>%
  ggplot(aes(x = treat_diff, y = cite_treat_raw)) + geom_point() + geom_smooth(method = "lm") +
  custom_theme + 
  theme(axis.text.x = element_text(), 
        axis.ticks.x = element_line()) + 
  xlab("Node Fitness Effect") + ylab("Citation Effect")


# ---- Difference-in-Difference with Matched Controls ----

# Solicitation Intervention
temp_solicit <- create_matched_data_for_graphing(cite_df, "solicit", "coarse", TRUE, TRUE)

temp_solicit_subclasses <- temp_solicit[["subclasses"]]

temp_solicit <- temp_solicit[["data"]] %>%
  mutate(solicit_ref = solicit_ref == "Solicit Ref") %>%
  mutate(post_center = post_center == "Post")

temp_solicit <- temp_solicit %>%
  mutate(did = solicit_ref*post_center)

temp_model <- glm(n ~ solicit_ref * cumsum_A + post_center + did, data = temp_solicit, weights = weights, family = "poisson")

effect_estimates <- marginaleffects::avg_comparisons(temp_model, variables = "did", by = "subclass",
                                                     vcov = ~subclass, wts = "weights")

effect_estimates <- map2(temp_solicit_subclasses$id_col, temp_solicit_subclasses$subclass, function(x, y) {
  treat_effect <- solicit_sims_summary[solicit_sims_summary[["treated_id"]] == x, ][["treat_diff"]]
  cite_effect <- effect_estimates[as.numeric(effect_estimates[["subclass"]]) == as.numeric(y), ][["estimate"]]
  
  return(tibble("treated_id" = x, 
                "subclass" = y, 
                "treat_effect" = treat_effect,
                "cite_effect" = cite_effect))
}) %>%
  list_rbind()

effect_estimates %>%
  ggplot(aes(x = treat_effect, y = cite_effect)) + geom_point() + geom_smooth(method = "lm") + 
  custom_theme + 
  theme(axis.text.x = element_text(), 
        axis.ticks.x = element_line()) + 
  xlab("Node Fitness Effect") + ylab("Citation Effect")


# Intersectionality Intervention
# note: nearest neighbor matching produces better balance, but coarsened exact matching is what we did for the solicit intervention, so we're currently going with the latter
# the results are substantively similar though
temp_intersectional <- create_matched_data_for_graphing(cite_df, "intersectional", "coarse", TRUE, TRUE)

temp_intersectional_subclasses <- temp_intersectional[["subclasses"]]

temp_intersectional <- temp_intersectional[["data"]] %>%
  mutate(post_center = post_center == "Post")

temp_intersectional <- temp_intersectional %>%
  mutate(did = intersectional*post_center)

temp_model <- glm(n ~ intersectional * cumsum_A + post_center + did, data = temp_intersectional, weights = weights, 
                  family = "poisson")

effect_estimates <- marginaleffects::avg_comparisons(temp_model, variables = "did", by = "subclass",
                                                     vcov = ~subclass, 
                                                     wts = "weights"
)

effect_estimates <- map2(temp_intersectional_subclasses$id_col, temp_intersectional_subclasses$subclass, function(x, y) {
  treat_effect <- intersectional_sims_summary[intersectional_sims_summary[["treated_id"]] == x, ][["treat_diff"]]
  cite_effect <- effect_estimates[as.numeric(effect_estimates[["subclass"]]) == as.numeric(y), ][["estimate"]]
  
  return(tibble("treated_id" = x, 
                "subclass" = y, 
                "treat_effect" = treat_effect,
                "cite_effect" = cite_effect))
}) %>%
  list_rbind()

effect_estimates %>%
  ggplot(aes(x = treat_effect, y = cite_effect)) + geom_point() + geom_smooth(method = "lm") + 
  custom_theme + 
  theme(axis.text.x = element_text(), 
        axis.ticks.x = element_line()) + 
  xlab("Node Fitness Effect") + ylab("Citation Effect")









