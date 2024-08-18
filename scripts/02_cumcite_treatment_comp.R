#' Title: 02_cumcite_treatment_comp.R
#' Author: Steven Lauterwasser
#' 
#' Summary: This script replicates figure 6.2, the comparison of cumulative
#' citations over time for treated vs. untreated authors. 
#' 
#' Input: 
#' - `data/cite_df.rds`
#' - `gen/model_list.rds`
#' 
#' Output (does not write to disk by default, simply displays the outputs): 
#' - Figure 2: Cumulative citations by year, treated vs. untreated authors.
#' 

library(tidyverse)
library(MatchIt)
library(patchwork)

# ---- Setup the Data ----
path_prefix <- "MatthewEffectADVANCE/" # Set path as appropriate

source(paste0(path_prefix, "scripts/00_function_library.R"))

model_list <- read_rds(paste0(path_prefix, "gen/model_list.rds"))
cite_df <- read_rds(paste0(path_prefix, "data/cite_df.rds"))

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
  theme(panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ylim(NA, 190) + 
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
  theme(panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  ggtitle("Treatment: Intersectionality Requirement") 

# ---- Arrange and Display ----
(solicit_g1 / intersectional_g1) + 
  plot_layout(guides = "collect")


