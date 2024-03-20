#' Title: cumcite_treatment_comp.R
#' Author: Steven Lauterwasser
#' 
#' Summary: This script replicates figure 2, the comparison of cumulative
#' citations over time for treated vs. untreated authors. 
#' 
#' Input: 
#' - `data/cite_df.rds`
#' - `gen/model_list.rds`
#' 
#' Output (does not write to disk by default, simply displays the outputs): 
#' - Figure 2: Cumulative citations by year, treated vs. untreated authors.
#' 
#' Full caption: Cumulative citations by year, treated vs. untreated authors. 
#' Row one shows the comparison for the solicit reference treatment and row two 
#' shows the same for the intersectionality treatment. Each dot represents the 
#' citations to an author in the given year. Year centered such that treatment 
#' occurred in year zero, marked by a vertical black line. Regression lines fit 
#' to the pre- and post-treatment periods in each panel are shown in blue. The 
#' average rate of citation accumulation increased more for treated authors than 
#' untreated authors, with a larger difference for the solicit reference treatment. 
#' Source: Corpus of ADVANCE outcome publications, collected by the authors.
#' 

library(tidyverse)
library(MatchIt)
library(patchwork)

# ---- Setup the Data ----
path_prefix <- "~/ADVANCE_postdoc/wgs_corpus/01_citational_inequality_paper/MatthewEffectADVANCE/"

model_list <- read_rds(paste0(path_prefix, "gen/model_list.rds"))
cite_df <- read_rds(paste0(path_prefix, "data/cite_df.rds"))

# Add necessary metadata
cite_df <- add_PAFit_metadata(cite_df, model_pipeline_output = model_list, node_id_col = "target_col") %>%
  mutate(year_fac = as_factor(cite_year))

# ---- Function for Creating Matched Data ----
create_matched_data_for_graphing <- function(data, mode = c("exact", "coarse", "exact_plus")) {
  
  mode <- match.arg(mode)
  id_col <- "target_col"
  colnames(data)[colnames(data) == id_col] <- "id_col"
  
  data_temp <- data %>%
    group_by(id_col) %>%
    nest() %>%
    mutate(data = map(data, function(x) { # create a new centered year which starts at 0 for everyone
      x[["new_centered"]] <- 1:nrow(x) - 1
      return(x)
    })) %>%
    unnest(cols = c(data)) %>%
    ungroup() %>%
    filter(!solicit_ref | (solicit_ref & cite_year == solicit_year)) # drop all the solicit ref source-years which are not their year of solicit appearance
    
  # all three versions produce good results for the data we're considering here. in fact, the other two are actually *stronger* 
  if (mode == "exact") {
    data_match <- matchit(formula = solicit_ref ~ new_centered + cumsum_A, data = data_temp, # current option, leaves 10 unmatched, var ratios of 1.02, 0 std mean diff
                          method = "exact", distance = "glm")
  } else if (mode == "coarse") {
    data_match <- matchit(formula = solicit_ref ~ new_centered + cumsum_A, data = data_temp, # none unmatched, var ratios of .93 and .84, small std mean diffs
                          method = "cem", distance = "glm")
  } else if (mode == "exact_plus") {
    data_match <- matchit(formula = solicit_ref ~ new_centered + cumsum_A, data = data_temp, # none unmatched (but it's only 1-to-1), var ratios  ~1, very small mean diffs
                          method = "nearest", exact = "new_centered", distance = "glm")
  }
  
  # quick diagnostic (since I haven't bothered delving into the quality of matching for every network separately)
  match_summary <- summary(data_match)
  
  # for testing matching options
  # return(match_summary)
  
  if ((match_summary[["nn"]][5, 2] / match_summary[["nn"]][4, 2]) >= .2) { # is proportion of dropped treated items too large?
    warning(str_glue("Matching dropped more than 1/5 of treated units."))
  } else if (any(match_summary$sum.matched[, "Var. Ratio"] > 1.5 | 
                 match_summary$sum.matched[, "Var. Ratio"] < .5, na.rm = TRUE)) { # any Var.Ratios out of whack?
    which_var_ratio <- rownames(match_summary[["sum.matched"]])[which(match_summary[["sum.matched"]][, "Var. Ratio"] > 1.5 | 
                                                                        match_summary[["sum.matched"]][, "Var. Ratio"] < .5)]
    warning(str_glue("Problem var ratio for {paste(which_var_ratio, collapse = ', ')}."))
  }
  
  matched_data <- match.data(data_match) 
  
  matched_data <- list_rbind(map(unique(matched_data[["subclass"]]), function(x) {
    
    subclass_data <- data %>% 
      filter(id_col %in% matched_data[matched_data[["subclass"]] == x, ][["id_col"]])
    
    subclass_data <- subclass_data %>% 
      mutate(centered_year = if_else(solicit_ref, centered_year, centered_year + min(subclass_data[["centered_year"]], na.rm = TRUE)))
    
    subclass_data[["subclass"]] <- x
    
    # ignoring weights since, with exact matching, they're only relevant for estimating treatment effects, which we're not really doing
    
    subclass_data <- bind_rows(list(subclass_data %>% filter(solicit_ref) %>%
                                      select(id_col, subclass, solicit_ref, centered_year, n, cumsum_cites,
                                             node_fitness, current_A, cumsum_A),
                                    subclass_data %>% filter(!solicit_ref) %>%
                                      group_by(centered_year) %>%
                                      summarize(across(c("n", "cumsum_cites", "node_fitness", "current_A", "cumsum_A"), ~mean(.x, na.rm = TRUE)),
                                                across(c("id_col", "solicit_ref", "subclass"), ~first(.x))) %>%
                                      ungroup()))
    
    return(subclass_data)
    
  }))
  
  matched_data <- matched_data %>%
    # arrange(desc(subclass), id_col, centered_year) %>%
    # arrange(subclass, id_col, centered_year) %>%
    # distinct(id_col, centered_year, .keep_all = TRUE) %>%
    mutate(post_center = factor(if_else(centered_year > 0, "Post", "Pre"), levels = c("Pre", "Post")), 
           solicit_ref = factor(if_else(solicit_ref, "Solicit Ref", "Other Source"), levels = c("Other Source", "Solicit Ref"))) #%>%
  
  return(matched_data)
  
}

# ---- Prep Data for Graphing ---- 
# Create matched data for the solicits
temp_solicit <- create_matched_data_for_graphing(cite_df)

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
