# MatthewEffectADVANCE

This repository contains replication materials for the paper "Talents for the Talented: Disrupting the Matthew Effect in an NSF Award Program", by Steven Lauterwasser (Northeastern University), Laura K. Nelson (University of British Columbia), Jessica R. Gold (Northeastern University), and Kathrin Zippel (Freie Universität Berlin). 


# Contents

This repo allows for the replication of all results from the paper, but does not contain the raw underlying data. Some of this data cannot be included because it straightforwardly is not public. Even that which is publicly available (or derived from publicly available sources) is not included, however. We have chosen to obfuscate the underlying data due to the current political climate around feminist work in organizations (especially universities), judging that this context makes the public presentation of a database of such work (and its authors) unwise. 

We have provided two alternatives: 

- `/data/outcome_mag_ids.csv` contains the Microsoft Academic Graph (MAG) IDs for the outcome documents which are the basis for most our data. We believe that this balances obfuscation and transparency, such that interested scholars can still investigate the underlying data. Note that, while the MAG may be defunct, [OpenAlex](https://openalex.org) is an open source alternative. It has incorporated much of the MAG's data using the original IDs. 
- The remaining files in the `/data` directory are anonymized and/or aggregated "last step" data which is directly necessary for the results. Specifically, this includes an anonymized version of the author/item aggregated citations per year data and the raw edge lists for the generative network modeling. 

The actual code for replicating the figures and tables are contained in the R script files in `/scripts`:

- `00_function_library.R` holds the custom functions used by the rest of the replication code.
- `01_generative_modeling.R` performs the generative network modeling that provides the basis for the remaining results. Also produces figure 6.1.
- `02_cumcite_treatment_comps.R` replicates figure 6.2, the comparison of cumulative citations over time for treated vs. untreated authors.
- `03_estimate_treatment_effects.R` estimates the node fitness treatment effects and runs bootstrap simulations to assess uncertainty.
- `04_treatment_effect_plots.R` summarizes the results of the effect estimates (on both node fitness and citations per year) and bootstrap simulations. Also replicates figures 6.3 and 6.4.
- `05_cite_effect_plots.R` replicates all figures from appendix D.
- `06_item_network_results.R` replicates all figures from appendix E.

The results for some of the more computationally intensive steps (i.e., the modeling or bootstrap simulations) are written to disk (`/gen` by default). This includes some or all of the output from scripts `01_generative_modeling.R`, `03_estimate_treatment_effects.R`, and `06_item_network_results.R`. All remaining outputs (including all figures) are not saved by default. 

The required libraries and inter-script dependencies should be made clear within each individual script.




