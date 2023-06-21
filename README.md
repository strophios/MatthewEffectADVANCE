# MatthewEffectADVANCE

This repository contains replication materials for the paper "Does an Equity-Minded Organization Produce Equity-Minded Science? The Matthew Effect in NSF ADVANCE Publications", by Steven Lauterwasser (Northeastern University), Laura K. Nelson (University of British Columbia), Jessica R. Gold (Northeastern University), and Kathrin Zippel (Free University of Berlin). 


# Contents

This repo allows for the replication of all figures and tables from the paper, but does not contain the raw underlying data. Some of this data cannot be included because it straightforwardly is not public. Even that which is publicly available (or derived from publicly available sources), however, is not included. We have chosen to obfuscate the underlying data due to the current political climate around feminist work in organizations (especially universities), judging that this context makes the public presentation of a database of such work (and its authors) unwise. 

We have provided two alternatives: 

- `/data/outcome_mag_ids.csv` contains the Microsoft Academic Graph (MAG) IDs for the outcome documents which are the basis for most our data. We believe that this balances obfuscation and transparency, such that interested scholars can still investigate the underlying data. 
- The remaining files in the `/data` directory are anonymized and/or aggregated "last step" data which is directly necessary for each of the replicated figures or tables. Generally this this is a CSV of the aggregated counts which a given figure or table displays. The only exception to this is the data for generative network modeling, for which the unlabeled raw edge lists are provided. 

The actual code for replicating the figures and tables are contained in the R script files in `/scripts`:

- `citation_statistics.R` replicates figures 1 and 6, as well as table 1, i.e., the results based purely on basic summary aggregations from the underlying data.
- `generative_modeling.R` replicates figures 2 through 5, performing the underlying modeling and running the necessary simulations. For details on the methods, see the paper and/or the package PAFit and its documentation.
  - `sim_f_net.R` provides a supporting function used in running the simulations which form the basis of figure 5. 
- `intersectionality.R` replicates figures 7 through 9. 
- `appendix_auth_results.R` replicates figures 11 through 16. These are the results based on aggregating cites at the level of the author rather than the work, presented in appendix C of the paper.
  - Note that figure 10 from appendix B is not replicated, being itself a copy of figure 2.

As written, these scripts do not write the replicated figures and tables to disk, but merely output them to R's default graphical output. If you would like to alter them to do so, see the documentation for ggplot2 (for the figures) and kableExtra (for the tables). 

Each of these scripts can be run independently (except for `sim_f_net.R`), requiring only an installation of R and the following libraries:

- tidyverse
- kableExtra
- PAFit




