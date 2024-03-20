#' Author: Steven Lauterwasser
#' 
#' Summary: Contains the custom functions used in the replication code
#' 
#' 

# ---- PAFit Modeling Pipeline ----
#' Functions to go from an edge list to an estimated General Temporal Model fit with PAFit

add_zero_edge_nodes <- function(pre_net_df, source_col, target_col, time_col, app_col) {
  #' Add rows representing nodes with no edges
  #' 
  #' @param pre_net_df An edge-list dataframe with additional columns for the necessary metadata. 
  #' @param source_col a string naming the column of source nodes. 
  #' @param target_col a string naming the column of target nodes. 
  #' @param time_col a string naming the column setting the time step of each edge. 
  #' @param app_col a string naming the "appearance" column, or the column of metadata to compare
  #' `time_col` against in order to find nodes which should pre-exist their first edge.
  #' 
  #' @return An edge-list dataframe with additional rows for zero edge nodes, where 
  #' the `target_col == "-1"`. 
  #' 
  
  # do some type fixing
  pre_net_df[[source_col]] <- as.character(pre_net_df[[source_col]])
  pre_net_df[[target_col]] <- as.character(pre_net_df[[target_col]])
  pre_net_df[[time_col]] <- as.numeric(pre_net_df[[time_col]])
  pre_net_df[[app_col]] <- as.numeric(pre_net_df[[app_col]])
  
  network_start <- min(pre_net_df[[time_col]])
  
  zero_edge_nodes <- map_dfr(unique(pre_net_df[[target_col]]), function(x) {
    temp <- pre_net_df[pre_net_df[[target_col]] == x, ]
    
    first_edge <- min(temp[[time_col]])
    
    # check whether we have a value for first appearance (e.g., we don't have a pub year for some sources)
    if (all(is.na(temp[[app_col]]))) {
      return(tibble("s_col" = NA, 
                    "t_col" = NA,
                    "ti_col" = NA,
                    "a_col" = NA))
    }
    
    first_app <- min(temp[[app_col]], na.rm = TRUE)
    
    # if the target also shows up as a source, check and make sure that that 
    # doesn't pre-date or match the first_app. if it does, then it enters the 
    # network as a source node (with edges it originates)
    if (x %in% pre_net_df[[source_col]]) {
      first_source <- min(pre_net_df[pre_net_df[[source_col]] == x, ][[time_col]])
      
      # if it does pre-date or match, then we just return an empty row)
      if (first_source <= first_app) {
        return(tibble("s_col" = NA, 
                      "t_col" = NA,
                      "ti_col" = NA,
                      "a_col" = NA))
      }
      
    }
    
    # now we can check whether the first appearance precedes the first edge
    if (first_app < first_edge) {
      # if so, then we create a row indicating that the node entered the network
      # but had no edges, at time = first_app
      return(tibble("s_col" = x, 
                    "t_col" = "-1",
                    "ti_col" = max(first_app, network_start), # the node can't appear before the network starts
                    "a_col" = first_app))
    } else {
      # otherwise, return an empty row
      return(tibble("s_col" = NA, 
                    "t_col" = NA,
                    "ti_col" = NA,
                    "a_col" = NA))
    }
  }) %>%
    filter(!is.na(s_col)) # and now drop the empty rows
  
  # rename columns to match the original dataframe  
  colnames(zero_edge_nodes) <- c(source_col, target_col, 
                                 time_col, app_col)
  
  pre_net_df <- bind_rows(pre_net_df, zero_edge_nodes)
  
  return(pre_net_df)
  
}


citation_network_pipeline <- function(cite_df, corpus, level, cite_type, node_entry) {
  #' From df of citations to an edge-list df ready for modeling
  #' 
  #' @param cite_df a dataframe of citations, one per row. 
  #' @param corpus a string indicating whether to include all citations or only those to WGS.
  #' @param level a string indicating the level of aggregation: item or author. 
  #' @param cite_type a string indicating whether to include "duplicate" cites or just "unique" cites. 
  #' @param node_entry a string indicating whether and how to add zero edge nodes. 
  #' 
  #' @return An edge-list dataframe. 
  #' 
  
  if (corpus == "wgs") {
    cite_df <- cite_df %>% 
      filter(wgs)
  }
  
  if (level == "item") {
    cite_df <- cite_df %>%
      select(all_of(c("cited_doc", "citing_doc", "pub_year", "cite_year", "first_when"))) %>%
      mutate(pub_year = as.numeric(str_extract(pub_year, "^\\d{4}"))) #%>% 
    #rename(cited_doc = source_id)
    
    source_col <- "citing_doc"
    target_col <- "cited_doc"
    
    if (cite_type == "uniq") {
      cite_df <- cite_df %>%
        distinct(cited_doc, citing_doc, .keep_all = TRUE)
    }
    
  } else if (level == "author") {
    cite_df <- cite_df %>%
      select(all_of(c("cited_doc", "citing_doc", "pub_year", "cite_year", "first_when", "cited_author_list", "first_when"))) %>%
      mutate(pub_year = as.numeric(str_extract(pub_year, "^\\d{4}"))) %>% 
      mutate(citing_author_list = map(citing_doc, ~combined_metadata[combined_metadata$pub_id == .x, ]$author_list[[1]]))# %>%
    #rename(cited_author_list = author_list, cited_doc = source_id)
    
    source_col <- "citing_author"
    target_col <- "cited_author"
    
    if (cite_type == "uniq") {
      cite_df <- cite_df %>%
        distinct(cited_doc, citing_doc, .keep_all = TRUE)
    }
    
    cite_df <- cite_df %>%
      mutate(citing_author_list = map(citing_author_list, function(x) {
        if (nrow(x) == 0) {
          return(tibble("full_name" = ""))
        }
        return(x)
      })) %>%
      mutate(cited_author_list = map(cited_author_list, ~.x %>% select(cited_author = full_name)),
             citing_author_list = map(citing_author_list, ~.x %>% select(citing_author = full_name))) %>%
      mutate(author_edges = map2(cited_author_list, citing_author_list, function(x, y) {
        out <- bind_rows(x, y) %>% 
          expand(cited_author, citing_author) %>%
          filter(!is.na(cited_author) & 
                   !is.na(citing_author))
        
        return(out)
      })) %>%
      select(-any_of(c("cited_author", "citing_author"))) %>%
      unnest(author_edges) %>%
      filter(!(cited_author == "" | citing_author == ""))
  }
  
  if (node_entry == "exist") {
    cite_df <- add_zero_edge_nodes(cite_df, source_col, target_col, time_col = "cite_year", app_col = "pub_year")
  } else if (node_entry == "ADVANCE") {
    cite_df <- add_zero_edge_nodes(cite_df, source_col, target_col, time_col = "cite_year", app_col = "first_when")
  } else if (node_entry == "edge") {
    
  }
  
  
  return(cite_df)  
  
  
}


edge_list_to_PAFit_net <- function(edge_list_df, source_col = "citing_doc", target_col = "cited_doc", time_col = "cite_year") {
  #' Edge-list df to PAFit network object and node_ids df
  #' 
  #' @param edge_list_df an edge-list dataframe, assumed to be output of `citation_network_pipeline()`
  #' @param source_col source_col a string naming a column of origin nodes. Defaults to `"citing_doc`".  
  #' @param target_col target_col a string naming a column of corresponding target nodes. Defaults to `"cited_doc"`. 
  #' @param time_col time_col a string naming a column giving each edge's time step. Defaults to `"cite_year"`. 
  #' 
  #' @returns A list containing: 
  #' 
  #' - `network_obj`: a PAFit network object. 
  #' - `node_ids`: a two column dataframe linking original and new node ids.
  
  # first, we need to create the network
  network_obj <- edge_list_df %>%
    select(all_of(c(source_col, target_col, time_col)))
  
  # PAFit requires node ids be integers > -1, so here I'm assigning unique integer ids to the old ids
  node_ids <- tibble(current_id = sort(unique(c(network_obj[[target_col]], network_obj[[source_col]])))) %>%
    filter(current_id != "-1")
  node_ids$id <- seq_along(node_ids$current_id)
  # in some cases, we have to deal with adding new nodes without edges, which you do by making the target node `-1`
  if (any(network_obj[[target_col]] == "-1")) {
    node_ids <- node_ids %>% add_row(current_id = "-1", 
                                     id = -1)
  }
  
  network_obj[[source_col]] <- node_ids[match(network_obj[[source_col]], node_ids$current_id),]$id
  network_obj[[target_col]] <- node_ids[match(network_obj[[target_col]], node_ids$current_id),]$id
  
  # make it an actual network object
  network_obj <- network_obj %>%
    mutate(across(all_of(c(source_col, target_col, time_col)), as.numeric)) %>%
    as.matrix() %>%
    as.PAFit_net()
  
  return(list("network_obj" = network_obj,
              "node_ids" = node_ids))
  
}



PAFit_model_pipeline <- function(cites_df, source_col = "citing_doc", target_col = "cited_doc", time_col = "cite_year", 
                                 skip_model = FALSE) {
  #' DF of cites to a network, stats, fit model, etc.
  #' 
  #' @param cites_df a dataframe of citations, one per row, from which to create the model. 
  #' @param source_col a string naming a column of origin nodes. Defaults to `"citing_doc`". 
  #' @param target_col a string naming a column of corresponding target nodes. Defaults to `"cited_doc"`.
  #' @param time_col a string naming a column giving each edge's time step. Defaults to `"cite_year"`.
  #' @param skip_model optionally skip the step of actually fitting the model, in cases where it isn't
  #' necessary, thus saving time. Default is `FALSE`. 
  #' 
  #' @return A list consisting of the following: 
  #' - `network_obj`: the PAFit network object created from `cites_df`.
  #' - `node_ids`: a two column dataframe of the correspondence between the IDs of the input (from `source_col` and
  #'   `target_col`) and sequential integer IDs guaranteed to work with PAFit.
  #' - `stats_obj`: the PAFit stats object returned by `get_statistics()`. 
  #' - `model_obj`: the fit model object produced by `joint_estimate()`. Omitted if `skip_model == TRUE`.
  #' 
  
  # first, we need to create the network
  network_obj <- cites_df %>%
    select(all_of(c(source_col, target_col, time_col)))
  
  pre_network_df <- network_obj
  
  network_list <- edge_list_to_PAFit_net(network_obj, source_col, target_col, time_col)
  
  # so now we get the stats
  stats_obj <- get_statistics(network_list[["network_obj"]])
  
  if (!skip_model) {
    # and fit the model
    model_obj <- joint_estimate(network_list[["network_obj"]], stats_obj)
    
  } else {
    # don't fit the model
    model_obj <- NULL
  }
  
  # finally, stick everything in the output list
  out <- list("network_obj" = network_list[["network_obj"]], 
              "pre_network_df" = pre_network_df,
              "node_ids" = network_list[["node_ids"]], 
              "stats_obj" = stats_obj, 
              "model_obj" = model_obj)
  
  return(out)
}

add_PAFit_metadata <- function(cites_year_df, model_pipeline_output, 
                               node_id_col = "source_id", k_col = "cumsum_cites") {
  #' Add the estimated PA and NF values to a dataframe
  #' 
  #' @param cites_year_df a dataframe of cites per item/author per year, to which 
  #' we will add node fitness and preferential attachment function values. 
  #' @param model_pipeline_output a list output by `PAFit_model_pipeline()`, from which 
  #' we get the node fitness and preferential attachment values (and the node ID correspondence).
  #' @param node_id_col a string naming the column in `cites_year_df` which should be used to match node IDs. Default `"source_id"`.
  #' @param k_col a string naming the column in `cites_year_df` which give the value of k for getting the PA value. Default `"cumsum_cites"`.
  #' 
  #' @return A dataframe identical to `cites_year_df`, but with node fitness and preferential attachment
  #' function values for each row. 
  #' 
  
  node_ids <- model_pipeline_output[["node_ids"]]
  kA <- tibble(k = 1:sum(model_pipeline_output$stats_obj$interval_length) - 1, 
               A = rep(model_pipeline_output$model_obj$estimate_result$theta, 
                       times = model_pipeline_output$stats_obj$interval_length))
  
  kA_orig <- kA
  
  kA <- interpolate_kA(kA)
  
  cites_year_df[["node_id"]] <- node_ids[match(cites_year_df[[node_id_col]], node_ids$current_id),]$id
  cites_year_df[["node_fitness"]] <- model_pipeline_output$model_obj$estimate_result$f[cites_year_df[["node_id"]]]
  
  colnames(cites_year_df)[colnames(cites_year_df) == node_id_col] <- "id_col"
  
  # make A the coefficient which *generated* this time step's new edges
  # previously was the coefficient based on number of cites up to and including the current time step
  # pretty sure this way makes more sense, though it does require setting A = 1 at the first time step
  cites_year_df <- cites_year_df %>%
    group_by(id_col) %>%
    nest() %>%
    mutate(data = map(data, function(x) {
      x[["count_year"]] <- x[["cite_year"]] - min(x[["cite_year"]])
      x[["current_A"]] <- kA[match(c(0, x[[k_col]][-length(x[[k_col]])]), kA[["k"]]), ][["A"]]
      x[["cumsum_A"]] <- cumsum(x[["current_A"]])
      x <- x %>% select(-count_year)
    })) %>% 
    unnest(cols = c(data)) %>%
    ungroup()
  
  colnames(cites_year_df)[colnames(cites_year_df) == "id_col"] <- node_id_col
  
  # cites_year_df[["current_A"]] <- kA[match(cites_year_df[[k_col]], kA$k),]$A
  # cites_year_df[["current_A_orig"]] <- kA_orig[match(cites_year_df[[k_col]], kA_orig$k),]$A
  
  return(cites_year_df)
  
}

interpolate_kA <- function(kA_df) {
  #' Interpolate values for A for any A == 0
  #' 
  #' Interpolates linearly on the log scale. 
  #' 
  
  kA_nonzero <- kA_df %>% 
    filter(A != 0)
  
  
  for (i in 1:(length(kA_nonzero$k) - 1)) {
    if (kA_nonzero$k[i + 1] > kA_nonzero$k[i] + 1) {
      regress <- lm(c(log(kA_nonzero$A[i]), log(kA_nonzero$A[i + 1])) ~
                      c(log(kA_nonzero$k[i]), log(kA_nonzero$k[i + 1])))
      
      for (j in (kA_nonzero$k[i] + 1) : (kA_nonzero$k[i + 1] - 1)) {
        kA_df[kA_df$k == j, ]$A <- exp(log(j) * regress$coefficients[2] + regress$coefficients[1])
      }
    }
  }
  
  # the above doesn't catch cases where the last value(s) are 0, so we just fill from the previous value, cause I'm feeling lazy
  kA_df <- kA_df %>%
    mutate(A = if_else(A == 0, NA, A)) %>%
    fill(A)
  
  return(kA_df)
  
}



est_contrib_uncertainty <- function(simulated_object, original_result) {
  #' Estimate Mean Contributions Across Simulated Networks
  #' 
  #' This is, almost exactly, the internals of `plot_contribution()`, just without 
  #' the part that does the plotting. The only difference is that this returns a 
  #' long dataframe with the estimates for both PA and NF contributions, since I'm
  #' basically always going to want both. 
  
  x <- y <- ymin <- ymax <- y_obs <- NULL
  if (class(simulated_object) != "Simulated_Data_From_Fitted_Model") {
    stop("simulated_object needs to be an object from generate_simulated_data_from_estimated_model")
  }
  if (class(original_result) != "Full_PAFit_result") {
    stop("original_result needs to be an object from joint_estimate")
  }
  result_list <- simulated_object$result_list
  M <- length(result_list)
  T <- length(result_list[[1]]$contribution$PA_contribution) + 1
  PA_contrib_replicate  <- matrix(0,nrow = T - 1, ncol = M)
  fit_contrib_replicate <- matrix(0,nrow = T - 1, ncol = M)
  
  for (mm in 1:M) {
    PA_contrib_replicate[,mm]  <- result_list[[mm]]$contribution$PA_contribution
    fit_contrib_replicate[,mm] <- result_list[[mm]]$contribution$fit_contribution
  }
  
  simulated_mean_PA_contrib  <- rowMeans(PA_contrib_replicate)
  simulated_sd_PA_contrib    <- apply(PA_contrib_replicate,MARGIN = 1, sd)
  simulated_mean_fit_contrib <- rowMeans(fit_contrib_replicate)
  simulated_sd_fit_contrib   <- apply(fit_contrib_replicate,MARGIN = 1, sd)
  sd_log_PA                  <- simulated_sd_PA_contrib/simulated_mean_PA_contrib
  sd_log_fit                 <- simulated_sd_fit_contrib/simulated_mean_fit_contrib
  sd_log_PA[is.nan(sd_log_PA)] <- 0
  sd_log_fit[is.nan(sd_log_fit)] <- 0
  
  y_dat_PA      <- simulated_mean_PA_contrib
  y_upper_PA    <- simulated_mean_PA_contrib + 2 * simulated_sd_PA_contrib 
  y_lower_PA    <- simulated_mean_PA_contrib - 2 * simulated_sd_PA_contrib 
  y_observed_PA <- original_result$contribution$PA_contribution 
  y_dat_fit      <- simulated_mean_fit_contrib
  y_upper_fit    <- simulated_mean_fit_contrib + 2 * simulated_sd_fit_contrib 
  y_lower_fit    <- simulated_mean_fit_contrib - 2 * simulated_sd_fit_contrib 
  y_observed_fit <- original_result$contribution$fit_contribution  
  
  # y_lower <- pmax(y_lower,0) # omitting this, since I think they only keep it around for pretty plotting, though I'm not certain
  
  dat_PA <- data.frame(x = 1:(T-1),y = y_dat_PA, ymin = y_lower_PA, ymax = y_upper_PA,
                       y_obs = y_observed_PA)
  dat_fit <- data.frame(x = 1:(T-1),y = y_dat_fit, ymin = y_lower_fit, ymax = y_upper_fit,
                        y_obs = y_observed_fit)
  
  dat <- bind_rows(list("PA" = dat_PA, "fit" = dat_fit), .id = "mechanism")
  
  return(dat)
}


# ---- Generating Simulated Data ----
generate_simulated_data_from_estimated_model_parallel <- function(net_object, net_stat, result, M = 5) {
  
  oopts <- options(scipen = 999)
  on.exit(options(oopts))
  
  if (!is(net_object,"PAFit_net"))
    stop("Error: net_object should be a PAFit_net object.")
  
  if (!is(net_stat,"PAFit_data"))
    stop("Error: net_stat should be a PAFit_data object.")
  
  if (!is(result,"Full_PAFit_result"))
    stop("Error: result should be a Full_PAFit_result object.")
  
  M <- as.integer(M) 
  
  if (M < 3)
    stop("Error: M is too small.")  
  
  
  
  net               <- net_object$graph
  net_type          <- net_object$type
  net               <- net[order(net[,3], decreasing = FALSE),]
  time_stamp        <- as.vector(net[,3])
  in_node           <- as.vector(net[,2])
  out_node          <- as.vector(net[,1])
  out_node          <- out_node
  node_id           <- as.numeric(sort(union(in_node[in_node !=  -1],out_node[out_node != - 1])))
  
  ok_id <- which(in_node != -1 & out_node != -1)
  if (net_type[1] == "directed") {
    deg           <- table(in_node[ok_id])
  } else
    if (net_type[1] == "undirected")
      deg           <- table(c(in_node[ok_id],out_node[ok_id]))     
  
  start_deg         <- 0
  deg_new           <- rep(0,length(node_id))
  names(deg_new)    <- as.numeric(node_id)
  deg_new[as.character(as.numeric(labels(deg)[[1]]))] <- deg
  deg               <- deg_new
  final_deg         <- deg
  deg.max           <- as.numeric(max(deg))
  unique_time       <- sort(unique(time_stamp))
  T                 <- length(unique_time)
  N                 <- length(node_id)
  
  
  existing_node          <- vector(mode = "list",length = T)
  new_node_list          <- vector(mode = "list",length = T) # the new nodes appear at that time-step
  const_graph_list       <- vector(mode = "list",length = T) #list of non-simulated edges or new nodes at each time-step
  edge_from_new_node     <- vector(mode = "list",length = T)  # list of the fixed source node of new edges needed to draw
  num_of_new_edges_fixed <- rep(0,length = T) # total number of new edges needed to draw with fixed source node : only in directed case
  num_of_new_edges_free  <- rep(0,length = T) # total number of new edges needed to draw free both end
  
  
  for (i in 1:T) {
    current_graph <- net[net[,3] == unique_time[i],,drop = FALSE]
    in_node_temp  <- current_graph[, 2, drop = FALSE]
    out_node_temp <- current_graph[, 1, drop = FALSE] 
    
    if (i == 1) {
      const_graph_list[[i]] <- current_graph
    } else {
      if (net_type[1] == "undirected") {
        const_index             <- !(in_node_temp %in% existing_node[[i - 1]]) | !(out_node_temp %in% existing_node[[i - 1]])
        const_graph_list[[i]]   <- current_graph[const_index,,drop = FALSE]
        edge_from_new_node[[i]] <- NULL
        num_of_new_edges_fixed[i] <- 0
        num_of_new_edges_free[i]         <- sum(net[,3] == unique_time[i]) - sum(const_index)
      } else if (net_type[1] == "directed") {
        # destination node are new
        const_index             <- !(in_node_temp %in% existing_node[[i - 1]])
        const_graph_list[[i]]   <- current_graph[const_index,,drop = FALSE]
        # source node index: source node is new but destination node is existing
        
        source_new_index          <- (in_node_temp %in% existing_node[[i - 1]]) & !(out_node_temp %in% existing_node[[i - 1]]) 
        source_node_is_new        <- out_node_temp[source_new_index]
        edge_from_new_node[[i]]   <- source_node_is_new
        num_of_new_edges_fixed[i] <- length(source_node_is_new)
        
        # the remaining new edges that have both source and destination nodes as existing nodes
        num_of_new_edges_free[i]      <- sum(net[,3] == unique_time[i]) - sum(const_index)  - sum(source_new_index)
        
        #check:
        if (num_of_new_edges_free[i] != sum((in_node_temp %in% existing_node[[i - 1]]) & (out_node_temp %in% existing_node[[i - 1]]))) {
          print("Mismatch in num of edge free") 
          print(i)
          print(num_of_new_edges_free[i])
          print(sum((in_node_temp %in% existing_node[[i - 1]]) & (out_node_temp %in% existing_node[[i - 1]])))
        }
        
      }
    }
    existing_node[[i]] <- as.numeric(sort(union(in_node_temp[in_node_temp !=  -1],
                                                out_node_temp[out_node_temp != - 1])))
    if (i > 1) {
      new_node_list[[i]] <- setdiff(existing_node[[i]],existing_node[[i - 1]])
      existing_node[[i]] <- union(existing_node[[i]],existing_node[[i - 1]])
      
    }
  }
  
  
  
  deg_second_max <- max(result$estimate_result$k)
  
  PA <- (0:deg_second_max)^result$estimate_result$alpha
  names(PA) <- 0:deg_second_max
  PA["0"]   <- 1
  PA[as.character(result$estimate_result$k)] <- result$estimate_result$A
  
  f  <- result$estimate_result$f
  
  
  is_only_PA <- result$estimate_result$only_PA
  is_only_f  <- result$estimate_result$only_f
  
  if (net_type[1] == "directed") {is_directed <- TRUE} else {is_directed <- FALSE};
  
  binning_used    <- net_stat$binning
  g_used          <- result$estimate_result$g
  deg_thresh_used <- result$estimate_result$deg_threshold
  p_used          <- result$cv_data$p
  stop_cond_used  <- result$estimate_result$stop_cond
  
  graph_list  <- vector(mode = "list", length = M)
  stats_list  <- vector(mode = "list", length = M)
  result_list <- vector(mode = "list",length = M)
  
  future_seed <- 1234
  plan(multisession, workers = 8)
  
  sims_list <- furrr::future_map(1:M, function(x) {
    
    out <- generate_simulated_data_from_estimated_model_inner(const_graph_list, existing_node, deg_second_max, edge_from_new_node, num_of_new_edges_free,  num_of_new_edges_fixed,  
                                                              new_node_list, binning_used, g_used, deg_thresh_used, p_used, stop_cond_used, net_type, is_directed, 
                                                              PA, f, unique_time, T, is_only_PA, is_only_f)
    
  }, .options = furrr_options(seed = future_seed)) %>%
    list_rbind()
  
  graph_list <- sims_list[["graph_obj"]]
  stats_list <- sims_list[["stats_obj"]]
  result_list <- sims_list[["result_obj"]]
  
  supplement_data <- list(existing_node      = existing_node,
                          new_node_list      = new_node_list,
                          const_graph_list   = const_graph_list, 
                          edge_from_new_node = edge_from_new_node,
                          num_of_new_edges_fixed   = num_of_new_edges_fixed,
                          num_of_new_edges_free    = num_of_new_edges_free)
  
  
  final_result <- list(result_list = result_list, graph_list = graph_list,
                       stats_list = stats_list, supplement_data = supplement_data)
  
  class(final_result) <- "Simulated_Data_From_Fitted_Model"
  
  return(final_result)
  
}


generate_simulated_data_from_estimated_model_inner <- function(const_graph_list, existing_node, deg_second_max, edge_from_new_node, num_of_new_edges_free,  num_of_new_edges_fixed,  
                                                               new_node_list, binning_used, g_used, deg_thresh_used, p_used, stop_cond_used, net_type, is_directed, 
                                                               PA, f, unique_time, T, is_only_PA, is_only_f) {
  
  graph         <- const_graph_list[[1]]
  in_node_temp  <- graph[, 2, drop = FALSE]
  out_node_temp <- graph[, 1, drop = FALSE] 
  ok_id         <- which(in_node_temp != -1 & out_node_temp != -1)
  deg_vec       <- rep(-1,length(existing_node[[T]]))
  names(deg_vec) <- as.character(existing_node[[T]])
  
  deg_vec[as.character(existing_node[[1]])] <- 0
  
  if (length(ok_id) > 0) {
    if (TRUE == is_directed) {
      temp_vec       <- table(in_node_temp[ok_id])
    } else {temp_vec <- table(c(in_node_temp[ok_id],out_node_temp[ok_id]))}
    deg_vec[names(temp_vec)] <- deg_vec[names(temp_vec)] + temp_vec
  }
  
  for (i in 2:T) {
    
    if (dim(const_graph_list[[i]])[1] > 0) graph <- rbind(graph,const_graph_list[[i]])
    pa_value <- PA[as.character(deg_vec[as.character(existing_node[[i - 1]])])]
    if (sum(is.na(pa_value)) > 0) {
      if (max(deg_vec) <= deg_second_max ) {
        print("Wrong about deg_second_max ")  
      } else pa_value[is.na(pa_value)] <- PA[length(PA)] # only happen when deg_max > observed deg max
    }
    
    fit_value <- f[as.character(existing_node[[i - 1]])]
    
    if (num_of_new_edges_free[i] > 0) {# sampling new edges whose both ends are free
      in_node_new  <- sample(size = num_of_new_edges_free[i], replace = TRUE, x = existing_node[[i - 1]], 
                             prob = pa_value * fit_value /sum(pa_value * fit_value))  
      if (TRUE == is_directed) {
        out_node_new <- sample(size = num_of_new_edges_free[i],replace = TRUE, x = existing_node[[i - 1]]) # sample out nodes uniformly
      } else {
        out_node_new  <- sample(size = num_of_new_edges_free[i], replace = TRUE, x = existing_node[[i - 1]], 
                                prob = pa_value * fit_value /sum(pa_value * fit_value) ) # based on model
      }
      graph        <- rbind(graph,cbind(out_node_new,in_node_new,unique_time[i]))
    }
    
    if (num_of_new_edges_fixed[i] > 0) { # only possible in directed case: sampling the destination nodes
      if (FALSE == is_directed) {print("Wrong in replicating new edge fixed")}
      in_node_new  <- sample(size = num_of_new_edges_fixed[i], replace = TRUE, x = existing_node[[i - 1]], 
                             prob = pa_value * fit_value/sum(pa_value * fit_value))  
      
      graph <- rbind(graph,cbind(edge_from_new_node[[i]],in_node_new,unique_time[i]))
    }
    
    # update degree vector 
    in_node_temp  <- graph[graph[,3] == unique_time[i], 2, drop = FALSE]
    out_node_temp <- graph[graph[,3] == unique_time[i], 1, drop = FALSE] 
    ok_id         <- which(in_node_temp != -1 & out_node_temp != -1)
    if (length(ok_id) > 0) {
      if (TRUE == is_directed) {
        temp_vec       <- table(in_node_temp[ok_id])
      } else {temp_vec <- table(c(in_node_temp[ok_id],out_node_temp[ok_id]))}
      
      if (length(new_node_list[[i]]) > 0) {
        # check these nodes are really new
        if (sum(deg_vec[as.character(new_node_list[[i]])] != -1) > 0) {
          print("Wrong in updating degree vec")   
        }
        deg_vec[as.character(new_node_list[[i]])] <- 0
      }
      deg_vec[names(temp_vec)] <- deg_vec[names(temp_vec)] + temp_vec
    }
  }
  
  graph
  one_net <- as.PAFit_net(graph, type = net_type)
  stats_obj  <- get_statistics(one_net, binning = binning_used, g = g_used) 
  
  if (TRUE == is_only_PA) {
    result_obj <- only_A_estimate(net_object = one_net, net_stat = stats_obj, p = p_used, 
                                  stop_cond = stop_cond_used)
  } else if (TRUE == is_only_f) {
    result_obj <- only_F_estimate(net_object = one_net, net_stat = stats_obj, p = p_used, 
                                  stop_cond = stop_cond_used)
  } else {
    result_obj <- joint_estimate(net_object = one_net, net_stat = stats_obj, p = p_used, 
                                 stop_cond = stop_cond_used)
  }
  
  return(tibble("graph_obj" = list(graph), "stats_obj" = list(stats_obj), "result_obj" = list(result_obj)))
  
}


# ---- Bootstrap Modeling ----

create_treatment_df <- function(cite_df, id_col, treatment_col, pipeline_output, pre_post = TRUE) {
  #' Create df of treated sources and year of treatment
  #' 
  #' @param cite_df a dataframe of citations or sources, containing at least `id_col` 
  #' and `treatment_col`, with at least the records of the treated sources. 
  #' @param id_col the name of the id column in `cite_df`. 
  #' @param treatment_col the name of the treatment year column in `cite_df`. 
  #' @param pipeline_output a list output by `PAFit_model_pipeline`. The 
  #' included `model_obj` can be `NULL`.
  #' @param pre_post sets whether we restrict treated nodes to those that have at least one
  #' year both pre- and post-treatment. Defaults to `TRUE`. The option is largely included
  #' for "backwards awareness": originally, there was no option and the function estimated 
  #' as though `pre_post = FALSE`, which I'm pretty sure was the wrong way to go.  
  #' 
  #' @return A dataframe of five columns, containing the original correspondence of 
  #' `treated_id` and `treatment_year`, together with the `PAFit_net` version of the
  #' `treated_id` as `pre_id`, ids for the post-treatment nodes (`post_id`, sequentially
  #' ascending from the final original node id), and the `treated_timestep`. 
  #' 
  
  treatment_df <- tibble(treated_id = cite_df[[id_col]], 
                         treated_year = cite_df[[treatment_col]], 
                         cite_year = cite_df[["cite_year"]])
  
  if (pre_post) { # only keep those sources with at least one year each side of the treatment year (with the year itself counting as "pre")
    treatment_df <- treatment_df %>%
      group_by(treated_id) %>%
      nest() %>%
      mutate(to_keep = map_lgl(data, function(x) {
        return(any(x[["cite_year"]] <= x[["treated_year"]]) & any(x[["cite_year"]] > x[["treated_year"]]))
      })) %>%
      unnest(cols = c(data)) %>%
      filter(to_keep) %>%
      ungroup() %>%
      select(-cite_year)
  } else { # otherwise, we drop cite_year, since we don't need it for anything
    treatment_df <- treatment_df %>%
      select(-cite_year)
  }
  
  treatment_df <- treatment_df %>% 
    distinct(treated_id, .keep_all = TRUE) %>%
    filter(!is.na(treated_year))
  
  # for convenience
  stats_obj <- pipeline_output$stats_obj
  node_ids <- pipeline_output$node_ids
  
  # set correspondence between external ids in treatment_df and network node ids, which will now be the 
  # ids for the pre-treatment nodes. Then generate the post-treatment ids and convert the treated_year into
  # a treated timestep
  treatment_df[["pre_id"]] <- node_ids[match(treatment_df[["treated_id"]], node_ids[["current_id"]]), ][["id"]]
  treatment_df[["post_id"]] <- (stats_obj$N:(stats_obj$N + nrow(treatment_df)))[-1] # drop the first, cause x:y syntax includes the first value in the sequence
  treatment_df[["treated_timestep"]] <- treatment_df[["treated_year"]] - min(as.numeric(pipeline_output$pre_network_df$cite_year))
  # these time steps line up how I want, I'm pretty sure
  
  return(treatment_df)
  
  
}

treat_PAFit_model <- function(pipeline_output, treatment_df) {
  #' A version
  #' 
  #' @param pipeline_output a list output by `PAFit_model_pipeline`. The 
  #' included `model_obj` can be `NULL`.
  #' @param treatment_df a five column dataframe with the set of treated source, 
  #' the time of treatment, and pre/post-treatment ids and time step of treatment
  #' in the `PAFit` network.
  #' 
  #' @returns A version of the `pipeline_output$stats_obj` which has been 
  #' altered to split treated nodes into separate pre- and post-treatment 
  #' nodes, while maintaining a continuity of edges &c. It also includes 
  #' `treatment_df` as an additional item in the list. 
  #' 
  
  # for convenience
  stats_obj <- pipeline_output$stats_obj
  node_ids <- pipeline_output$node_ids
  
  # create the pre/post-treatment careers to use to update stats_obj$node_degree
  treated_careers <- pmap(list("pre_id" = treatment_df[["pre_id"]], 
                               "post_id" = treatment_df[["post_id"]], 
                               "treated_timestep" = treatment_df[["treated_timestep"]]), 
                          function(pre_id, post_id, treated_timestep) {
                            
                            source_career <- stats_obj[["node_degree"]][, pre_id]
                            
                            pre_career <- c(source_career[1:treated_timestep], rep(-1, times = length(source_career) - treated_timestep))
                            post_career <- c(rep(-1, times = treated_timestep - 1), source_career[treated_timestep:length(source_career)])
                            
                            out <- list(pre_career, post_career)
                            names(out) <- c(pre_id, post_id)
                            
                            return(as_tibble(out))
                            
                          }) %>% 
    list_cbind()
  
  # update node degree with new careers
  treated_node_degree <- stats_obj$node_degree
  treated_node_degree[, treatment_df[["pre_id"]]] <- # update the originals with the pre-treatment careers
    as.matrix(treated_careers[, as.character(treatment_df[["pre_id"]])])
  treated_node_degree <- cbind(treated_node_degree, # and add the new post-treatment careers
                               as.matrix(treated_careers[, as.character(treatment_df[["post_id"]])]))
  
  treated_node_id <- c(stats_obj$node_id, treatment_df[["post_id"]])
  
  treated_N <- length(treated_node_id)
  treated_appear <- c(stats_obj$appear_time, treatment_df[["treated_timestep"]])
  names(treated_appear) <- treated_node_id
  
  # update z_j
  treated_z_j <- c(stats_obj$z_j, stats_obj$z_j[treatment_df[["pre_id"]]])
  names(treated_z_j) <- treated_node_id
  treated_z_j[treatment_df[["pre_id"]]] <- treated_node_degree[cbind(treatment_df[["treated_timestep"]] + 1, treatment_df[["post_id"]])] # I think this works
  treated_z_j[treated_z_j == -1] <- 0 # except they can't be negative 1
  
  stats_obj[["node_degree"]] <- treated_node_degree
  stats_obj[["z_j"]] <- treated_z_j
  stats_obj[["node_id"]] <- treated_node_id
  stats_obj[["f_position"]] <- treated_node_id # currently assuming it's always gonna be all nodes
  stats_obj[["N"]] <- treated_N
  stats_obj[["appear_time"]] <- treated_appear
  stats_obj[["treatment_df"]] <- treatment_df
  
  return(stats_obj)  
  
}




treat_PAFit_network <- function(PAFit_net, treatment_df) {
  #' Create a treated version of a PAFit_net object
  #' 
  #' @param PAFit_net an object of class `PAFit_net` to be treated.
  #' @param treatment_df the output of `create_treatment_df()`. 
  #' 
  #' @return A new `PAFit_net` object with node ids changed, nodes added, and edges added
  #' such that each "treated" node is now a "pre" node and a "post" node, overlapping at the 
  #' treatment time-step.
  #' 
  #' This is not, at the moment, used for treated model fitting, but rather for `generate_simulated_data_from_estimated_model()`.
  
  network_graph <- PAFit_net$graph
  
  citing <- colnames(network_graph)[1]
  cited <- colnames(network_graph)[2]
  
  treated_sub_graphs <- pmap(list(treatment_df$pre_id, treatment_df$post_id, treatment_df$treated_year), function(pre_id, post_id, treated_year) {
    
    sub_graph <- network_graph[network_graph[, cited] == pre_id, ] # subset to the treated node
    
    if (!("matrix" %in% class(sub_graph))) { # again with this absolutely insane default matrix behavior of just having a single row become a vector
      sub_graph <- matrix(c(sub_graph, sub_graph[1], post_id, treated_year), ncol = 3, byrow = TRUE, 
                          dimnames = list(NULL, c(citing, cited, "cite_year")))
      # print(sub_graph)
      return(sub_graph)
    }
    
    sub_graph[sub_graph[, cited] > treated_year, cited] <- post_id # replace pre_id with post_id after the treated year
    treat_year_new <- sub_graph[sub_graph[, cited] == pre_id, ] # create new subset of pre-treatment edges
    
    if ("matrix" %in% class(treat_year_new)) { # so that we can have the post_id start with all the pre_id edges
      treat_year_new[, c(cited, "cite_year")] <- rep(c(post_id, treated_year), each = nrow(treat_year_new))
      sub_graph <- rbind(sub_graph, treat_year_new) # by binding them together
    } else {
      sub_graph <- rbind(sub_graph, c(treat_year_new[1], post_id, treated_year)) # for some reason, if we only get one row, then it becomes a vector instead of a matrix
      # print(sub_graph)
    }
    
    return(sub_graph)
  })
  
  new_graph <- rbind(network_graph[!(network_graph[, cited] %in% treatment_df$pre_id), ], do.call(rbind, treated_sub_graphs))
  
  return(as.PAFit_net(new_graph))
}



fit_treated_model <- function(treated_stats, pretreatment_model = NULL, pretreatment_network = NULL) {
  #' Fit a PAFit model with treated data
  #' 
  #' Fit a PAFit model to a treated `get_statistics()` object. To do this, it calls `PAFit()` directly. 
  #' One of `pretreatment_model` or `pretreatment_network` must be provided, because the regularization 
  #' parameters and estimated node fitnesses are used to initialize the estimation 
  #' procedure (as though they had been output from the cross-validation/regularization step, which we
  #' are skipping here). 
  #' 
  #' @param treated_stats the output of `treat_PAFit_model()`. 
  #' @param pretreatment_model a PAFit model object fit to the original untreated network. If `NULL`, 
  #' then `pretreatment_network` must be provided. 
  #' @param pretreatment_network a PAFit network object which can be used to fit a pre-treatment network.
  #' If `NULL`, then `pretreatment_model` must be provided. 
  #' 
  #' @return A `Full_PAFit_result` object, with `cv_data` and `cv_result` set to their values from the
  #' pretreatement model, since that is what was used. 
  #' 
  
  if (!is.null(pretreatment_model)) {
    cv_result <- pretreatment_model$cv_result
    cv_data <- pretreatment_model$cv_data
  } else if (!is.null(pretreatment_network)) {
    pretreatment_model <- joint_estimate(pretreatment_network)
    cv_result <- pretreatment_model$cv_result
    cv_data <- pretreatment_model$cv_data
  } else {
    stop("Error: Either a pretreatment model or network must be provided to initialize the estimation procedure.")
  }
  
  treatment_df <- treated_stats$treatment_df
  
  # we take from the actual cross validation results where we can, to most fully match the normal procedure
  r_optimal <- cv_result$r_optimal
  s_optimal <- cv_result$s_optimal
  
  # for the intializing estimated fitnesses, we want to use the estimates from the cross validation when
  # we have them, but also need to add the post-treatment nodes (with any estimate from the pre-treatment)
  # and set to 1 for all nodes with no cv estimate
  f_vector <- rep(1, length(treated_stats$f_position))
  names(f_vector) <- as.numeric(treated_stats$f_position)
  
  name_vec <- as.character(as.numeric(treated_stats$f_position))
  name_cv  <- names(cv_result$estimated_fitness)
  for (ii in 1:length(name_vec)) {
    
    jj <- which(name_cv == name_vec[ii])  
    
    if (length(jj) > 0) {
      f_vector[name_vec[ii]] <- cv_result$estimated_fitness[jj]
      if (name_cv[jj] %in% treatment_df[["pre_id"]]) {
        f_vector[name_vec[treatment_df[treatment_df[["pre_id"]] == ii, ][["post_id"]]]] <- 
          cv_result$estimated_fitness[jj]
      }
    }
  }
  
  alpha_optimal <- pretreatment_model$cv_result$alpha_optimal
  stop_cond <- pretreatment_model$estimate_result$stop_cond
  
  
  result <- PAFit:::PAFit(treated_stats,
                          r            = r_optimal,
                          s            = s_optimal,
                          start_f      = f_vector           ,
                          alpha_start  = alpha_optimal      ,
                          stop_cond    = stop_cond          ,
                          weight_power = 0                  ,
                          mode_reg_A   = 0)
  
  combined_result <- list(cv_data = cv_data, cv_result = cv_result, 
                          estimate_result = result) 
  #calculate contribution
  
  PA  <- result$A
  fit <- result$f
  small_t <- dim(treated_stats$node_degree)[1]
  contrib_PA_array  <- rep(0,small_t)
  contrib_fit_array <- rep(0,small_t) 
  name_node <- colnames(treated_stats$node_degree)
  
  for (i in 1:small_t) {
    presence <- treated_stats$node_degree[i,] > 0
    
    # sampling the node based on the product of PA and fitness
    pa_value                  <- PA[treated_stats$node_degree[i,presence] + 1]
    pa_value[is.na(pa_value)] <- PA[length(PA)]
    #print(length(pa_value))
    
    fitness_value <- fit[name_node[presence]]
    #print(length(fitness_value))
    sampling_prob <- fitness_value * pa_value / sum(fitness_value * pa_value)
    
    mean_log_PA   <- mean(sampling_prob * log(pa_value), na.rm = TRUE)
    var_log_PA    <- mean(sampling_prob* (log(pa_value) - mean_log_PA)^2, na.rm = TRUE)
    
    mean_log_fit   <- mean(sampling_prob * log(fitness_value) , na.rm = TRUE)
    var_log_fit    <- mean(sampling_prob* (log(fitness_value) - mean_log_fit)^2 , na.rm = TRUE)
    
    contrib_PA    <- var_log_PA
    contrib_fit   <- var_log_fit
    contrib_PA_array[i]  <- contrib_PA
    contrib_fit_array[i] <- contrib_fit
  }
  mean_PA_contrib  <- sqrt(mean(contrib_PA_array, na.rm = TRUE))
  mean_fit_contrib <- sqrt(mean(contrib_fit_array, na.rm = TRUE))
  contribution <- list(PA_contribution = sqrt(contrib_PA_array),
                       fit_contribution = sqrt(contrib_fit_array),
                       mean_PA_contrib = mean_PA_contrib,
                       mean_fit_contrib = mean_fit_contrib)
  
  combined_result <- list(cv_data = NULL, cv_result = NULL, 
                          estimate_result = result, contribution = contribution) 
  
  class(combined_result) <- "Full_PAFit_result"
  return(combined_result)
  
}


randomize_treated_nodes <- function(pipeline_output, treatment_df) {
  #' Randomize edges of treated nodes
  #' 
  #' @param pipeline_output pipeline_output a list output by `PAFit_model_pipeline`. The 
  #' included `model_obj` can be `NULL`.
  #' @param treatment_df 
  #' 
  #' @return A `PAFit_net` object with the edges of treated nodes randomized in network graph. Can 
  #' be fed directly to `get_statistics()`, followed by `treat_PAFit_model()`. 
  #' 
  
  network_obj <- pipeline_output$network_obj
  
  treated_edge_list <- map(treatment_df$pre_id, function(pre_id) { # do we care about year of node entry or year of first cite? for now it's first cite
    treated_edges <- network_obj[["graph"]][network_obj[["graph"]][, 2] == pre_id, ]
    
    if (!is.matrix(treated_edges)) { # ideally we maybe allow the year to change?
      # (note we're checking whether it's  matrix, not whether it has one row, because if you select a single row, suddenly it's no longer a matrix! the absolute fuckers)
      return(treated_edges)
    }
    
    bounds <- c(min(treated_edges[, 3]), max(network_obj[["graph"]][, 3])) # we allow them to be cited later than they ever were in reality, but not earlier, I think makes sense
    
    if (length(unique(bounds)) == 1) { # trying to sample from a length 1 int, is cause problems, I think
      return(treated_edges)
    }
    
    # randomize the years
    treated_edges[, 3] <- sample(seq(from = bounds[1], to = bounds[2]), size = nrow(treated_edges), 
                                 replace = TRUE)
    
    # And we randomize the citing document, being careful to avoid accidentally changing a cite year such that we make a citing doc appear earlier than it should...
    treated_edges[, 1] <- map_dbl(treated_edges[, 3], ~sample(network_obj[["graph"]][network_obj[["graph"]][, 3] == .x, 1], size = 1))
    
    return(treated_edges)
  })
  
  # drop the original, unrandomized, treated source edges from the network graph
  dropped_graph <- network_obj[["graph"]][!(network_obj[["graph"]][, 2] %in% treatment_df[["pre_id"]]), ]
  
  # and *now* we make sure we haven't lost any nodes (e.g., citing nodes which only cited a single treated node can easily disappear)
  missing_nodes <- network_obj[["graph"]][!(network_obj[["graph"]][, 1] %in% dropped_graph[, 1]), ]
  
  if (!is_empty(missing_nodes)) {
    # and we have to, again, check whether there's just a single row (EVEN THOUGH THE ERROR IT GIVES IS "incorrect number of subscripts on matrix"!)
    if (!is.matrix(missing_nodes)) {
      missing_nodes[2] <- -1 # just doing this for now, ideally I'd have them replace one instance of another citing source
    } else {
      missing_nodes[, 2] <- -1 # just doing this for now, ideally I'd have them replace one instance of another citing source
    }
    dropped_graph <- rbind(dropped_graph, missing_nodes)
  }
  
  # and add the new random ones
  network_obj[["graph"]] <- rbind(dropped_graph, do.call(rbind, treated_edge_list))
  # and order by year again
  network_obj[["graph"]] <- network_obj[["graph"]][order(network_obj[["graph"]][, 3]), ]
  
  return(network_obj)
  
}


library(furrr)

PAFit_bootstrap_outer <- function(pipeline_output, cite_df, id_col, treatment_col, n = 100) {
  #' Orchestrates a full run of the bootstrap model estimation
  #' 
  #' @param pipeline_output
  #' @param 
  #' 
  #' @return A dataframe summary of 
  #' 
  
  future_seed <- 1234
  
  treatment_df <- create_treatment_df(cite_df = cite_df, pipeline_output = pipeline_output, id_col = id_col, treatment_col = treatment_col)
  
  treated_model <- treat_PAFit_model(pipeline_output, treatment_df) %>%
    fit_treated_model(pretreatment_model = pipeline_output[["model_obj"]])
  
  # create the randomized versions of the network object, which we'll iterate over
  bootstrap_nets <- replicate(n = n, 
                              expr = randomize_treated_nodes(pipeline_output = pipeline_output, 
                                                             treatment_df = treatment_df), 
                              simplify = FALSE)
  
  results <- future_map(bootstrap_nets, function(boot_net) {
    boot_pipeline_output <- pipeline_output
    boot_pipeline_output[["network_obj"]] <- boot_net
    boot_pipeline_output[["stats_obj"]] <- get_statistics(boot_net)
    
    boot_treated_stats <- treat_PAFit_model(boot_pipeline_output, treatment_df = treatment_df)
    boot_fitted_model <- fit_treated_model(boot_treated_stats, pretreatment_model = pipeline_output[["model_obj"]])
    
    
    
    return(tibble(theta = list(boot_fitted_model[["estimate_result"]][["theta"]]),
                  f = list(boot_fitted_model[["estimate_result"]][["f"]]),
                  alpha = boot_fitted_model[["estimate_result"]][["alpha"]], 
                  r = boot_fitted_model[["estimate_result"]][["ratio"]], 
                  s = boot_fitted_model[["estimate_result"]][["shape"]], 
                  PA_contribution = list(boot_fitted_model[["contribution"]][["PA_contribution"]]), 
                  fit_contribution = list(boot_fitted_model[["contribution"]][["fit_contribution"]])))
    
  }, .options = furrr_options(seed = future_seed)) %>%
    list_rbind() %>%
    mutate(boot_rep = 1:n())
  
  out <- list("treated_model" = treated_model, 
              "boot_summary" = results, 
              "treatment_df" = treatment_df)
  
  return(out)
  
}


# ---- Bootstrap/Simulation Evaluation ----

bootstrap_eval <- function(bootstrap_result, treatment_df = NULL) {
  #' Transform a bootstrap result into a more easy to evaluate form
  #' 
  #'  
  
  if (is_empty(treatment_df)) {
    if (!("treatment_df" %in% names(bootstrap_result))) {
      stop("Error: `treatment_df` must be included as an element of `bootstrap_result` or passed explicitly as an argument.")
    } else {
      treatment_df <- bootstrap_result$treatment_df
    }
  }
  
  out <- list()
  
  # get a summary of the f-value bootstraps and treated values for the treated nodes/sources
  f <- do.call(rbind, bootstrap_result$boot_summary$f) %>% as_tibble(.names_repair = "minimal")
  out[["f"]] <- pmap(list("treated" = treatment_df[["treated_id"]], 
                          "pre" = treatment_df[["pre_id"]], 
                          "post" = treatment_df[["post_id"]]), 
                     function(treated, pre, post) {
                       
                       pre_stat <- list("treated" = bootstrap_result[["treated_model"]][["estimate_result"]][["f"]][pre], 
                                        "upr" = quantile(f[[pre]], .975),
                                        "lwr" = quantile(f[[pre]], .025),
                                        "mean" = mean(f[[pre]]))
                       post_stat <- list("treated" = bootstrap_result[["treated_model"]][["estimate_result"]][["f"]][post], 
                                         "upr" = quantile(f[[post]], .975),
                                         "lwr" = quantile(f[[post]], .025),
                                         "mean" = mean(f[[post]]))
                       
                       out <- tibble("treated_id" = treated, 
                                     "stat" = names(pre_stat), 
                                     "pre" = unlist(pre_stat), 
                                     "post" = unlist(post_stat))
                       
                       return(out)
                     }) %>% 
    list_rbind()
  
  # and a version where we drop any nodes which were NF = 1 both pre and post
  
  # don't think I actually really care about theta
  
  # alpha, r, and s
  out[["alpha"]] <- tibble("treated" = bootstrap_result$treated_model$estimate_result$alpha, 
                           "mean" = mean(bootstrap_result$boot_summary$alpha), 
                           "upr" = quantile(bootstrap_result$boot_summary$alpha, .975), 
                           "lwr" = quantile(bootstrap_result$boot_summary$alpha, .025))
  out[["r"]] <- bootstrap_result$treated_model$estimate_result$ratio # can't be different, across reps, since they're all from the same CV of the original model
  out[["s"]] <- bootstrap_result$treated_model$estimate_result$shape # see above
  
  # mean PA and NF contributions
  PA_reps <- map_dbl(bootstrap_result$boot_summary$PA_contribution, ~mean(.x, na.rm = TRUE)) # sometimes we wind up with NaN's in the contributions; not sure why
  fit_reps <- map_dbl(bootstrap_result$boot_summary$fit_contribution, ~mean(.x, na.rm = TRUE))
  
  out[["PA_contribution"]] <- tibble("treated" = bootstrap_result$treated_model$contribution$mean_PA_contrib, 
                                     "mean" = mean(PA_reps), 
                                     "upr" = quantile(PA_reps, .975), 
                                     "lwr" = quantile(PA_reps, .025))
  out[["fit_contribution"]] <- tibble("treated" = bootstrap_result$treated_model$contribution$mean_fit_contrib, 
                                      "mean" = mean(fit_reps), 
                                      "upr" = quantile(fit_reps, .975), 
                                      "lwr" = quantile(fit_reps, .025))
  
  return(out)
}


bootstrap_f_proc <- function(bootstrap_result, treatment_df = NULL) {
  #' Transform a bootstrap result into a more easy to evaluate form
  #' 
  #'  
  
  if (is_empty(treatment_df)) {
    if (!("treatment_df" %in% names(bootstrap_result))) {
      stop("Error: `treatment_df` must be included as an element of `bootstrap_result` or passed explicitly as an argument.")
    } else {
      treatment_df <- bootstrap_result$treatment_df
    }
  }
  
  out <- list()
  
  treated_model <- bootstrap_result$treated_model
  
  f <- do.call(rbind, bootstrap_result$boot_summary$f) %>% as_tibble(.names_repair = "minimal")
  out[["f"]] <- pmap(list("treated" = treatment_df[["treated_id"]], 
                          "pre" = treatment_df[["pre_id"]], 
                          "post" = treatment_df[["post_id"]]), 
                     function(treated, pre, post) {
                       f_pre <- f[[pre]]
                       f_post <- f[[post]]
                       
                       f_diff <- f_post - f_pre
                       
                       return(tibble("treated_id" = treated, 
                                     "treat_pre" = treated_model[["estimate_result"]][["f"]][pre],
                                     "treat_post" = treated_model[["estimate_result"]][["f"]][post],
                                     "pre" = list(f_pre), 
                                     "post" = list(f_post)))
                       
                     }) %>% 
    list_rbind()
  
  return(out)
  
}


NF_prepost_comp <- function(bootstrap_f_summary) {
  
  
  out <- bootstrap_f_summary %>%
    mutate(treat_diff = treat_post - treat_pre, 
           rand_diff = map2(pre, post, \(x, y) y - x)) %>%
    mutate(lwr = map_dbl(rand_diff, \(x) quantile(x, .025)), 
           upr = map_dbl(rand_diff, \(x) quantile(x, .975)), 
           mean = map_dbl(rand_diff, mean), 
           index = 1:n())
  
  return(out)
}


# NF_prepost_comp_orig <- function(bootstrap_f_summary) {
#   
#   out <- bootstrap_f_summary %>%
#     mutate(diff = post - pre) %>%
#     select(treated_id, stat, diff) %>%
#     pivot_wider(names_from = stat, values_from = diff) %>%
#     mutate(index = 1:n()) %>%
#     mutate(upr_new = if_else(upr > lwr, upr, lwr), 
#            lwr_new = if_else(upr > lwr, lwr, upr))
#   
#   return(out)
# }


sims_result_proc <- function(sims_obj) {
  #' Fetch a subset of the data in a simulation result object
  #' 
  #' 
  
  f <- do.call(rbind, map(sims_obj$result_list, \(x) x[["estimate_result"]][["f"]])) %>% as_tibble(.names_repair = "minimal")
  var_f <- do.call(rbind, map(sims_obj$result_list, \(x) x[["estimate_result"]][["var_f"]])) %>% as_tibble(.names_repair = "minimal")
  alpha <- do.call(rbind, map(sims_obj$result_list, \(x) x[["estimate_result"]][["alpha"]])) %>% as_tibble(.names_repair = "minimal")
  max_k <- do.call(rbind, map(sims_obj$result_list, \(x) max(x[["estimate_result"]][["k"]]))) %>% as_tibble(.name_repair = "minimal")
  max_A <- do.call(rbind, map(sims_obj$result_list, \(x) max(x[["estimate_result"]][["A"]]))) %>% as_tibble(.name_repair = "minimal")
  fit_contribution <- do.call(rbind, map(sims_obj$result_list, \(x) x[["contribution"]][["fit_contribution"]])) %>% as_tibble(.names_repair = "minimal")
  PA_contribution <- do.call(rbind, map(sims_obj$result_list, \(x) x[["contribution"]][["PA_contribution"]])) %>% as_tibble(.names_repair = "minimal")
  mean_fit_contrib <- do.call(rbind, map(sims_obj$result_list, \(x) x[["contribution"]][["mean_fit_contrib"]])) %>% as_tibble(.names_repair = "minimal")
  mean_PA_contrib <- do.call(rbind, map(sims_obj$result_list, \(x) x[["contribution"]][["mean_PA_contrib"]])) %>% as_tibble(.names_repair = "minimal")
  
  return(list("f" = f, 
              "var_f" = var_f, 
              "max_k" = max_k,
              "max_A" = max_A,
              "alpha" = alpha, 
              "fit_contribution" = fit_contribution, 
              "PA_contribution" = PA_contribution, 
              "mean_fit_contrib" = mean_fit_contrib, 
              "mean_PA_contrib" = mean_PA_contrib))
  
}



f_summarize <- function(f_df, sims_model, treatment_df = NULL, node_ids = NULL) {
  #' Get and summarize the estimated NFs from df of simulated NFs
  #' 
  #' @param f_df Output from `generate_simulated_data_from_estimated_models()`. 
  #' @param sim_model The `PAFit` model which is the source for the simulations. 
  #' @param treatment_df Output of `create_treatment_df()`. If provided, the output will have separate columns 
  #' for pre- and post-treatment nodes and will consider only those nodes. Can be `NULL`. 
  #' @param node_ids A vector of node IDs. Can be `NULL`. If provided, the output will contain only 
  #' those nodes. 
  #' 
  #' @return A dataframe organizing the distributions of the estimated f values to make summarization easy 
  #' (but still using the whole distribution). At most one of `treatment_df` or `node_ids` can be provided. 
  #' If neither is provided, the summaries for all node ids are returned. 
  
  if (!is_empty(treatment_df) & !is_empty(node_ids)) stop("At most one of treatment_df or node_ids can be provided.")
  
  f_summary <- map(1:ncol(f_df), function(node_id) {
    
    f_dist <- f_df %>% pull(node_id)
    
    out <- tibble("node_id" = node_id, 
                  "f" = list(f_dist))
    
    return(out)
    
  }) %>%
    list_rbind()
  
  if (!is_empty(treatment_df)) {
    f_pre <- f_summary[match(treatment_df$pre_id, f_summary$node_id), ]
    f_post <- f_summary[match(treatment_df$post_id, f_summary$node_id), ]
    
    f_summary <- tibble("treated_id" = treatment_df[["treated_id"]],
                        "treat_pre" = sims_model$estimate_result$f[treatment_df$pre_id], 
                        "treat_post" = sims_model$estimate_result$f[treatment_df$post_id], 
                        "pre" = f_pre[["f"]], 
                        "post" = f_post[["f"]])
    
  } else if (!is_empty(node_ids)) {
    f_summary <- f_summary[f_summary$node_id %in% node_ids, ]
    f_summary$original <- sims_model$estimate_result$f[node_ids]
  } else {
    f_summary$original <- sims_model$estimate_result$f[f_summary$node_id]
  }
  
  return(f_summary)
  
}


