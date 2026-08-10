# looking at names data

# setup ----

library(tidyverse)
library(stringdist)
library(igraph)
library(visNetwork)

load("data/clean_names_bystate.Rda")


# get vector of unique names ----

unique_names <- sort(unique(df$name))

# nearest neighbors ----

library(stringdist)
library(igraph)
library(visNetwork)

# Sample data - replace with your actual data
# first_names <- unique(df$first_name_column)
first_names <- sort(unique(df$name))
######


find_similar_firstnames <- function(firstname_of_interest, number_names = 5, threshold = 0.2) {
  
  # Check if name exists
  if (!firstname_of_interest %in% first_names) {
    stop(paste("Name '", firstname_of_interest, "' not found in the dataset!", sep = ""))
  }
  
  # Calculate distances from target name to all names
  distances <- stringdist(firstname_of_interest, first_names, method = "jw")
  names(distances) <- first_names
  
  # Remove the target name itself
  distances <- distances[names(distances) != firstname_of_interest]
  
  # Get the top number_names closest neighbors
  sorted_distances <- sort(distances)
  n_neighbors <- min(number_names, length(sorted_distances))
  closest_names <- names(sorted_distances)[1:n_neighbors]
  closest_distances <- sorted_distances[1:n_neighbors]
  
  # Create output list with similarities
  neighbor_list <- data.frame(
    rank = 1:n_neighbors,
    first_name = closest_names,
    distance = round(closest_distances, 4),
    similarity = paste0(round((1 - closest_distances) * 100, 1), "%"),
    stringsAsFactors = FALSE
  )
  
  # Create network with ONLY the target name and its closest neighbors
  cluster_names <- c(firstname_of_interest, closest_names)
  
  # Create edges from target name to each neighbor (star network)
  edges <- data.frame()
  for (neighbor in closest_names) {
    dist <- stringdist(firstname_of_interest, neighbor, method = "jw")
    edges <- rbind(edges, data.frame(
      from = firstname_of_interest,
      to = neighbor,
      distance = dist,
      similarity = round((1 - dist) * 100, 1),
      stringsAsFactors = FALSE
    ))
  }
  
  # Create network visualization
  if (nrow(edges) > 0) {
    # Determine node colors: target = red, neighbors = teal
    node_colors <- sapply(cluster_names, function(name) {
      if (name == firstname_of_interest) return("#ff6b6b")
      return("#4ecdc4")
    })
    
    # Determine node sizes: target larger, neighbors smaller
    node_sizes <- sapply(cluster_names, function(name) {
      if (name == firstname_of_interest) return(35)
      return(22)
    })
    
    nodes <- data.frame(
      id = cluster_names,
      label = cluster_names,
      title = cluster_names,
      color = node_colors,
      size = node_sizes,
      stringsAsFactors = FALSE
    )
    
    edges_vis <- data.frame(
      from = edges$from,
      to = edges$to,
      label = paste0(edges$similarity, "%"),
      title = paste0("Similarity: ", edges$similarity, "%"),
      width = edges$similarity / 15,
      stringsAsFactors = FALSE
    )
    
    network_plot <- visNetwork(nodes, edges_vis, 
                               main = paste("Name Cluster for:", firstname_of_interest)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(smooth = TRUE, color = list(color = "#848484", highlight = "#ff6b6b")) %>%
      visPhysics(stabilization = TRUE) %>%
      visLayout(randomSeed = 42)
  } else {
    network_plot <- NULL
    cat("No network created - no names within threshold.\n")
  }
  
  # Print the neighbor list
  cat("\n=== CLOSEST NEIGHBORS FOR:", firstname_of_interest, "===\n")
  print(neighbor_list, row.names = FALSE)
  cat("\n")
  
  # Return list with both outputs
  result <- list(
    closest_neighbors = neighbor_list,
    network_diagram = network_plot
  )
  
  return(result)
}

###############

# Main function to find similar first names and create network
find_similar_firstnames <- function(firstname_of_interest, number_names = 5, threshold = 0.2) {
  
  # Check if name exists
  if (!firstname_of_interest %in% first_names) {
    stop(paste("Name '", firstname_of_interest, "' not found in the dataset!", sep = ""))
  }
  
  # Calculate distances from target name to all names
  distances <- stringdist(firstname_of_interest, first_names, method = "jw")
  names(distances) <- first_names
  
  # Remove the target name itself
  distances <- distances[names(distances) != firstname_of_interest]
  
  # Get the top number_names closest neighbors
  sorted_distances <- sort(distances)
  n_neighbors <- min(number_names, length(sorted_distances))
  closest_names <- names(sorted_distances)[1:n_neighbors]
  closest_distances <- sorted_distances[1:n_neighbors]
  
  # Create output list with similarities
  neighbor_list <- data.frame(
    rank = 1:n_neighbors,
    first_name = closest_names,
    distance = round(closest_distances, 4),
    similarity = paste0(round((1 - closest_distances) * 100, 1), "%"),
    stringsAsFactors = FALSE
  )
  
  # Create network including target name and neighbors within threshold
  # Plus any names that are within threshold of the neighbors
  cluster_names <- c(firstname_of_interest, closest_names)
  
  # Add names that are similar to any of our cluster members
  all_cluster <- unique(cluster_names)
  for (name in cluster_names) {
    dists <- stringdist(name, first_names, method = "jw")
    similar <- first_names[dists < threshold & dists > 0]
    all_cluster <- unique(c(all_cluster, similar))
  }
  
  # Create edges between similar names in cluster
  edges <- data.frame()
  for (i in 1:(length(all_cluster)-1)) {
    for (j in (i+1):length(all_cluster)) {
      dist <- stringdist(all_cluster[i], all_cluster[j], method = "jw")
      if (dist < threshold) {
        edges <- rbind(edges, data.frame(
          from = all_cluster[i],
          to = all_cluster[j],
          distance = dist,
          similarity = round((1 - dist) * 100, 1),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Create network visualization
  if (nrow(edges) > 0) {
    # Determine node colors: target = red, top neighbors = orange, others = teal
    node_colors <- sapply(all_cluster, function(name) {
      if (name == firstname_of_interest) return("#ff6b6b")
      if (name %in% closest_names) return("#ffa500")
      return("#4ecdc4")
    })
    
    # Determine node sizes
    node_sizes <- sapply(all_cluster, function(name) {
      if (name == firstname_of_interest) return(35)
      if (name %in% closest_names) return(25)
      return(18)
    })
    
    nodes <- data.frame(
      id = all_cluster,
      label = all_cluster,
      title = all_cluster,
      color = node_colors,
      size = node_sizes,
      stringsAsFactors = FALSE
    )
    
    edges_vis <- data.frame(
      from = edges$from,
      to = edges$to,
      label = paste0(edges$similarity, "%"),
      title = paste0("Similarity: ", edges$similarity, "%"),
      width = edges$similarity / 15,
      stringsAsFactors = FALSE
    )
    
    network_plot <- visNetwork(nodes, edges_vis, 
                               main = paste("Name Cluster for:", firstname_of_interest)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(smooth = TRUE, color = list(color = "#848484", highlight = "#ff6b6b")) %>%
      visPhysics(stabilization = TRUE) %>%
      visLayout(randomSeed = 42)
  } else {
    network_plot <- NULL
    cat("No network created - no names within threshold.\n")
  }
  
  # Print the neighbor list
  cat("\n=== CLOSEST NEIGHBORS FOR:", firstname_of_interest, "===\n")
  print(neighbor_list, row.names = FALSE)
  cat("\n")
  
  # Return list with both outputs
  result <- list(
    closest_neighbors = neighbor_list,
    network_diagram = network_plot
  )
  
  return(result)
}

find_similar_firstnames(firstname_of_interest = "Apple", 
                        number_names = 30, 
                        threshold = 0.2)