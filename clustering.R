# load libraries
install.packages("patchwork")
install.packages("plotly")
library(tidyverse)
library(cluster)
library(ggplot2)
library(patchwork)
library(plotly)
library(htmlwidgets)

## ---------------------- Task 1 ---------------------- ##

# function to generate the data below
generate_hypercube_clusters <- function(n, k, side_length, noise_sd = 1.0) {
  points_list <- list()
  labels <- c()
  
  # creating a matrix (n x n) with side length on the diagonal, zeroes otherwise
  # each row is a cluster center at a positive corner of the cube
  # the positive corners are exactly the points where one coordinate = side_length, all others = 0
  cluster_centers <- diag(side_length, n, n)
  
  for (i in 1:n) {
    # Generate k points around this center with Gaussian noise
    cluster_points <- matrix(rnorm(k * n, mean = 0, sd = noise_sd), nrow = k, ncol = n) + 
      matrix(rep(cluster_centers[i, ], each = k), nrow = k)
    points_list[[i]] <- cluster_points
    labels <- c(labels, rep(i, k))
  }
  
  points <- do.call(rbind, points_list)
  return(list(points = points, labels = labels))
}

# let's use the function above to create datasets where clusters move progressively closer. Running a simulation below.
k <- 100 # points per cluster
noise_sd <- 1.0 # keep noise at 1 sd
dimensions <- c(6,5,4,3,2) # n
side_lengths <- 10:1 #for each dimension, we need to test side lengths from 10 down to 1

# for storing results
results <- data.frame()

# Loop over dimensions and side lengths
for (n in dimensions) {
  for (L in side_lengths) {
    cat("n =", n, "side_length =", L, "\n")
    
    # Generate data using my function above
    data <- generate_hypercube_clusters(n = n, k = k, side_length = L, noise_sd = noise_sd)
    points <- data$points
    
    # calculating the gap statistic with clusGap! using kmeans as our input function
    # specify nstart = 20 and iter.max = 50 to ensure good clustering
    gap_stat <- clusGap(points, FUN = function(x, k) {
      kmeans(x, centers = k, nstart = 20, iter.max = 50)
    },
    K.max = n+2, B = 20)
    
    # let's find the best number of clusters according to Gap statistic
    optimal_k <- maxSE(gap_stat$Tab[,"gap"], gap_stat$Tab[,"SE.sim"], method = "firstSEmax")
    
    # Store results here
    results <- rbind(results, data.frame(n = n, side_length = L, estimated_k = optimal_k))
  }
}

# View results
print(results)


# make n as integer for plotting
results$n <- as.integer(results$n)

# List to store plots
plot_list <- list()

for (dim in sort(unique(results$n), decreasing = TRUE)) {
  df <- subset(results, n == dim)
  
  # Separate dataframe for the red reference line
  ref_line <- data.frame(yintercept = dim, label = "True number of clusters")
  
  p <- ggplot(df, aes(x = side_length, y = estimated_k, group = 1)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(color = "steelblue", size = 2) +
    # Now use data=ref_line for the red reference line
    geom_hline(data = ref_line, aes(yintercept = yintercept, color = label),
               linetype = "dashed", size = 1) +
    scale_color_manual(values = c("True number of clusters" = "red")) +
    scale_x_reverse(breaks = 1:10) +
    coord_cartesian(ylim = c(0, 8)) +
    labs(
      title = paste("Dimensions (n):", dim),
      x = "side length",
      y = "Estimated # of clusters",
      color = ""  # Legend title
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 10),
      strip.text = element_text(size = 10),
      legend.position = "bottom"
    )
  
  plot_list[[as.character(dim)]] <- p
}

# Combine into a grid: 3 in top row, 2 in bottom
combined_plot <- (plot_list[["6"]] | plot_list[["5"]] | plot_list[["4"]]) / 
  (plot_list[["3"]] | plot_list[["2"]]) +
  plot_layout(guides = "collect")

combined_plot

ggsave("figures/combined_plot.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

# Write up explaining results are in the Rmd file!


## ---------------------- Task 2 ---------------------- ##

generate_shell_clusters <- function(n_shells, k_per_shell, max_radius, noise_sd = 0.1) {
  # radius values from 1 to max evenly spaced for num of shells needed
  radius_values <- seq(1, max_radius, length.out = n_shells)
  
  # for storing results
  all_points <- data.frame()
  
  for (r in radius_values) {
    # let's get random points on sphere 
    theta <- runif(k_per_shell, 0, 2 * pi)
    phi <- acos(runif(k_per_shell, -1, 1))
    
    # add noise to radius, thickness to each shell
    noisy_r <- r + rnorm(k_per_shell, mean = 0, sd = noise_sd)
    
    # get 3D position for each point on the shell for plotting purposes
    x <- noisy_r * sin(phi) * cos(theta)
    y <- noisy_r * sin(phi) * sin(theta)
    z <- noisy_r * cos(phi)
    
    # make small df for one shell and then combine with previous shells
    shell_df <- data.frame(x, y, z, shell = as.factor(r))
    all_points <- rbind(all_points, shell_df)
  }
  
  return(all_points)
}

#Now, let's generate a sample dataset and create an interactive 3D scatter plot using plotly to confirm this structure.

# randomly picking param values for sample dataset
sample_data <- generate_shell_clusters(n_shells = 3, k_per_shell = 400, max_radius = 3)

# relabel shells for legend in plot
sample_data$shell_label <- factor(paste("Shell", 1:nlevels(sample_data$shell))[as.integer(sample_data$shell)])

# 3D scatter plot
p3d <- plot_ly(sample_data, 
               x = ~x, y = ~y, z = ~z, 
               color = ~shell_label, 
               colors = "Set2",
               type = "scatter3d",
               mode = "markers",
               marker = list(size = 3)) %>%
  layout(
    title = list(text = "3D Concentric Shells test data", font = list(size = 16)),
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "Z")
    ),
    legend = list(title = list(text = "Shell"))
  )

# Save the interactive 3D plot as an HTML file
htmlwidgets::saveWidget(p3d, file = "figures/3d_shell_plot.html", selfcontained = TRUE)

#spectral clustering implementation
spectral_clustering <- function(x, k, d_threshold = 1) {
  # Step 1: Build adjacency matrix
  dist_mat <- as.matrix(dist(x))
  A <- (dist_mat < d_threshold) * 1
  diag(A) <- 0
  
  # Step 2: Compute degree and Laplacian matrices
  D <- diag(rowSums(A))
  L <- D - A
  
  # Step 3: Compute normalized Laplacian L_sym = D^{-1/2} * L * D^{-1/2}
  D_inv_sqrt <- diag(1 / sqrt(diag(D)))
  D_inv_sqrt[!is.finite(D_inv_sqrt)] <- 0  # handles isolated points safely
  L_sym <- D_inv_sqrt %*% L %*% D_inv_sqrt
  
  # Step 4: Eigen decomposition
  eig <- eigen(L_sym, symmetric = TRUE)
  
  # smallest k non-zero eigenvectors
  tol <- 1e-10
  nonzero_idx <- which(eig$values > tol)       # filter out numerical zeros
  eig_order <- order(eig$values[nonzero_idx])  # ascending order of non-zero eigenvalues
  U <- eig$vectors[, nonzero_idx[eig_order[1:k]]]  # pick k smallest non-zero eigenvectors
  
  # K-means on rows of U
  # normalize U?
  U <- U / sqrt(rowSums(U^2))
  U[!is.finite(U)] <- 0
  km <- kmeans(U, centers = k, nstart = 20, iter.max = 200)
  
  return(km)
}

# run simulation
# parameters for simulation
radius_values <- seq(10, 0, by = -1)
# constants below
n_shells <- 4
k_per_shell <- 100
noise_sd <- 0.1

results_shells <- data.frame(max_radius = numeric(), estimated_k = numeric())

for (r in radius_values) {
  # generate dataset
  data_shells <- generate_shell_clusters(
    n_shells = n_shells,
    k_per_shell = k_per_shell,
    max_radius = r,
    noise_sd = noise_sd
  )
  
  # just extract the points
  points <- as.matrix(data_shells[, c("x", "y", "z")])
  
  #set.seed(123)
  # run gap statistic using spectral clustering
  gap <- clusGap(points,
                 FUN = function(x, k){ spectral_clustering(x, k, d_threshold = 1)},
                 K.max = n_shells + 2,
                 B = 20)
  
  
  est_k <- maxSE(gap$Tab[,"gap"], gap$Tab[,"SE.sim"], method = "firstSEmax")
  
  results_shells <- rbind(results_shells, data.frame(max_radius = r, estimated_k = est_k))
}

results_shells

## plot and save
# Make sure max_radius is numeric for plotting
results_shells$max_radius <- as.numeric(results_shells$max_radius)

# Reference line for true number of clusters
true_k <- 4
ref_line <- data.frame(yintercept = true_k, label = "True number of clusters")

# Plot
spectral_plot <- ggplot(results_shells, aes(x = max_radius, y = estimated_k, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(data = ref_line, aes(yintercept = yintercept, color = label),
             linetype = "dashed", size = 1) +
  scale_color_manual(values = c("True number of clusters" = "red")) +
  scale_x_reverse(breaks = sort(unique(results_shells$max_radius))) +
  coord_cartesian(ylim = c(0, max(results_shells$estimated_k) + 1)) +
  labs(
    title = "Estimated number of clusters vs max radius",
    x = "Maximum radius",
    y = "Estimated # of clusters",
    color = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("figures/spectral_plot.png", plot = spectral_plot, width = 10, height = 6, dpi = 300)

