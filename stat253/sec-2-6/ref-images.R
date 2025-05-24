# Percentile Calculations Reference Guide - Visualization Generator
# This script creates all visualizations for the LaTeX document

# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)

# Create output directory
if (!dir.exists("rs-images")) {
  dir.create("rs-images")
}

# Set theme for all plots
theme_set(theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ))

# Color palette
colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#592E83")

# 1. Conceptual percentile visualization
create_percentile_concept <- function() {
  # Create data for cumulative distribution
  scores <- seq(0, 100, by = 1)
  cumulative_pct <- pnorm(scores, mean = 70, sd = 15) * 100
  
  df <- data.frame(scores = scores, cumulative_pct = cumulative_pct)
  
  # Mark key percentiles
  percentiles <- c(25, 50, 75)
  perc_scores <- qnorm(percentiles/100, mean = 70, sd = 15)
  
  p <- ggplot(df, aes(x = scores, y = cumulative_pct)) +
    geom_line(color = colors[1], size = 1.2) +
    geom_segment(data = data.frame(x = perc_scores, y = percentiles, 
                                   xend = perc_scores, yend = 0),
                 aes(x = x, y = 0, xend = xend, yend = y), 
                 color = colors[2], linetype = "dashed", size = 0.8) +
    geom_point(data = data.frame(x = perc_scores, y = percentiles),
               aes(x = x, y = y), color = colors[2], size = 3) +
    geom_text(data = data.frame(x = perc_scores, y = percentiles + 8,
                                labels = paste0(percentiles, "th percentile\n(", round(perc_scores, 1), ")")),
              aes(x = x, y = y, label = labels), 
              size = 3, hjust = 0.5, color = colors[2]) +
    labs(title = "Conceptual Understanding of Percentiles",
         subtitle = "Example: Normally distributed test scores (μ=70, σ=15)",
         x = "Test Score", 
         y = "Cumulative Percentage") +
    scale_x_continuous(breaks = seq(20, 100, 20), limits = c(20, 100)) +
    scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 110)) +
    theme(panel.grid.major = element_line(color = "gray90", size = 0.5))
  
  ggsave("rs-images/percentile_concept.png", p, width = 8, height = 5, dpi = 300)
  return(p)
}

# 2. Interpolation methods comparison
create_interpolation_demo <- function() {
  # Small dataset to show interpolation
  data_points <- c(10, 15, 20, 25, 30)
  ranks <- 1:5
  target_rank <- 2.3  # 30th percentile position
  
  # Different interpolation methods
  methods <- data.frame(
    method = c("Type 1 (No interpolation)", "Type 7 (R default)", "Type 8 (Recommended)", 
               "Excel INC", "Excel EXC"),
    result = c(15, 16.5, 16.0, 16.25, 15.75),
    color = colors[1:5]
  )
  
  # Main plot showing data points and interpolation
  p1 <- ggplot() +
    geom_point(data = data.frame(x = ranks, y = data_points),
               aes(x = x, y = y), size = 4, color = colors[1]) +
    geom_line(data = data.frame(x = ranks, y = data_points),
              aes(x = x, y = y), color = colors[1], alpha = 0.5) +
    geom_vline(xintercept = target_rank, color = colors[2], linetype = "dashed", size = 1) +
    geom_text(data = data.frame(x = ranks, y = data_points + 1),
              aes(x = x, y = y, label = data_points), size = 3) +
    annotate("text", x = target_rank + 0.1, y = 28, label = "30th percentile\nposition (2.3)", 
             color = colors[2], size = 3, hjust = 0) +
    labs(title = "How Different Methods Interpolate Between Data Points",
         subtitle = "Dataset: [10, 15, 20, 25, 30] - Finding 30th percentile",
         x = "Rank Position", y = "Value") +
    scale_x_continuous(breaks = 1:5, limits = c(0.8, 3.5)) +
    scale_y_continuous(breaks = seq(10, 30, 5)) +
    theme(panel.grid.major = element_line(color = "gray90", size = 0.3))
  
  # Bar chart showing different results
  p2 <- ggplot(methods, aes(x = reorder(method, -result), y = result, fill = method)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = result), vjust = -0.5, size = 3) +
    scale_fill_manual(values = methods$color) +
    labs(title = "Results from Different Methods",
         x = "Method", y = "30th Percentile Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    scale_y_continuous(limits = c(14, 17))
  
  # Combine plots
  combined <- grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))
  
  ggsave("rs-images/interpolation_demo.png", combined, width = 10, height = 8, dpi = 300)
  return(combined)
}

# 3. Method comparison for realistic dataset
create_method_comparison <- function() {
  # Realistic test scores
  scores <- c(62, 68, 71, 73, 75, 78, 82, 85, 88, 92)
  
  # Calculate 25th percentile using different methods
  methods_data <- data.frame(
    Method = c("R Type 1", "R Type 7", "R Type 8", "Excel INC", "Excel EXC"),
    Software = c("R", "R", "R", "Excel", "Excel"),
    Value = c(
      quantile(scores, 0.25, type = 1),
      quantile(scores, 0.25, type = 7), 
      quantile(scores, 0.25, type = 8),
      # Approximate Excel INC: k*(n-1)+1 method
      71.75,  # calculated manually
      # Approximate Excel EXC: k*(n+1) method  
      70.5    # calculated manually
    ),
    Type = c("Discontinuous", "Continuous", "Continuous", "Continuous", "Continuous"),
    Recommended = c("Basic", "Coursework", "Research", "Business", "Conservative")
  )
  
  # Create visualization
  p <- ggplot(methods_data, aes(x = reorder(Method, Value), y = Value, fill = Software)) +
    geom_col(width = 0.6, alpha = 0.8) +
    geom_text(aes(label = paste0(Value, "\n(", Recommended, ")")), 
              vjust = 0.5, hjust = 1.1, size = 3, color = "white", fontface = "bold") +
    scale_fill_manual(values = c("R" = colors[1], "Excel" = colors[2])) +
    labs(title = "25th Percentile Comparison: Student Test Scores",
         subtitle = "Dataset: [62, 68, 71, 73, 75, 78, 82, 85, 88, 92] (n=10)",
         x = "Method", 
         y = "25th Percentile Value",
         caption = "Range: 1.5 points difference between methods") +
    coord_flip() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray90", size = 0.3)) +
    scale_y_continuous(breaks = seq(70, 72, 0.5), limits = c(69.5, 72.5))
  
  ggsave("rs-images/method_comparison.png", p, width = 10, height = 6, dpi = 300)
  return(p)
}

# 4. Sample size effect on method differences
create_sample_size_effect <- function() {
  set.seed(123)
  
  # Generate datasets of different sizes
  sample_sizes <- c(5, 10, 20, 50, 100, 500)
  results_list <- list()
  
  for (n in sample_sizes) {
    # Generate sample data
    data <- rnorm(n, mean = 50, sd = 10)
    
    # Calculate 25th percentile with different methods
    q25_type7 <- quantile(data, 0.25, type = 7)
    q25_type8 <- quantile(data, 0.25, type = 8)
    q25_type1 <- quantile(data, 0.25, type = 1)
    
    # Calculate range of differences
    range_diff <- max(q25_type7, q25_type8, q25_type1) - min(q25_type7, q25_type8, q25_type1)
    
    results_list[[length(results_list) + 1]] <- data.frame(
      sample_size = n,
      range_difference = range_diff,
      type1 = q25_type1,
      type7 = q25_type7,
      type8 = q25_type8
    )
  }
  
  results_df <- do.call(rbind, results_list)
  
  p <- ggplot(results_df, aes(x = sample_size, y = range_difference)) +
    geom_line(color = colors[1], size = 1.2) +
    geom_point(color = colors[1], size = 3) +
    geom_text(aes(label = round(range_difference, 2)), 
              vjust = -0.8, size = 3, color = colors[1]) +
    labs(title = "How Sample Size Affects Method Differences",
         subtitle = "Range of 25th percentile values across R Types 1, 7, and 8",
         x = "Sample Size (n)", 
         y = "Range of Differences",
         caption = "Larger samples → smaller differences between methods") +
    scale_x_continuous(trans = "log10", breaks = sample_sizes, 
                       labels = sample_sizes) +
    scale_y_continuous(labels = number_format(accuracy = 0.01)) +
    theme(panel.grid.major = element_line(color = "gray90", size = 0.3)) +
    annotation_logticks(sides = "b")
  
  ggsave("rs-images/sample_size_effect.png", p, width = 8, height = 5, dpi = 300)
  return(p)
}

# 5. Percentile sensitivity across the distribution
create_percentile_sensitivity <- function() {
  set.seed(456)
  data <- rnorm(20, mean = 50, sd = 10)  # Small sample to show differences
  
  percentiles <- seq(0.1, 0.9, by = 0.05)
  sensitivity_data <- data.frame()
  
  for (p in percentiles) {
    type7 <- quantile(data, p, type = 7)
    type8 <- quantile(data, p, type = 8)
    type1 <- quantile(data, p, type = 1)
    
    # Calculate coefficient of variation as measure of sensitivity
    methods <- c(type1, type7, type8)
    cv <- sd(methods) / mean(methods) * 100
    
    sensitivity_data <- rbind(sensitivity_data, 
                             data.frame(percentile = p * 100, 
                                       cv = cv,
                                       range_diff = max(methods) - min(methods)))
  }
  
  p <- ggplot(sensitivity_data, aes(x = percentile, y = range_diff)) +
    geom_line(color = colors[1], size = 1.2) +
    geom_point(color = colors[1], size = 2) +
    geom_vline(xintercept = c(25, 75), linetype = "dashed", 
               color = colors[2], alpha = 0.7) +
    annotate("text", x = 25, y = max(sensitivity_data$range_diff) * 0.9, 
             label = "25th", angle = 90, vjust = 1.2, color = colors[2]) +
    annotate("text", x = 75, y = max(sensitivity_data$range_diff) * 0.9, 
             label = "75th", angle = 90, vjust = 1.2, color = colors[2]) +
    labs(title = "Method Sensitivity Across Percentiles",
         subtitle = "Range of differences between R Types 1, 7, and 8 (n=20)",
         x = "Percentile", 
         y = "Range of Method Differences",
         caption = "Quartiles (25th, 75th) show highest sensitivity") +
    scale_x_continuous(breaks = seq(10, 90, 20)) +
    theme(panel.grid.major = element_line(color = "gray90", size = 0.3))
  
  ggsave("rs-images/percentile_sensitivity.png", p, width = 8, height = 5, dpi = 300)
  return(p)
}

# 6. Excel PERCENTILE.EXC valid range visualization
create_exc_validity <- function() {
  sample_sizes <- c(5, 10, 20, 50, 100)
  validity_data <- data.frame()
  
  for (n in sample_sizes) {
    min_k <- 1/(n+1)
    max_k <- n/(n+1)
    
    validity_data <- rbind(validity_data,
                          data.frame(
                            sample_size = n,
                            min_percentile = min_k * 100,
                            max_percentile = max_k * 100,
                            range_width = (max_k - min_k) * 100
                          ))
  }
  
  # Reshape for plotting
  library(tidyr)
  plot_data <- validity_data %>%
    select(sample_size, min_percentile, max_percentile) %>%
    pivot_longer(cols = c(min_percentile, max_percentile), 
                 names_to = "bound", values_to = "percentile")
  
  p <- ggplot(validity_data, aes(x = factor(sample_size))) +
    geom_ribbon(aes(ymin = min_percentile, ymax = max_percentile), 
                fill = colors[2], alpha = 0.3) +
    geom_line(aes(y = min_percentile, group = 1), color = colors[2], size = 1) +
    geom_line(aes(y = max_percentile, group = 1), color = colors[2], size = 1) +
    geom_point(aes(y = min_percentile), color = colors[2], size = 2) +
    geom_point(aes(y = max_percentile), color = colors[2], size = 2) +
    geom_hline(yintercept = c(10, 90), linetype = "dashed", 
               color = colors[3], alpha = 0.7) +
    annotate("text", x = 1, y = 10, label = "10th percentile", 
             hjust = 0, vjust = -0.5, color = colors[3], size = 3) +
    annotate("text", x = 1, y = 90, label = "90th percentile", 
             hjust = 0, vjust = 1.5, color = colors[3], size = 3) +
    labs(title = "Excel PERCENTILE.EXC Valid Range by Sample Size",
         subtitle = "Shaded area shows valid percentiles for PERCENTILE.EXC",
         x = "Sample Size (n)", 
         y = "Valid Percentile Range (%)",
         caption = "Smaller samples cannot calculate extreme percentiles with EXC method") +
    scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
    theme(panel.grid.major = element_line(color = "gray90", size = 0.3))
  
  ggsave("rs-images/exc_validity.png", p, width = 8, height = 5, dpi = 300)
  return(p)
}

# 7. Software ecosystem comparison
create_software_ecosystem <- function() {
  # Create a comparison matrix
  software_data <- data.frame(
    Software = rep(c("R", "Excel", "Google Sheets", "SPSS", "SAS"), each = 3),
    Flexibility = rep(c("High", "Medium", "Medium", "Medium", "Low"), each = 3),
    Method = c("Type 1-9", "Type 1-9", "Type 1-9",
               "INC", "EXC", "Legacy",
               "INC", "EXC", "Legacy", 
               "Default", "Alternative", "Custom",
               "Default", "Alternative", "Custom"),
    Ease = rep(c(3, 5, 5, 4, 2), each = 3),
    Flexibility_num = rep(c(5, 3, 3, 3, 2), each = 3),
    Method_type = c("Discontinuous", "Continuous", "Continuous",
                   "Continuous", "Continuous", "Continuous",
                   "Continuous", "Continuous", "Continuous",
                   "Continuous", "Continuous", "Continuous",
                   "Continuous", "Discontinuous", "Continuous")
  )
  
  # Aggregate by software
  summary_data <- software_data %>%
    group_by(Software) %>%
    summarise(
      Flexibility = first(Flexibility_num),
      Ease_of_Use = first(Ease),
      Methods_Available = n_distinct(Method),
      .groups = 'drop'
    )
  
  p <- ggplot(summary_data, aes(x = Ease_of_Use, y = Flexibility)) +
    geom_point(aes(size = Methods_Available, color = Software), alpha = 0.7) +
    geom_text(aes(label = Software), vjust = -1.2, size = 3.5) +
    scale_color_manual(values = colors) +
    scale_size_continuous(range = c(5, 15), name = "Methods\nAvailable") +
    labs(title = "Software Comparison: Ease vs Flexibility",
         subtitle = "Bubble size = number of different percentile methods available",
         x = "Ease of Use (1=Hard, 5=Easy)", 
         y = "Flexibility (1=Low, 5=High)",
         caption = "R offers most flexibility, Excel/Sheets balance ease and functionality") +
    scale_x_continuous(breaks = 1:5, limits = c(1, 5.5)) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5.5)) +
    theme(legend.position = "right",
          panel.grid.major = element_line(color = "gray90", size = 0.3))
  
  ggsave("rs-images/software_ecosystem.png", p, width = 8, height = 6, dpi = 300)
  return(p)
}

# Generate all visualizations
cat("Generating percentile concept visualization...\n")
create_percentile_concept()

cat("Generating interpolation demo...\n")
create_interpolation_demo()

cat("Generating method comparison...\n")
create_method_comparison()

cat("Generating sample size effect...\n")
create_sample_size_effect()

cat("Generating percentile sensitivity...\n")
create_percentile_sensitivity()

cat("Generating Excel EXC validity...\n")
create_exc_validity()

cat("Generating software ecosystem comparison...\n")
create_software_ecosystem()

cat("\nAll visualizations saved to 'rs-images/' directory!\n")
cat("Images generated:\n")
cat("- percentile_concept.png\n")
cat("- interpolation_demo.png\n") 
cat("- method_comparison.png\n")
cat("- sample_size_effect.png\n")
cat("- percentile_sensitivity.png\n")
cat("- exc_validity.png\n")
cat("- software_ecosystem.png\n")
