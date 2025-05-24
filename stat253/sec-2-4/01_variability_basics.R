# Variability Measures - Basic Calculations
# Range, Variance, and Standard Deviation

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Measures of Variability\n")
cat("======================\n\n")

# Table 2.5 data from textbook - perfect teaching example
sample1 <- c(1, 2, 3, 4, 5)
sample2 <- c(2, 3, 3, 3, 4)

cat("Table 2.5 Data from Textbook:\n")
cat("Sample 1:", paste(sample1, collapse = ", "), "\n")
cat("Sample 2:", paste(sample2, collapse = ", "), "\n\n")

# Function to show step-by-step variance calculation
show_variance_calculation <- function(data, sample_name) {
  cat(sample_name, "Analysis:\n")
  cat(rep("=", nchar(sample_name) + 10), "\n", sep = "")
  
  n <- length(data)
  mean_val <- mean(data)
  
  cat("Data:", paste(data, collapse = ", "), "\n")
  cat("n =", n, "\n")
  cat("Mean = (", paste(data, collapse = " + "), ") / ", n, " = ", mean_val, "\n\n")
  
  # Calculate deviations
  deviations <- data - mean_val
  cat("Deviations from mean:\n")
  for(i in 1:length(data)) {
    cat("  ", data[i], "- ", mean_val, "=", deviations[i], "\n")
  }
  
  # Calculate squared deviations
  squared_devs <- deviations^2
  cat("\nSquared deviations:\n")
  for(i in 1:length(data)) {
    cat("  (", deviations[i], ")² =", squared_devs[i], "\n")
  }
  
  # Sample variance (using n-1)
  sum_squared_devs <- sum(squared_devs)
  sample_var <- sum_squared_devs / (n - 1)
  sample_sd <- sqrt(sample_var)
  
  cat("\nSample Variance (s²):\n")
  cat("s² = Σ(xᵢ - x̄)² / (n-1)\n")
  cat("s² = (", paste(squared_devs, collapse = " + "), ") / (", n, "- 1)\n")
  cat("s² =", sum_squared_devs, "/", (n-1), "=", round(sample_var, 4), "\n")
  
  cat("\nSample Standard Deviation (s):\n")
  cat("s = √s² = √", round(sample_var, 4), "=", round(sample_sd, 4), "\n")
  
  # Range
  data_range <- max(data) - min(data)
  cat("\nRange:\n")
  cat("Range = Max - Min =", max(data), "-", min(data), "=", data_range, "\n\n")
  
  # Return values for comparison
  return(list(range = data_range, variance = sample_var, sd = sample_sd))
}

# Analyze both samples
results1 <- show_variance_calculation(sample1, "Sample 1")
results2 <- show_variance_calculation(sample2, "Sample 2")

# Comparison
cat("COMPARISON:\n")
cat("===========\n")
cat("Both samples have the same mean (3) but different variability:\n\n")

comparison_table <- data.frame(
  Measure = c("Range", "Variance (s²)", "Std Dev (s)"),
  Sample1 = c(results1$range, round(results1$variance, 4), round(results1$sd, 4)),
  Sample2 = c(results2$range, round(results2$variance, 4), round(results2$sd, 4))
)
print(comparison_table)

cat("\nInterpretation:\n")
cat("Sample 1 is MORE variable (larger variance and standard deviation)\n")
cat("Sample 2 is LESS variable (smaller variance and standard deviation)\n")
cat("Both have the same range, showing why range can be misleading!\n\n")

# Verify with R built-in functions
cat("Verification with R Functions:\n")
cat("=============================\n")
cat("Sample 1: var() =", var(sample1), ", sd() =", sd(sample1), "\n")
cat("Sample 2: var() =", var(sample2), ", sd() =", sd(sample2), "\n")

# Save summary
sink("output/variability_summary.txt")
cat("Variability Analysis Summary\n")
cat("============================\n\n")
print(comparison_table)
cat("\nKey Concept: Same center, different spread!\n")
sink()

cat("\n✓ Summary saved to output/variability_summary.txt\n")
