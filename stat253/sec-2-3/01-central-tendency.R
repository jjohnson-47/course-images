# Central Tendency Measures - Basic Calculations
# Mean, Median, and Mode with EPA data

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Central Tendency Analysis\n")
cat("========================\n\n")

# Example 1: Gas mileage calculation from HTML
gas_mileage <- c(36.3, 37.1, 35.9, 38.2, 36.8)

cat("Example 1: Gas Mileage Data\n")
cat("Data:", paste(gas_mileage, collapse = ", "), "\n")
cat("n =", length(gas_mileage), "\n\n")

# Mean calculation
mean_value <- mean(gas_mileage)
cat("MEAN:\n")
cat("Formula: (Sum of all values) / n\n")
cat("Calculation:", paste(gas_mileage, collapse = " + "), "=", sum(gas_mileage), "\n")
cat("Mean =", sum(gas_mileage), "/", length(gas_mileage), "=", round(mean_value, 2), "mpg\n\n")

# Median calculation  
sorted_data <- sort(gas_mileage)
median_value <- median(gas_mileage)
cat("MEDIAN:\n")
cat("Sorted data:", paste(sorted_data, collapse = ", "), "\n")
cat("Middle position:", ceiling(length(gas_mileage)/2), "\n")
cat("Median =", median_value, "mpg\n\n")

# Mode (create function since R doesn't have built-in mode)
get_mode <- function(x) {
  freq_table <- table(x)
  max_freq <- max(freq_table)
  modes <- names(freq_table)[freq_table == max_freq]
  if(max_freq == 1) {
    return("No mode (all values appear once)")
  } else {
    return(paste("Mode(s):", paste(modes, collapse = ", "), 
                 "- appears", max_freq, "times"))
  }
}

cat("MODE:\n")
cat(get_mode(gas_mileage), "\n\n")

# Example 2: Larger EPA dataset
epa_data <- c(32.7, 36.3, 37.1, 35.9, 38.2, 41.0, 36.8, 37.9, 39.0, 33.9)

cat("Example 2: EPA Mileage Dataset (n=10)\n")
cat("Data:", paste(sort(epa_data), collapse = ", "), "\n\n")

cat("Summary Statistics:\n")
cat("Mean =", round(mean(epa_data), 2), "mpg\n")
cat("Median =", median(epa_data), "mpg\n")
cat(get_mode(epa_data), "\n\n")

# Save summary to file
sink("output/central_tendency_summary.txt")
cat("Central Tendency Analysis Results\n")
cat("=================================\n\n")
cat("Gas Mileage Example (n=5):\n")
cat("Data:", paste(gas_mileage, collapse = ", "), "\n")
cat("Mean:", round(mean_value, 2), "mpg\n")
cat("Median:", median_value, "mpg\n\n")
cat("EPA Dataset (n=10):\n")
cat("Data:", paste(sort(epa_data), collapse = ", "), "\n")
cat("Mean:", round(mean(epa_data), 2), "mpg\n")
cat("Median:", median(epa_data), "mpg\n")
sink()

cat("✓ Summary saved to output/central_tendency_summary.txt\n")
