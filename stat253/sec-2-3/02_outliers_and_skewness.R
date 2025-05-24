# Central Tendency - Outlier Effects and Skewness
# Demonstrates how outliers affect mean vs median

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Outlier Effects and Skewness Analysis\n")
cat("====================================\n\n")

# Original EPA data
original_data <- c(32.7, 36.3, 37.1, 35.9, 38.2, 41.0, 36.8, 37.9, 39.0, 33.9)

# Add outliers to demonstrate effect
with_outlier <- c(original_data, 55.0)  # High outlier
with_low_outlier <- c(original_data, 15.0)  # Low outlier

cat("Outlier Effects Comparison:\n")
cat("==========================\n\n")

# Function to analyze dataset
analyze_data <- function(data, name) {
  cat(name, ":\n")
  cat("Data:", paste(sort(data), collapse = ", "), "\n")
  cat("Mean:", round(mean(data), 2), "\n")
  cat("Median:", round(median(data), 2), "\n")
  cat("Difference (Mean - Median):", round(mean(data) - median(data), 2), "\n")
  
  if(mean(data) > median(data)) {
    cat("Distribution: Right-skewed (positive skew)\n")
  } else if(mean(data) < median(data)) {
    cat("Distribution: Left-skewed (negative skew)\n")
  } else {
    cat("Distribution: Symmetric\n")
  }
  cat("\n")
}

analyze_data(original_data, "Original Data (no outliers)")
analyze_data(with_outlier, "With High Outlier (55.0)")
analyze_data(with_low_outlier, "With Low Outlier (15.0)")

# Create visualization
png("output/outlier_effects.png", width = 1000, height = 600, res = 120)
par(mfrow = c(1, 3))

# Plot 1: Original data
hist(original_data, main = "Original Data", xlab = "MPG", 
     col = "lightblue", xlim = c(15, 60))
abline(v = mean(original_data), col = "red", lwd = 2, lty = 2)
abline(v = median(original_data), col = "blue", lwd = 2, lty = 2)
legend("topright", c("Mean", "Median"), col = c("red", "blue"), lty = 2)

# Plot 2: With high outlier
hist(with_outlier, main = "With High Outlier", xlab = "MPG", 
     col = "lightcoral", xlim = c(15, 60))
abline(v = mean(with_outlier), col = "red", lwd = 2, lty = 2)
abline(v = median(with_outlier), col = "blue", lwd = 2, lty = 2)

# Plot 3: With low outlier  
hist(with_low_outlier, main = "With Low Outlier", xlab = "MPG", 
     col = "lightgreen", xlim = c(15, 60))
abline(v = mean(with_low_outlier), col = "red", lwd = 2, lty = 2)
abline(v = median(with_low_outlier), col = "blue", lwd = 2, lty = 2)

dev.off()
cat("✓ Outlier effects plot saved to output/outlier_effects.png\n")

# Household income example (skewness)
cat("Skewness Example: Household Income\n")
cat("==================================\n")
# Simulate right-skewed income data (like in HTML example)
set.seed(123)
incomes <- c(35000, 42000, 38000, 51000, 45000, 39000, 48000, 125000)  # One high earner

cat("Household incomes: $", paste(sort(incomes), collapse = ", $"), "\n")
cat("Mean income: $", round(mean(incomes), 0), "\n")
cat("Median income: $", median(incomes), "\n")
cat("The mean is", ifelse(mean(incomes) > median(incomes), "higher", "lower"), 
    "than the median, indicating", 
    ifelse(mean(incomes) > median(incomes), "right (positive)", "left (negative)"), 
    "skew\n\n")

cat("Key Insights:\n")
cat("- Mean is sensitive to outliers\n")
cat("- Median is resistant to outliers\n") 
cat("- Right skew: Mean > Median\n")
cat("- Left skew: Mean < Median\n")
cat("- Symmetric: Mean ≈ Median\n")
