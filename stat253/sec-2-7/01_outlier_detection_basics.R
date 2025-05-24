# Methods for Detecting Outliers - Basic Concepts
# Box plots, IQR, and fence calculations

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Methods for Detecting Outliers\n")
cat("==============================\n\n")

# Simple example for teaching IQR and fence concepts
simple_data <- c(7, 12, 15, 18, 23)
cat("Simple Example for IQR Calculation:\n")
cat("===================================\n")
cat("Data:", paste(simple_data, collapse = ", "), "\n\n")

# Step-by-step IQR calculation
quartiles_simple <- quantile(simple_data, probs = c(0.25, 0.5, 0.75))
iqr_simple <- IQR(simple_data)

cat("STEP-BY-STEP IQR CALCULATION:\n")
cat("=============================\n")
cat("Q1 (25th percentile):", quartiles_simple[1], "\n")
cat("Q2 (50th percentile/median):", quartiles_simple[2], "\n")
cat("Q3 (75th percentile):", quartiles_simple[3], "\n")
cat("IQR = Q3 - Q1 =", quartiles_simple[3], "-", quartiles_simple[1], "=", iqr_simple, "\n\n")

# Fence calculations
cat("FENCE CALCULATIONS:\n")
cat("==================\n")
# Inner fences (1.5 × IQR)
inner_lower <- quartiles_simple[1] - 1.5 * iqr_simple
inner_upper <- quartiles_simple[3] + 1.5 * iqr_simple

# Outer fences (3 × IQR)  
outer_lower <- quartiles_simple[1] - 3 * iqr_simple
outer_upper <- quartiles_simple[3] + 3 * iqr_simple

cat("Inner Fences (1.5 × IQR):\n")
cat("  Lower inner fence = Q1 - 1.5×IQR =", quartiles_simple[1], "- 1.5×", iqr_simple, "=", inner_lower, "\n")
cat("  Upper inner fence = Q3 + 1.5×IQR =", quartiles_simple[3], "+ 1.5×", iqr_simple, "=", inner_upper, "\n\n")

cat("Outer Fences (3 × IQR):\n")
cat("  Lower outer fence = Q1 - 3×IQR =", quartiles_simple[1], "- 3×", iqr_simple, "=", outer_lower, "\n")
cat("  Upper outer fence = Q3 + 3×IQR =", quartiles_simple[3], "+ 3×", iqr_simple, "=", outer_upper, "\n\n")

# Outlier detection for simple data
cat("OUTLIER DETECTION:\n")
cat("=================\n")
cat("Data points:", paste(simple_data, collapse = ", "), "\n")
cat("Inner fence range: [", inner_lower, ", ", inner_upper, "]\n")
cat("Outer fence range: [", outer_lower, ", ", outer_upper, "]\n")

# Check for outliers
mild_outliers <- simple_data[simple_data < inner_lower | simple_data > inner_upper]
extreme_outliers <- simple_data[simple_data < outer_lower | simple_data > outer_upper]

if(length(mild_outliers) > 0) {
  cat("Mild outliers (beyond inner fences):", paste(mild_outliers, collapse = ", "), "\n")
} else {
  cat("No mild outliers detected\n")
}

if(length(extreme_outliers) > 0) {
  cat("Extreme outliers (beyond outer fences):", paste(extreme_outliers, collapse = ", "), "\n")
} else {
  cat("No extreme outliers detected\n")
}
cat("\n")

# Gas mileage data (simulated to match textbook characteristics)
# QL = 35.625, QU = 38.375, median = 37, outliers at 30.0 and 44.9
set.seed(123)
gas_base <- rnorm(98, 37, 1.2)  # 98 normal values
gas_data <- c(gas_base, 30.0, 44.9)  # Add the two outliers

cat("Gas Mileage Data Analysis:\n")
cat("=========================\n")
cat("Sample size:", length(gas_data), "measurements\n")

# Calculate key statistics
gas_quartiles <- quantile(gas_data, probs = c(0.25, 0.5, 0.75))
gas_iqr <- IQR(gas_data)

cat("Five-number summary:\n")
cat("  Minimum:", round(min(gas_data), 2), "\n")
cat("  Q1:", round(gas_quartiles[1], 3), "\n") 
cat("  Median:", round(gas_quartiles[2], 3), "\n")
cat("  Q3:", round(gas_quartiles[3], 3), "\n")
cat("  Maximum:", round(max(gas_data), 2), "\n")
cat("  IQR:", round(gas_iqr, 3), "\n\n")

# Calculate fences for gas data
gas_inner_lower <- gas_quartiles[1] - 1.5 * gas_iqr
gas_inner_upper <- gas_quartiles[3] + 1.5 * gas_iqr
gas_outer_lower <- gas_quartiles[1] - 3 * gas_iqr
gas_outer_upper <- gas_quartiles[3] + 3 * gas_iqr

cat("FENCE CALCULATIONS FOR GAS DATA:\n")
cat("===============================\n")
cat("Inner fences: [", round(gas_inner_lower, 2), ", ", round(gas_inner_upper, 2), "]\n")
cat("Outer fences: [", round(gas_outer_lower, 2), ", ", round(gas_outer_upper, 2), "]\n\n")

# Identify outliers in gas data
gas_mild <- gas_data[gas_data < gas_inner_lower | gas_data > gas_inner_upper]
gas_extreme <- gas_data[gas_data < gas_outer_lower | gas_data > gas_outer_upper]

cat("OUTLIER IDENTIFICATION:\n")
cat("======================\n")
cat("Mild outliers (beyond inner fences):", paste(sort(gas_mild), collapse = ", "), "\n")
cat("Extreme outliers (beyond outer fences):", paste(sort(gas_extreme), collapse = ", "), "\n\n")

# Three causes of outliers explanation
cat("THREE MAIN CAUSES OF OUTLIERS:\n")
cat("==============================\n")
cat("1. MEASUREMENT ERROR:\n")
cat("   - Faulty instruments, recording mistakes, data entry errors\n")
cat("   - Example: A 300 mpg reading due to decimal point error\n\n")

cat("2. DIFFERENT POPULATION:\n")
cat("   - Observation from a different group than intended\n")
cat("   - Example: Hybrid car mixed in with conventional cars\n\n")

cat("3. RARE EVENT:\n")
cat("   - Legitimate but unusual observation from the same population\n")
cat("   - Example: Exceptional driving conditions or vehicle performance\n\n")

# Create basic visualization
png("output/outlier_detection_basics.png", width = 1200, height = 800, res = 120)
par(mfrow = c(2, 2))

# Simple data box plot
boxplot(simple_data, main = "Simple Data Box Plot", ylab = "Value", col = "lightblue")
abline(h = c(inner_lower, inner_upper), col = "orange", lty = 2, lwd = 2)
abline(h = c(outer_lower, outer_upper), col = "red", lty = 3, lwd = 2)
legend("topright", c("Inner Fences", "Outer Fences"), 
       col = c("orange", "red"), lty = c(2, 3), cex = 0.8)

# Gas data box plot
boxplot(gas_data, main = "Gas Mileage Box Plot", ylab = "MPG", col = "lightgreen")
abline(h = c(gas_inner_lower, gas_inner_upper), col = "orange", lty = 2, lwd = 2)
abline(h = c(gas_outer_lower, gas_outer_upper), col = "red", lty = 3, lwd = 2)

# Histogram with outliers highlighted
hist(gas_data, main = "Gas Mileage Distribution", xlab = "MPG", 
     col = "lightcoral", breaks = 15)
abline(v = gas_mild, col = "red", lwd = 3)

# Dot plot showing outliers
stripchart(gas_data, main = "Gas Mileage Dot Plot", xlab = "MPG", 
           method = "stack", pch = 19, col = "blue")
points(gas_mild, rep(1, length(gas_mild)), col = "red", pch = 19, cex = 2)
abline(v = c(gas_inner_lower, gas_inner_upper), col = "orange", lty = 2, lwd = 2)

dev.off()
cat("✓ Basic outlier detection plots saved to output/outlier_detection_basics.png\n")

# Function for complete outlier analysis
outlier_analysis <- function(data, data_name) {
  cat("\n", data_name, " Complete Analysis:\n")
  cat(rep("=", nchar(data_name) + 19), "\n", sep = "")
  
  n <- length(data)
  quartiles <- quantile(data, c(0.25, 0.5, 0.75))
  iqr_val <- IQR(data)
  
  # Fences
  inner_lower <- quartiles[1] - 1.5 * iqr_val
  inner_upper <- quartiles[3] + 1.5 * iqr_val
  outer_lower <- quartiles[1] - 3 * iqr_val
  outer_upper <- quartiles[3] + 3 * iqr_val
  
  # Outliers
  mild <- data[data < inner_lower | data > inner_upper]
  extreme <- data[data < outer_lower | data > outer_upper]
  
  cat("Sample size:", n, "\n")
  cat("Q1:", round(quartiles[1], 3), ", Q3:", round(quartiles[3], 3), "\n")
  cat("IQR:", round(iqr_val, 3), "\n")
  cat("Inner fences: [", round(inner_lower, 2), ", ", round(inner_upper, 2), "]\n")
  cat("Outer fences: [", round(outer_lower, 2), ", ", round(outer_upper, 2), "]\n")
  cat("Mild outliers:", ifelse(length(mild) > 0, paste(sort(mild), collapse = ", "), "None"), "\n")
  cat("Extreme outliers:", ifelse(length(extreme) > 0, paste(sort(extreme), collapse = ", "), "None"), "\n")
  
  return(list(mild = mild, extreme = extreme, fences = c(inner_lower, inner_upper, outer_lower, outer_upper)))
}

# Apply to both datasets
simple_results <- outlier_analysis(simple_data, "Simple Data")
gas_results <- outlier_analysis(gas_data, "Gas Mileage Data")

# Save summary
sink("output/outlier_detection_summary.txt")
cat("OUTLIER DETECTION METHODS SUMMARY\n")
cat("=================================\n\n")
cat("BOX PLOT METHOD:\n")
cat("1. Calculate Q1, Q3, and IQR\n")
cat("2. Inner fences: Q1 ± 1.5×IQR, Q3 ± 1.5×IQR\n")
cat("3. Outer fences: Q1 ± 3×IQR, Q3 ± 3×IQR\n")
cat("4. Mild outliers: Beyond inner fences\n")
cat("5. Extreme outliers: Beyond outer fences\n\n")
cat("ADVANTAGES:\n")
cat("• Visual method\n")
cat("• Robust to existing outliers\n")
cat("• Based on quartiles (not affected by extreme values)\n\n")
cat("THREE CAUSES OF OUTLIERS:\n")
cat("1. Measurement error (investigate and possibly remove)\n")
cat("2. Different population (investigate context)\n")
cat("3. Rare event (legitimate, keep in analysis)\n")
sink()

cat("\n✓ Summary saved to output/outlier_detection_summary.txt\n")
cat("Box plot method complete - ready for method comparison!\n")
