# Outlier Detection Method Comparison
# Box Plot Method vs Z-Score Method

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Outlier Detection Method Comparison\n")
cat("==================================\n\n")

# Use the same gas mileage data for comparison
set.seed(123)
gas_base <- rnorm(98, 37, 1.2)
gas_data <- c(gas_base, 30.0, 44.9)  # Include known outliers

cat("Dataset: Gas Mileage Data (n =", length(gas_data), ")\n")
cat("Known outliers added: 30.0 and 44.9 mpg\n\n")

# Function for box plot outlier detection
boxplot_outliers <- function(data) {
  quartiles <- quantile(data, c(0.25, 0.5, 0.75))
  iqr_val <- IQR(data)
  
  # Calculate fences
  inner_lower <- quartiles[1] - 1.5 * iqr_val
  inner_upper <- quartiles[3] + 1.5 * iqr_val
  outer_lower <- quartiles[1] - 3 * iqr_val
  outer_upper <- quartiles[3] + 3 * iqr_val
  
  # Identify outliers
  mild_outliers <- data[data < inner_lower | data > inner_upper]
  extreme_outliers <- data[data < outer_lower | data > outer_upper]
  
  return(list(
    quartiles = quartiles,
    iqr = iqr_val,
    fences = c(inner_lower, inner_upper, outer_lower, outer_upper),
    mild = mild_outliers,
    extreme = extreme_outliers
  ))
}

# Function for z-score outlier detection
zscore_outliers <- function(data) {
  mean_val <- mean(data)
  sd_val <- sd(data)
  z_scores <- abs((data - mean_val) / sd_val)
  
  # Identify outliers using different thresholds
  possible_outliers <- data[z_scores > 2]    # |z| > 2
  outliers <- data[z_scores > 3]             # |z| > 3
  
  return(list(
    mean = mean_val,
    sd = sd_val,
    z_scores = z_scores,
    possible = possible_outliers,
    outliers = outliers
  ))
}

# Apply both methods
cat("METHOD 1: BOX PLOT APPROACH\n")
cat("===========================\n")
box_results <- boxplot_outliers(gas_data)

cat("Quartiles: Q1 =", round(box_results$quartiles[1], 3), 
    ", Q2 =", round(box_results$quartiles[2], 3),
    ", Q3 =", round(box_results$quartiles[3], 3), "\n")
cat("IQR =", round(box_results$iqr, 3), "\n")
cat("Inner fences: [", round(box_results$fences[1], 2), ", ", 
    round(box_results$fences[2], 2), "]\n")
cat("Outer fences: [", round(box_results$fences[3], 2), ", ", 
    round(box_results$fences[4], 2), "]\n")
cat("Mild outliers:", ifelse(length(box_results$mild) > 0, 
                            paste(sort(box_results$mild), collapse = ", "), "None"), "\n")
cat("Extreme outliers:", ifelse(length(box_results$extreme) > 0, 
                               paste(sort(box_results$extreme), collapse = ", "), "None"), "\n\n")

cat("METHOD 2: Z-SCORE APPROACH\n")
cat("==========================\n")
z_results <- zscore_outliers(gas_data)

cat("Mean =", round(z_results$mean, 3), "\n")
cat("Standard deviation =", round(z_results$sd, 3), "\n")
cat("Possible outliers (|z| > 2):", ifelse(length(z_results$possible) > 0, 
                                           paste(sort(z_results$possible), collapse = ", "), "None"), "\n")
cat("Outliers (|z| > 3):", ifelse(length(z_results$outliers) > 0, 
                                 paste(sort(z_results$outliers), collapse = ", "), "None"), "\n\n")

# Detailed comparison for specific values
cat("DETAILED COMPARISON:\n")
cat("===================\n")
test_values <- c(30.0, 44.9, 35.0, 40.0)

for(val in test_values) {
  cat("Value:", val, "\n")
  
  # Box plot method
  is_mild <- val < box_results$fences[1] | val > box_results$fences[2]
  is_extreme <- val < box_results$fences[3] | val > box_results$fences[4]
  
  if(is_extreme) {
    box_classification <- "Extreme outlier"
  } else if(is_mild) {
    box_classification <- "Mild outlier"
  } else {
    box_classification <- "Normal"
  }
  
  # Z-score method
  z_score <- abs((val - z_results$mean) / z_results$sd)
  
  if(z_score > 3) {
    z_classification <- "Outlier (|z| > 3)"
  } else if(z_score > 2) {
    z_classification <- "Possible outlier (|z| > 2)"
  } else {
    z_classification <- "Normal"
  }
  
  cat("  Box plot method:", box_classification, "\n")
  cat("  Z-score method:", z_classification, "(z =", round(z_score, 2), ")\n")
  cat("  Agreement:", ifelse(
    (box_classification != "Normal" && z_classification != "Normal") ||
    (box_classification == "Normal" && z_classification == "Normal"), 
    "Yes", "No"), "\n\n")
}

# Method comparison summary
cat("WHEN TO USE EACH METHOD:\n")
cat("=======================\n")
cat("BOX PLOT METHOD:\n")
cat("✓ Visual and intuitive\n")
cat("✓ Robust to outliers (uses quartiles)\n")
cat("✓ Good for skewed distributions\n")
cat("✓ Doesn't assume normal distribution\n")
cat("✗ Less precise than z-scores\n\n")

cat("Z-SCORE METHOD:\n")
cat("✓ Precise numerical criteria\n")
cat("✓ Works well with normal distributions\n")
cat("✓ Easy to automate\n")
cat("✗ Sensitive to existing outliers (uses mean/SD)\n")
cat("✗ Assumes approximately normal distribution\n\n")

# Create comprehensive visualization
png("output/method_comparison.png", width = 1400, height = 1000, res = 120)
par(mfrow = c(3, 2))

# Box plot with annotations
boxplot(gas_data, main = "Box Plot Method", ylab = "MPG", col = "lightblue")
abline(h = box_results$fences[1:2], col = "orange", lty = 2, lwd = 2)
abline(h = box_results$fences[3:4], col = "red", lty = 3, lwd = 2)
points(rep(1, length(box_results$mild)), box_results$mild, col = "red", pch = 19, cex = 1.5)
legend("topright", c("Inner Fences", "Outer Fences", "Outliers"), 
       col = c("orange", "red", "red"), lty = c(2, 3, NA), pch = c(NA, NA, 19))

# Histogram with z-score regions
hist(gas_data, main = "Z-Score Method", xlab = "MPG", col = "lightgreen", breaks = 15)
abline(v = z_results$mean, col = "blue", lwd = 2)
abline(v = z_results$mean + c(-2, 2) * z_results$sd, col = "orange", lty = 2, lwd = 2)
abline(v = z_results$mean + c(-3, 3) * z_results$sd, col = "red", lty = 3, lwd = 2)
points(z_results$possible, rep(0, length(z_results$possible)), col = "red", pch = 19, cex = 1.5)

# Side-by-side dot plots
stripchart(gas_data, main = "Box Plot Outliers", xlab = "MPG", 
           method = "jitter", pch = 19, col = "blue")
points(box_results$mild, rep(1, length(box_results$mild)), col = "red", pch = 19, cex = 2)
abline(v = box_results$fences[1:2], col = "orange", lty = 2, lwd = 2)

stripchart(gas_data, main = "Z-Score Outliers", xlab = "MPG", 
           method = "jitter", pch = 19, col = "blue")
points(z_results$possible, rep(1, length(z_results$possible)), col = "red", pch = 19, cex = 2)
abline(v = z_results$mean + c(-2, 2) * z_results$sd, col = "orange", lty = 2, lwd = 2)

# Comparison table plot
plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10),
     main = "Method Comparison Summary", xlab = "", ylab = "", 
     xaxt = "n", yaxt = "n")

text(5, 9, "Outlier Detection Comparison", cex = 1.4, font = 2)
text(2.5, 7.5, "Box Plot Method", cex = 1.2, font = 2, col = "blue")
text(7.5, 7.5, "Z-Score Method", cex = 1.2, font = 2, col = "red")

text(2.5, 6.5, "Uses: Quartiles & IQR", cex = 1)
text(7.5, 6.5, "Uses: Mean & SD", cex = 1)

text(2.5, 5.5, "Robust to outliers", cex = 1)
text(7.5, 5.5, "Sensitive to outliers", cex = 1)

text(2.5, 4.5, "Visual approach", cex = 1)
text(7.5, 4.5, "Numerical approach", cex = 1)

text(2.5, 3.5, "Any distribution", cex = 1)
text(7.5, 3.5, "Normal preferred", cex = 1)

# Agreement analysis
agreement_data <- c(length(intersect(box_results$mild, z_results$possible)),
                   length(setdiff(box_results$mild, z_results$possible)),
                   length(setdiff(z_results$possible, box_results$mild)))

barplot(agreement_data, names.arg = c("Both Methods", "Box Plot Only", "Z-Score Only"),
        main = "Method Agreement on Outliers", col = c("green", "blue", "red"),
        ylab = "Number of Outliers")

dev.off()
cat("✓ Method comparison plots saved to output/method_comparison.png\n")

# Create detailed comparison table
comparison_table <- data.frame(
  Value = sort(unique(c(box_results$mild, z_results$possible))),
  stringsAsFactors = FALSE
)

# Add classifications for each value
comparison_table$Box_Plot <- sapply(comparison_table$Value, function(x) {
  if(x %in% box_results$extreme) return("Extreme")
  if(x %in% box_results$mild) return("Mild")
  return("Normal")
})

comparison_table$Z_Score <- sapply(comparison_table$Value, function(x) {
  z <- abs((x - z_results$mean) / z_results$sd)
  if(z > 3) return("Outlier")
  if(z > 2) return("Possible")
  return("Normal")
})

comparison_table$Z_Value <- round(sapply(comparison_table$Value, function(x) {
  abs((x - z_results$mean) / z_results$sd)
}), 2)

cat("\nDETAILED COMPARISON TABLE:\n")
print(comparison_table)

# Save comparison report
sink("output/method_comparison_report.txt")
cat("OUTLIER DETECTION METHOD COMPARISON\n")
cat("===================================\n\n")
cat("DATASET: Gas Mileage (n = ", length(gas_data), ")\n\n")
cat("BOX PLOT METHOD RESULTS:\n")
cat("Mild outliers:", ifelse(length(box_results$mild) > 0, 
                            paste(sort(box_results$mild), collapse = ", "), "None"), "\n")
cat("Extreme outliers:", ifelse(length(box_results$extreme) > 0, 
                               paste(sort(box_results$extreme), collapse = ", "), "None"), "\n\n")
cat("Z-SCORE METHOD RESULTS:\n")
cat("Possible outliers (|z| > 2):", ifelse(length(z_results$possible) > 0, 
                                           paste(sort(z_results$possible), collapse = ", "), "None"), "\n")
cat("Outliers (|z| > 3):", ifelse(length(z_results$outliers) > 0, 
                                 paste(sort(z_results$outliers), collapse = ", "), "None"), "\n\n")
cat("AGREEMENT:\n")
both_methods <- intersect(box_results$mild, z_results$possible)
cat("Identified by both methods:", ifelse(length(both_methods) > 0, 
                                         paste(sort(both_methods), collapse = ", "), "None"), "\n")
cat("\nRECOMMENDATION:\n")
cat("Use box plot method for exploratory analysis and robust detection.\n")
cat("Use z-score method for precise numerical criteria with normal data.\n")
sink()

cat("✓ Comparison report saved to output/method_comparison_report.txt\n")
cat("Method comparison complete!\n")
