# Outlier Detection and Data Assessment
# Using standard deviation rules to identify unusual observations

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Outlier Detection Using Standard Deviation Rules\n")
cat("===============================================\n\n")

# EPA mileage data (continuing from previous sections)
epa_data <- c(32.7, 36.3, 37.1, 35.9, 38.2, 41.0, 36.8, 37.9, 39.0, 33.9)

cat("EPA Mileage Data Analysis:\n")
cat("=========================\n")
cat("Data:", paste(sort(epa_data), collapse = ", "), "mpg\n")

mean_epa <- mean(epa_data)
sd_epa <- sd(epa_data)
n_epa <- length(epa_data)

cat("Mean:", round(mean_epa, 2), "mpg\n")
cat("Standard deviation:", round(sd_epa, 2), "mpg\n")
cat("Sample size:", n_epa, "\n\n")

# Function for outlier detection
detect_outliers <- function(data, data_name) {
  mean_val <- mean(data)
  sd_val <- sd(data)
  n <- length(data)
  
  cat(data_name, "Outlier Analysis:\n")
  cat(rep("=", nchar(data_name) + 18), "\n", sep = "")
  
  # Calculate z-scores (how many SDs from mean)
  z_scores <- (data - mean_val) / sd_val
  
  # Create analysis table
  analysis <- data.frame(
    Value = data,
    Z_Score = round(z_scores, 2),
    Distance_from_Mean = round(abs(data - mean_val), 2),
    Classification = ifelse(abs(z_scores) > 3, "Extreme Outlier",
                           ifelse(abs(z_scores) > 2, "Moderate Outlier", "Normal"))
  )
  
  # Sort by absolute z-score for clarity
  analysis <- analysis[order(abs(analysis$Z_Score), decreasing = TRUE), ]
  
  cat("\nDATA CLASSIFICATION:\n")
  print(analysis)
  cat("\n")
  
  # Count by category
  normal_count <- sum(abs(z_scores) <= 2)
  moderate_count <- sum(abs(z_scores) > 2 & abs(z_scores) <= 3)
  extreme_count <- sum(abs(z_scores) > 3)
  
  cat("OUTLIER SUMMARY:\n")
  cat("Normal values (within ±2 SD):", normal_count, "/", n, 
      "(", round(normal_count/n*100, 1), "%)\n")
  cat("Moderate outliers (2-3 SD):", moderate_count, "/", n, 
      "(", round(moderate_count/n*100, 1), "%)\n")
  cat("Extreme outliers (beyond 3 SD):", extreme_count, "/", n, 
      "(", round(extreme_count/n*100, 1), "%)\n\n")
  
  # Identify specific outliers
  if(moderate_count > 0) {
    moderate_outliers <- data[abs(z_scores) > 2 & abs(z_scores) <= 3]
    cat("⚠️  MODERATE OUTLIERS:", paste(moderate_outliers, collapse = ", "), "\n")
  }
  
  if(extreme_count > 0) {
    extreme_outliers <- data[abs(z_scores) > 3]
    cat("🚨 EXTREME OUTLIERS:", paste(extreme_outliers, collapse = ", "), "\n")
  }
  
  if(moderate_count == 0 && extreme_count == 0) {
    cat("✅ No outliers detected - all values within ±2 SD\n")
  }
  
  cat("\n")
  return(analysis)
}

# Analyze EPA data
epa_analysis <- detect_outliers(epa_data, "EPA Mileage")

# Create dataset with clear outliers for demonstration
cat("Demonstration with Outlier Dataset:\n")
cat("==================================\n")
outlier_demo <- c(epa_data, 50.5, 25.1)  # Add clear outliers
cat("Original data + artificial outliers:", paste(sort(outlier_demo), collapse = ", "), "\n\n")

demo_analysis <- detect_outliers(outlier_demo, "Demo with Outliers")

# Student test scores with outliers
test_scores_outliers <- c(78, 82, 85, 88, 91, 94, 76, 89, 93, 87, 45, 99)  # 45 is outlier
cat("Test Scores Example:\n")
test_analysis <- detect_outliers(test_scores_outliers, "Test Scores")

# Assessment of data shape for rule selection
assess_data_shape <- function(data, data_name) {
  cat(data_name, "Shape Assessment:\n")
  cat(rep("=", nchar(data_name) + 18), "\n", sep = "")
  
  mean_val <- mean(data)
  median_val <- median(data)
  
  # Skewness indicator
  skew_diff <- mean_val - median_val
  skew_ratio <- skew_diff / sd(data)
  
  cat("Mean:", round(mean_val, 2), "\n")
  cat("Median:", round(median_val, 2), "\n")
  cat("Mean - Median:", round(skew_diff, 2), "\n")
  cat("Skewness ratio:", round(skew_ratio, 2), "\n")
  
  if(abs(skew_ratio) < 0.2) {
    shape_assessment <- "Approximately symmetric - Empirical Rule likely applies"
  } else if(skew_ratio > 0.2) {
    shape_assessment <- "Right-skewed - Use Chebyshev's Rule"
  } else {
    shape_assessment <- "Left-skewed - Use Chebyshev's Rule"
  }
  
  cat("Shape assessment:", shape_assessment, "\n\n")
  return(shape_assessment)
}

# Assess shape of our datasets
epa_shape <- assess_data_shape(epa_data, "EPA Data")
test_shape <- assess_data_shape(test_scores_outliers, "Test Scores")

# Create comprehensive visualization
png("output/outlier_detection.png", width = 1400, height = 1000, res = 120)
par(mfrow = c(3, 2))

# EPA data with outlier boundaries
hist(epa_data, main = "EPA Data - No Outliers", xlab = "MPG", 
     col = "lightblue", breaks = 6)
abline(v = mean_epa, col = "red", lwd = 2)
abline(v = mean_epa + c(-2, 2) * sd_epa, col = "orange", lwd = 2, lty = 2)
abline(v = mean_epa + c(-3, 3) * sd_epa, col = "purple", lwd = 2, lty = 3)
legend("topright", c("Mean", "±2 SD", "±3 SD"), 
       col = c("red", "orange", "purple"), lty = c(1, 2, 3), cex = 0.8)

# Demo data with outliers
hist(outlier_demo, main = "Demo Data - With Outliers", xlab = "MPG", 
     col = "lightcoral", breaks = 8)
mean_demo <- mean(outlier_demo)
sd_demo <- sd(outlier_demo)
abline(v = mean_demo, col = "red", lwd = 2)
abline(v = mean_demo + c(-2, 2) * sd_demo, col = "orange", lwd = 2, lty = 2)
abline(v = mean_demo + c(-3, 3) * sd_demo, col = "purple", lwd = 2, lty = 3)

# Dot plots with outlier zones
stripchart(epa_data, main = "EPA Data Points", xlab = "MPG", 
           method = "stack", pch = 19, col = "blue")
abline(v = mean_epa + c(-2, 2) * sd_epa, col = "orange", lwd = 2, lty = 2)
abline(v = mean_epa + c(-3, 3) * sd_epa, col = "purple", lwd = 2, lty = 3)

stripchart(outlier_demo, main = "Demo Data Points", xlab = "MPG", 
           method = "stack", pch = 19, col = "red")
abline(v = mean_demo + c(-2, 2) * sd_demo, col = "orange", lwd = 2, lty = 2)
abline(v = mean_demo + c(-3, 3) * sd_demo, col = "purple", lwd = 2, lty = 3)

# Z-score plots
z_epa <- (epa_data - mean_epa) / sd_epa
z_demo <- (outlier_demo - mean_demo) / sd_demo

plot(1:length(z_epa), z_epa, type = "h", lwd = 3, col = "blue",
     main = "EPA Data Z-Scores", xlab = "Observation", ylab = "Z-Score",
     ylim = c(-4, 4))
abline(h = c(-3, -2, 0, 2, 3), col = c("red", "orange", "black", "orange", "red"), 
       lty = c(3, 2, 1, 2, 3))
points(1:length(z_epa), z_epa, pch = 19, col = "blue")

plot(1:length(z_demo), z_demo, type = "h", lwd = 3, col = "red",
     main = "Demo Data Z-Scores", xlab = "Observation", ylab = "Z-Score",
     ylim = c(-4, 4))
abline(h = c(-3, -2, 0, 2, 3), col = c("red", "orange", "black", "orange", "red"), 
       lty = c(3, 2, 1, 2, 3))
points(1:length(z_demo), z_demo, pch = 19, col = "red")
# Highlight extreme outliers
extreme_indices <- which(abs(z_demo) > 3)
if(length(extreme_indices) > 0) {
  points(extreme_indices, z_demo[extreme_indices], pch = 19, col = "black", cex = 2)
}

dev.off()
cat("✓ Outlier detection plots saved to output/outlier_detection.png\n")

# Save comprehensive report
sink("output/outlier_detection_report.txt")
cat("OUTLIER DETECTION REPORT\n")
cat("========================\n\n")
cat("DETECTION RULES:\n")
cat("• Normal values: Within ±2 standard deviations\n")
cat("• Moderate outliers: 2 to 3 standard deviations away\n")
cat("• Extreme outliers: Beyond ±3 standard deviations\n\n")

cat("EPA MILEAGE DATA:\n")
cat("Mean ±2 SD: [", round(mean_epa - 2*sd_epa, 1), ", ", round(mean_epa + 2*sd_epa, 1), "] mpg\n")
cat("Mean ±3 SD: [", round(mean_epa - 3*sd_epa, 1), ", ", round(mean_epa + 3*sd_epa, 1), "] mpg\n")
cat("Result: No outliers detected in EPA data\n\n")

cat("PRACTICAL APPLICATIONS:\n")
cat("• Quality control: Flag products outside normal ranges\n")
cat("• Data cleaning: Identify potential measurement errors\n")
cat("• Research: Detect unusual subjects or responses\n")
cat("• Grading: Identify exceptional performance (high or low)\n\n")

cat("RULE SELECTION:\n")
cat("• Symmetric data: Use Empirical Rule (68-95-99.7)\n")
cat("• Skewed data: Use Chebyshev's Rule (safer, broader)\n")
cat("• Unknown distribution: Always use Chebyshev's Rule\n")
sink()

cat("✓ Outlier detection report saved to output/outlier_detection_report.txt\n")
cat("\nKey takeaway: Standard deviation rules provide systematic outlier detection!\n")
cat("Remember: ±2 SD catches ~95%, ±3 SD catches ~99.7% of normal data.\n")
