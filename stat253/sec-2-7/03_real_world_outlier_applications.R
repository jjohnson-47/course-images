# Real-World Outlier Detection Applications
# Complete workflow and practical interpretation

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Real-World Outlier Detection Applications\n")
cat("========================================\n\n")

# 1. Quality Control Example
cat("1. MANUFACTURING QUALITY CONTROL\n")
cat("================================\n")

set.seed(789)
# Widget weights: target = 100g, normal process variation
normal_weights <- rnorm(95, 100, 2)
# Add some problematic measurements
widget_weights <- c(normal_weights, 85, 87, 115, 118, 92)  # Mixed outliers

cat("Widget manufacturing data (n =", length(widget_weights), ")\n")
cat("Target weight: 100g\n\n")

# Complete outlier analysis function
complete_outlier_analysis <- function(data, context_name) {
  cat(context_name, "Outlier Analysis:\n")
  cat(rep("=", nchar(context_name) + 18), "\n", sep = "")
  
  # Basic statistics
  n <- length(data)
  mean_val <- mean(data)
  sd_val <- sd(data)
  
  # Box plot method
  quartiles <- quantile(data, c(0.25, 0.5, 0.75))
  iqr_val <- IQR(data)
  inner_fences <- c(quartiles[1] - 1.5 * iqr_val, quartiles[3] + 1.5 * iqr_val)
  outer_fences <- c(quartiles[1] - 3 * iqr_val, quartiles[3] + 3 * iqr_val)
  
  mild_outliers <- data[data < inner_fences[1] | data > inner_fences[2]]
  extreme_outliers <- data[data < outer_fences[1] | data > outer_fences[2]]
  
  # Z-score method
  z_scores <- abs((data - mean_val) / sd_val)
  possible_outliers <- data[z_scores > 2]
  z_outliers <- data[z_scores > 3]
  
  cat("Sample size:", n, "\n")
  cat("Mean:", round(mean_val, 2), ", SD:", round(sd_val, 2), "\n")
  cat("Q1:", round(quartiles[1], 2), ", Median:", round(quartiles[2], 2), 
      ", Q3:", round(quartiles[3], 2), "\n\n")
  
  cat("BOX PLOT METHOD:\n")
  cat("Inner fences: [", round(inner_fences[1], 2), ", ", round(inner_fences[2], 2), "]\n")
  cat("Mild outliers:", ifelse(length(mild_outliers) > 0, 
                              paste(round(sort(mild_outliers), 1), collapse = ", "), "None"), "\n")
  cat("Extreme outliers:", ifelse(length(extreme_outliers) > 0, 
                                 paste(round(sort(extreme_outliers), 1), collapse = ", "), "None"), "\n\n")
  
  cat("Z-SCORE METHOD:\n")
  cat("Possible outliers (|z| > 2):", ifelse(length(possible_outliers) > 0, 
                                            paste(round(sort(possible_outliers), 1), collapse = ", "), "None"), "\n")
  cat("Outliers (|z| > 3):", ifelse(length(z_outliers) > 0, 
                                   paste(round(sort(z_outliers), 1), collapse = ", "), "None"), "\n\n")
  
  return(list(
    mild = mild_outliers, extreme = extreme_outliers,
    possible = possible_outliers, z_outliers = z_outliers,
    fences = c(inner_fences, outer_fences),
    stats = c(mean_val, sd_val, quartiles)
  ))
}

# Analyze widget weights
widget_results <- complete_outlier_analysis(widget_weights, "Widget Manufacturing")

# Quality control interpretation
cat("QUALITY CONTROL INTERPRETATION:\n")
cat("===============================\n")
outlier_values <- unique(c(widget_results$mild, widget_results$possible))
for(weight in sort(outlier_values)) {
  cat("Weight", weight, "g:")
  
  if(weight < 90) {
    cat(" CRITICAL - Underweight, check calibration\n")
  } else if(weight < 95) {
    cat(" WARNING - Below specification, investigate\n")
  } else if(weight > 110) {
    cat(" CRITICAL - Overweight, check process\n")
  } else if(weight > 105) {
    cat(" WARNING - Above specification, monitor\n")
  } else {
    cat(" MINOR - Within acceptable range\n")
  }
}
cat("\n")

# 2. Medical/Health Data Example
cat("2. BLOOD PRESSURE MONITORING\n")
cat("============================\n")

# Systolic blood pressure readings
set.seed(456)
normal_bp <- rnorm(45, 120, 10)  # Normal range around 120
bp_readings <- c(normal_bp, 180, 185, 85, 90, 165)  # Add some concerning values

cat("Systolic blood pressure readings (n =", length(bp_readings), ")\n")
cat("Normal range: 90-120 mmHg\n\n")

bp_results <- complete_outlier_analysis(bp_readings, "Blood Pressure")

# Medical interpretation
cat("MEDICAL INTERPRETATION:\n")
cat("======================\n")
bp_outliers <- unique(c(bp_results$mild, bp_results$possible))
for(bp in sort(bp_outliers)) {
  cat("BP", round(bp), "mmHg:")
  
  if(bp >= 180) {
    cat(" HYPERTENSIVE CRISIS - Immediate attention needed\n")
  } else if(bp >= 160) {
    cat(" STAGE 2 HYPERTENSION - Medication indicated\n")
  } else if(bp >= 140) {
    cat(" STAGE 1 HYPERTENSION - Lifestyle changes\n")
  } else if(bp <= 90) {
    cat(" HYPOTENSION - Monitor for symptoms\n")
  } else {
    cat(" ELEVATED - Recheck and monitor\n")
  }
}
cat("\n")

# 3. Financial Data Example
cat("3. INVESTMENT RETURNS ANALYSIS\n")
cat("==============================\n")

# Monthly returns (as percentages)
set.seed(321)
normal_returns <- rnorm(47, 1.2, 3.5)  # Average 1.2% monthly return
returns <- c(normal_returns, -15, -18, 22, 25, -12)  # Add some extreme months

cat("Monthly investment returns (n =", length(returns), ")\n")
cat("Expected monthly return: ~1.2%\n\n")

returns_results <- complete_outlier_analysis(returns, "Investment Returns")

# Financial interpretation
cat("FINANCIAL INTERPRETATION:\n")
cat("========================\n")
return_outliers <- unique(c(returns_results$mild, returns_results$possible))
for(ret in sort(return_outliers)) {
  cat("Return", round(ret, 1), "%:")
  
  if(ret <= -15) {
    cat(" MAJOR LOSS - Review portfolio, market event?\n")
  } else if(ret <= -10) {
    cat(" SIGNIFICANT LOSS - Investigate causes\n")
  } else if(ret >= 20) {
    cat(" EXCEPTIONAL GAIN - Unsustainable, consider rebalancing\n")
  } else if(ret >= 15) {
    cat(" STRONG PERFORMANCE - Monitor for volatility\n")
  } else {
    cat(" MODERATE OUTLIER - Normal market variation\n")
  }
}
cat("\n")

# Create comprehensive visualization
png("output/real_world_outlier_applications.png", width = 1600, height = 1200, res = 120)
par(mfrow = c(3, 3))

# Widget manufacturing analysis
boxplot(widget_weights, main = "Widget Weights", ylab = "Weight (g)", col = "lightblue")
abline(h = c(95, 105), col = "orange", lty = 2, lwd = 2)  # Specification limits
points(rep(1, length(widget_results$mild)), widget_results$mild, col = "red", pch = 19, cex = 1.5)

hist(widget_weights, main = "Widget Weight Distribution", xlab = "Weight (g)", 
     col = "lightblue", breaks = 12)
abline(v = 100, col = "green", lwd = 2)  # Target
abline(v = c(95, 105), col = "orange", lty = 2, lwd = 2)  # Specs

stripchart(widget_weights, main = "Widget Weights - Outlier Detection", 
           xlab = "Weight (g)", method = "jitter", pch = 19, col = "blue")
points(widget_results$mild, rep(1, length(widget_results$mild)), col = "red", pch = 19, cex = 2)

# Blood pressure analysis
boxplot(bp_readings, main = "Blood Pressure Readings", ylab = "Systolic BP (mmHg)", col = "lightcoral")
abline(h = c(90, 120, 140, 180), col = c("blue", "green", "orange", "red"), lwd = 2)
points(rep(1, length(bp_results$mild)), bp_results$mild, col = "red", pch = 19, cex = 1.5)

hist(bp_readings, main = "BP Distribution", xlab = "Systolic BP (mmHg)", 
     col = "lightcoral", breaks = 12)
abline(v = c(90, 120, 140, 180), col = c("blue", "green", "orange", "red"), lwd = 2)

stripchart(bp_readings, main = "BP - Outlier Detection", 
           xlab = "Systolic BP (mmHg)", method = "jitter", pch = 19, col = "red")
points(bp_results$mild, rep(1, length(bp_results$mild)), col = "darkred", pch = 19, cex = 2)

# Investment returns analysis
boxplot(returns, main = "Investment Returns", ylab = "Monthly Return (%)", col = "lightgreen")
abline(h = c(-10, 0, 10), col = c("red", "black", "green"), lwd = 2)
points(rep(1, length(returns_results$mild)), returns_results$mild, col = "red", pch = 19, cex = 1.5)

hist(returns, main = "Returns Distribution", xlab = "Monthly Return (%)", 
     col = "lightgreen", breaks = 12)
abline(v = 1.2, col = "blue", lwd = 2)  # Expected return

stripchart(returns, main = "Returns - Outlier Detection", 
           xlab = "Monthly Return (%)", method = "jitter", pch = 19, col = "green")
points(returns_results$mild, rep(1, length(returns_results$mild)), col = "darkgreen", pch = 19, cex = 2)

dev.off()
cat("✓ Real-world applications plot saved to output/real_world_outlier_applications.png\n")

# Decision framework for outlier treatment
cat("OUTLIER TREATMENT DECISION FRAMEWORK:\n")
cat("====================================\n\n")

cat("STEP 1: IDENTIFY\n")
cat("• Use both box plot and z-score methods\n")
cat("• Document all potential outliers\n\n")

cat("STEP 2: INVESTIGATE\n")
cat("• Check for measurement errors\n")
cat("• Verify data entry accuracy\n")
cat("• Consider context and domain knowledge\n\n")

cat("STEP 3: CLASSIFY\n")
cat("• Measurement error → Correct or remove\n")
cat("• Different population → Separate analysis\n")
cat("• Rare event → Keep and note\n\n")

cat("STEP 4: DOCUMENT\n")
cat("• Record decision rationale\n")
cat("• Report impact on results\n")
cat("• Consider sensitivity analysis\n\n")

# Create action matrix
action_matrix <- data.frame(
  Outlier_Type = c("Measurement Error", "Different Population", "Rare Event"),
  Box_Plot_Action = c("Investigate & Remove", "Separate Analysis", "Keep & Note"),
  Z_Score_Action = c("Investigate & Remove", "Separate Analysis", "Keep & Note"),
  Follow_Up = c("Check instruments", "Review sampling", "Document context")
)

cat("OUTLIER ACTION MATRIX:\n")
print(action_matrix)

# Save comprehensive report
sink("output/outlier_applications_report.txt")
cat("REAL-WORLD OUTLIER DETECTION REPORT\n")
cat("===================================\n\n")

cat("1. WIDGET MANUFACTURING:\n")
cat("Outliers detected:", paste(round(sort(unique(c(widget_results$mild, widget_results$possible))), 1), collapse = ", "), "\n")
cat("Recommendation: Investigate process control for extreme values\n\n")

cat("2. BLOOD PRESSURE MONITORING:\n")
cat("Outliers detected:", paste(round(sort(unique(c(bp_results$mild, bp_results$possible))), 1), collapse = ", "), "\n")
cat("Recommendation: Follow up on extreme readings, consider medical intervention\n\n")

cat("3. INVESTMENT RETURNS:\n")
cat("Outliers detected:", paste(round(sort(unique(c(returns_results$mild, returns_results$possible))), 1), collapse = ", "), "\n")
cat("Recommendation: Analyze market conditions during extreme periods\n\n")

cat("GENERAL RECOMMENDATIONS:\n")
cat("• Always investigate outliers before removing\n")
cat("• Use domain knowledge in interpretation\n")
cat("• Document all decisions about outlier treatment\n")
cat("• Consider both statistical and practical significance\n")
sink()

cat("✓ Applications report saved to output/outlier_applications_report.txt\n")
cat("\nKey takeaway: Outlier detection is just the first step - interpretation and action depend on context!\n")
cat("Statistical methods identify unusual values, but domain expertise determines what to do about them.\n")
