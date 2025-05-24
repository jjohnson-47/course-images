# Comparing Relative Standing Across Different Distributions
# Same raw score, different meanings

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Comparing Relative Standing Across Distributions\n")
cat("===============================================\n\n")

# Two different classes with different performance levels
class_A <- c(65, 68, 72, 75, 78, 80, 82, 85, 87, 90, 92, 94, 96, 98, 99)  # High-performing
class_B <- c(45, 52, 58, 63, 67, 71, 75, 78, 82, 85, 89, 92, 96, 88, 75)  # Mixed performance

cat("Scenario: Same raw score in two different classes\n")
cat("================================================\n")
cat("Class A (High-performing):", paste(sort(class_A), collapse = ", "), "\n")
cat("Class B (Mixed performance):", paste(sort(class_B), collapse = ", "), "\n\n")

# Function for complete relative standing analysis
analyze_relative_standing <- function(data, score, class_name) {
  mean_val <- mean(data)
  sd_val <- sd(data)
  z_score <- (score - mean_val) / sd_val
  percentile <- sum(data <= score) / length(data) * 100
  quartiles <- quantile(data, probs = c(0.25, 0.5, 0.75))
  
  # Determine quartile
  if(score <= quartiles[1]) {
    quartile_pos <- "Q1 (bottom 25%)"
  } else if(score <= quartiles[2]) {
    quartile_pos <- "Q2 (25th-50th percentile)"
  } else if(score <= quartiles[3]) {
    quartile_pos <- "Q3 (50th-75th percentile)"
  } else {
    quartile_pos <- "Q4 (top 25%)"
  }
  
  cat(class_name, "Analysis:\n")
  cat(rep("=", nchar(class_name) + 10), "\n", sep = "")
  cat("Class mean:", round(mean_val, 1), "\n")
  cat("Class SD:", round(sd_val, 1), "\n")
  cat("Score of", score, ":\n")
  cat("  Z-score:", round(z_score, 2), "\n")
  cat("  Percentile rank:", round(percentile, 1), "%\n")
  cat("  Quartile position:", quartile_pos, "\n")
  cat("  Interpretation:", ifelse(z_score > 1, "Above average", 
                                 ifelse(z_score > 0, "Slightly above average",
                                       ifelse(z_score > -1, "Around average", "Below average"))), "\n\n")
  
  return(list(mean = mean_val, sd = sd_val, z = z_score, percentile = percentile))
}

# Analyze the same score (85) in both classes
target_score <- 85
cat("Analyzing a score of", target_score, "in both classes:\n")
cat("===========================================\n\n")

results_A <- analyze_relative_standing(class_A, target_score, "Class A")
results_B <- analyze_relative_standing(class_B, target_score, "Class B")

cat("KEY INSIGHT:\n")
cat("===========\n")
cat("The SAME raw score (", target_score, ") has very different meanings:\n")
cat("• In Class A: ", round(results_A$percentile, 1), "th percentile (z = ", 
    round(results_A$z, 2), ")\n", sep = "")
cat("• In Class B: ", round(results_B$percentile, 1), "th percentile (z = ", 
    round(results_B$z, 2), ")\n", sep = "")
cat("This demonstrates why RELATIVE STANDING matters!\n\n")

# SAT Score Example
cat("SAT Score Example (Standardized Testing):\n")
cat("========================================\n")
# SAT scores: mean = 500, SD = 100 (traditional scale)
sat_mean <- 500
sat_sd <- 100

cat("SAT Critical Reading Section:\n")
cat("Mean =", sat_mean, ", Standard Deviation =", sat_sd, "\n\n")

sat_scores <- c(400, 500, 600, 650, 750)
cat("SAT Score Interpretations:\n")
for(score in sat_scores) {
  z_sat <- (score - sat_mean) / sat_sd
  # Using normal distribution approximation for percentiles
  percentile_sat <- pnorm(z_sat) * 100
  
  cat("Score", score, ":\n")
  cat("  Z-score:", round(z_sat, 2), "\n")
  cat("  Approximate percentile:", round(percentile_sat, 1), "%\n")
  cat("  Interpretation:", 
      ifelse(z_sat >= 2, "Excellent (top 2.5%)",
             ifelse(z_sat >= 1, "Above average (top 16%)", 
                    ifelse(z_sat >= 0, "Average or above",
                           ifelse(z_sat >= -1, "Below average", "Well below average")))), "\n\n")
}

# Create visualization
png("output/relative_standing_comparison.png", width = 1400, height = 1000, res = 120)
par(mfrow = c(3, 2))

# Class A vs Class B histograms
hist(class_A, main = "Class A Distribution", xlab = "Score", 
     col = "lightblue", breaks = 8, xlim = c(40, 100))
abline(v = target_score, col = "red", lwd = 3)
abline(v = mean(class_A), col = "blue", lwd = 2, lty = 2)
legend("topleft", c("Target Score", "Mean"), col = c("red", "blue"), lwd = c(3, 2))

hist(class_B, main = "Class B Distribution", xlab = "Score", 
     col = "lightgreen", breaks = 8, xlim = c(40, 100))
abline(v = target_score, col = "red", lwd = 3)
abline(v = mean(class_B), col = "blue", lwd = 2, lty = 2)

# Box plots comparison
boxplot(list("Class A" = class_A, "Class B" = class_B), 
        main = "Class Comparison", ylab = "Score", col = c("lightblue", "lightgreen"))
abline(h = target_score, col = "red", lwd = 2, lty = 2)

# Z-score comparison
z_A <- (class_A - mean(class_A)) / sd(class_A)
z_B <- (class_B - mean(class_B)) / sd(class_B)

plot(1:length(z_A), sort(z_A), type = "h", col = "blue", lwd = 2,
     main = "Z-Scores: Class A", xlab = "Student Rank", ylab = "Z-Score",
     ylim = c(-3, 3))
abline(h = 0, col = "black", lwd = 1)
abline(h = c(-2, -1, 1, 2), col = "gray", lty = 2)

plot(1:length(z_B), sort(z_B), type = "h", col = "green", lwd = 2,
     main = "Z-Scores: Class B", xlab = "Student Rank", ylab = "Z-Score",
     ylim = c(-3, 3))
abline(h = 0, col = "black", lwd = 1)
abline(h = c(-2, -1, 1, 2), col = "gray", lty = 2)

# SAT distribution
x_sat <- seq(200, 800, by = 10)
y_sat <- dnorm(x_sat, sat_mean, sat_sd)

plot(x_sat, y_sat, type = "l", lwd = 2, col = "purple",
     main = "SAT Score Distribution", xlab = "SAT Score", ylab = "Density")
abline(v = sat_mean, col = "red", lwd = 2)
abline(v = sat_mean + c(-1, 1) * sat_sd, col = "orange", lwd = 2, lty = 2)
abline(v = sat_mean + c(-2, 2) * sat_sd, col = "blue", lwd = 2, lty = 3)
legend("topright", c("Mean", "±1 SD", "±2 SD"), 
       col = c("red", "orange", "blue"), lty = c(1, 2, 3))

dev.off()
cat("✓ Comparison plots saved to output/relative_standing_comparison.png\n")

# Create comparison table - FIXED VERSION
comparison_data <- data.frame(
  Dataset = c("Class A", "Class B", "SAT Scores"),
  Mean = round(c(mean(class_A), mean(class_B), sat_mean), 1),
  SD = round(c(sd(class_A), sd(class_B), sat_sd), 1),
  Score_85_Z = round(c((85 - mean(class_A))/sd(class_A), 
                       (85 - mean(class_B))/sd(class_B), 
                       (85 - sat_mean)/sat_sd), 2),
  Score_85_Percentile = round(c(sum(class_A <= 85)/length(class_A)*100,
                                sum(class_B <= 85)/length(class_B)*100,
                                pnorm((85 - sat_mean)/sat_sd)*100), 1)
)

cat("\nCOMPARISON TABLE:\n")
print(comparison_data)

# Save comparison report
sink("output/distribution_comparison_report.txt")
cat("RELATIVE STANDING COMPARISON REPORT\n")
cat("===================================\n\n")
cat("KEY FINDING: Same raw score = Different relative standing\n\n")
print(comparison_data)
cat("\nIMPORTANT LESSON:\n")
cat("Raw scores by themselves are meaningless without context!\n")
cat("Always consider the distribution when interpreting scores.\n")
sink()

cat("\n✓ Comparison report saved to output/distribution_comparison_report.txt\n")
