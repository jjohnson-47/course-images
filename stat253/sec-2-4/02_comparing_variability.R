# Comparing Datasets - Same Center, Different Spread
# Demonstrates why we need variability measures

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Comparing Variability in Real Data\n")
cat("==================================\n\n")

# Drug response time example (simulated based on textbook concept)
set.seed(123)
drug_A <- c(2.1, 2.5, 2.8, 3.2, 3.6, 4.0, 4.4, 4.7, 5.1, 5.5)  # More spread
drug_B <- c(3.4, 3.6, 3.7, 3.8, 3.9, 4.0, 4.1, 4.2, 4.3, 4.5)  # Less spread

# Test scores example  
class_A <- c(85, 87, 88, 90, 92)  # Low variability
class_B <- c(70, 80, 88, 95, 99)  # High variability

cat("Example 1: Drug Response Times (seconds)\n")
cat("========================================\n")
cat("Drug A:", paste(drug_A, collapse = ", "), "\n")
cat("Drug B:", paste(drug_B, collapse = ", "), "\n\n")

# Analysis function
analyze_variability <- function(data, name) {
  cat(name, "Statistics:\n")
  cat("Mean:", round(mean(data), 2), "\n")
  cat("Median:", round(median(data), 2), "\n")
  cat("Range:", max(data) - min(data), "\n")
  cat("Variance:", round(var(data), 4), "\n")
  cat("Std Dev:", round(sd(data), 4), "\n\n")
}

analyze_variability(drug_A, "Drug A")
analyze_variability(drug_B, "Drug B")

cat("Notice: Both drugs have similar means but very different variability!\n")
cat("Drug A has", round(sd(drug_A)/sd(drug_B), 1), "times more variability than Drug B\n\n")

cat("Example 2: Test Scores\n")
cat("======================\n")
cat("Class A:", paste(class_A, collapse = ", "), "\n")
cat("Class B:", paste(class_B, collapse = ", "), "\n\n")

analyze_variability(class_A, "Class A")
analyze_variability(class_B, "Class B")

# Create visualization
png("output/variability_comparison.png", width = 1200, height = 800, res = 120)
par(mfrow = c(2, 2))

# Drug comparison
hist(drug_A, main = "Drug A Response Times", xlab = "Seconds", 
     col = "lightblue", xlim = c(1, 6), breaks = 6)
abline(v = mean(drug_A), col = "red", lwd = 2)

hist(drug_B, main = "Drug B Response Times", xlab = "Seconds", 
     col = "lightgreen", xlim = c(1, 6), breaks = 6)
abline(v = mean(drug_B), col = "red", lwd = 2)

# Test scores comparison
stripchart(class_A, main = "Class A Test Scores", xlab = "Score", 
           method = "stack", pch = 19, col = "blue", xlim = c(65, 105))
abline(v = mean(class_A), col = "red", lwd = 2)

stripchart(class_B, main = "Class B Test Scores", xlab = "Score", 
           method = "stack", pch = 19, col = "orange", xlim = c(65, 105))
abline(v = mean(class_B), col = "red", lwd = 2)

dev.off()
cat("✓ Comparison plots saved to output/variability_comparison.png\n")

# Side-by-side dot plots
png("output/spread_comparison.png", width = 1000, height = 600, res = 120)
par(mfrow = c(2, 1))

# Show spread around mean
stripchart(list("Low Variability" = class_A, "High Variability" = class_B),
           main = "Same Mean, Different Spread - Test Scores",
           xlab = "Score", method = "stack", pch = 19,
           col = c("blue", "red"))

stripchart(list("Drug A (More Variable)" = drug_A, "Drug B (Less Variable)" = drug_B),
           main = "Same Mean, Different Spread - Response Times", 
           xlab = "Seconds", method = "stack", pch = 19,
           col = c("purple", "green"))

dev.off()
cat("✓ Spread comparison saved to output/spread_comparison.png\n")

# Summary table
summary_data <- data.frame(
  Dataset = c("Drug A", "Drug B", "Class A", "Class B"),
  Mean = c(mean(drug_A), mean(drug_B), mean(class_A), mean(class_B)),
  StdDev = c(sd(drug_A), sd(drug_B), sd(class_A), sd(class_B)),
  Variability = c("High", "Low", "Low", "High")
)

cat("\nSUMMARY TABLE:\n")
print(round(summary_data[,1:3], 2))

cat("\nKey Insight: Means can be similar while variability differs greatly!\n")
cat("This is why we need BOTH central tendency AND variability measures.\n")
