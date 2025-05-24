# Test Score Relative Standing Visualization
# Comprehensive plot showing percentiles, quartiles, and z-scores

# Data from the example
test_scores <- c(68, 72, 74, 75, 76, 77, 78, 79, 81, 82, 83, 84, 85, 
                 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97)
analyzed_score <- 87

# Calculate statistics
mean_score <- mean(test_scores)
sd_score <- sd(test_scores)
quartiles <- quantile(test_scores, probs = c(0.25, 0.5, 0.75))
z_score <- (analyzed_score - mean_score) / sd_score
percentile_rank <- sum(test_scores <= analyzed_score) / length(test_scores) * 100

# Create comprehensive visualization
png("test_score_relative_standing.png", width = 1200, height = 800, res = 120)
par(mfrow = c(2, 2))

# 1. Histogram with analyzed score marked
hist(test_scores, breaks = 8, col = "lightblue", border = "darkblue",
     main = "Test Score Distribution", xlab = "Score", ylab = "Frequency",
     xlim = c(65, 100))
abline(v = analyzed_score, col = "red", lwd = 3)
abline(v = mean_score, col = "blue", lwd = 2, lty = 2)
abline(v = quartiles, col = "green", lwd = 2, lty = 3)
legend("topright", c("Analyzed Score (87)", "Mean", "Quartiles"), 
       col = c("red", "blue", "green"), lwd = c(3, 2, 2), lty = c(1, 2, 3))

# 2. Box plot with score marked
boxplot(test_scores, horizontal = TRUE, col = "lightgreen",
        main = "Box Plot with Quartiles", xlab = "Score")
points(analyzed_score, 1, col = "red", pch = 19, cex = 2)
text(analyzed_score, 1.3, paste("Score =", analyzed_score), col = "red", font = 2)
text(quartiles[1], 0.7, "Q1", col = "blue", font = 2)
text(quartiles[2], 0.7, "Q2", col = "blue", font = 2)
text(quartiles[3], 0.7, "Q3", col = "blue", font = 2)

# 3. Percentile visualization
sorted_scores <- sort(test_scores)
percentile_positions <- (1:length(sorted_scores) - 0.5) / length(sorted_scores) * 100

plot(sorted_scores, percentile_positions, type = "s", lwd = 2, col = "purple",
     main = "Cumulative Percentile Plot", xlab = "Score", ylab = "Percentile Rank")
points(analyzed_score, percentile_rank, col = "red", pch = 19, cex = 2)
abline(h = c(25, 50, 75), col = "gray", lty = 2)
abline(v = quartiles, col = "gray", lty = 2)
text(analyzed_score + 2, percentile_rank, 
     paste(round(percentile_rank, 1), "th percentile"), col = "red", font = 2)

# 4. Z-score context plot
z_scores_all <- (test_scores - mean_score) / sd_score
plot(test_scores, z_scores_all, pch = 19, col = "darkblue",
     main = "Z-Score Context", xlab = "Score", ylab = "Z-Score")
points(analyzed_score, z_score, col = "red", pch = 19, cex = 2)
abline(h = c(-2, -1, 0, 1, 2), col = "gray", lty = 2)
abline(h = 0, col = "black", lwd = 2)
text(analyzed_score + 2, z_score, 
     paste("z =", round(z_score, 2)), col = "red", font = 2)
text(70, 1.5, "68% of data\n(±1 SD)", cex = 0.8)
text(70, -1.5, "95% of data\n(±2 SD)", cex = 0.8)

dev.off()

# Also create a single comprehensive panel version
png("test_score_comprehensive.png", width = 1000, height = 600, res = 120)

# Combined visualization in one plot
layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE))

# Main histogram with all annotations
hist(test_scores, breaks = 8, col = "lightblue", border = "darkblue",
     main = "Test Score Analysis: Score = 87", xlab = "Score", ylab = "Frequency",
     xlim = c(65, 100))

# Mark the analyzed score
abline(v = analyzed_score, col = "red", lwd = 4)
text(analyzed_score, par("usr")[4] * 0.9, 
     paste("Score = 87\nZ-score =", round(z_score, 2), 
           "\n", round(percentile_rank, 1), "th percentile"), 
     col = "red", font = 2, adj = 0.5)

# Mark quartiles
abline(v = quartiles, col = "green", lwd = 2, lty = 2)
text(quartiles[1], par("usr")[4] * 0.1, "Q1", col = "green", font = 2)
text(quartiles[2], par("usr")[4] * 0.1, "Q2", col = "green", font = 2)
text(quartiles[3], par("usr")[4] * 0.1, "Q3", col = "green", font = 2)

# Mark mean
abline(v = mean_score, col = "blue", lwd = 2, lty = 3)
text(mean_score, par("usr")[4] * 0.7, 
     paste("Mean =", round(mean_score, 1)), col = "blue", font = 2)

# Box plot
boxplot(test_scores, horizontal = TRUE, col = "lightgreen", add = FALSE,
        main = "Quartile Visualization", xlab = "Score")
points(analyzed_score, 1, col = "red", pch = 19, cex = 2)

# Summary statistics table as text
plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
     xlab = "", ylab = "", main = "Analysis Summary", 
     xaxt = "n", yaxt = "n", bty = "n")

summary_text <- paste(
  "RELATIVE STANDING ANALYSIS",
  "========================",
  paste("Raw Score:", analyzed_score),
  paste("Z-score:", round(z_score, 2)),
  paste("Percentile Rank:", round(percentile_rank, 1), "%"),
  paste("Quartile: 3rd (above median)"),
  "",
  "INTERPRETATION:",
  "• Above average performance", 
  "• Better than 60% of class",
  "• In upper half of distribution",
  sep = "\n"
)

text(0.05, 0.95, summary_text, adj = c(0, 1), family = "mono", cex = 0.9)

dev.off()

cat("✓ Visualizations saved:")
cat("\n  - test_score_relative_standing.png (4-panel)")
cat("\n  - test_score_comprehensive.png (integrated)")
