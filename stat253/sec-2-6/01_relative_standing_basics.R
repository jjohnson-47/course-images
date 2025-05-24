# Numerical Measures of Relative Standing
# Percentiles, Quartiles, and Z-scores

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Numerical Measures of Relative Standing\n")
cat("======================================\n\n")

# Test scores example - class of 25 students
test_scores <- c(72, 78, 81, 85, 88, 90, 92, 95, 68, 76, 83, 87, 91, 94, 97,
                 74, 79, 84, 86, 89, 93, 96, 75, 82, 77)

cat("Class Test Scores (n=25):\n")
cat("=========================\n")
cat("Scores:", paste(sort(test_scores), collapse = ", "), "\n")
cat("Mean:", round(mean(test_scores), 1), "\n")
cat("Standard deviation:", round(sd(test_scores), 1), "\n\n")

# 1. PERCENTILES AND QUARTILES
cat("1. PERCENTILES AND QUARTILES\n")
cat("============================\n")

# Calculate quartiles
quartiles <- quantile(test_scores, probs = c(0.25, 0.5, 0.75))
cat("Quartiles:\n")
cat("Q1 (25th percentile):", quartiles[1], "\n")
cat("Q2 (50th percentile/median):", quartiles[2], "\n") 
cat("Q3 (75th percentile):", quartiles[3], "\n\n")

# Calculate common percentiles
percentiles <- quantile(test_scores, probs = seq(0.1, 0.9, 0.1))
cat("Key Percentiles:\n")
for(i in 1:length(percentiles)) {
  p <- as.numeric(names(percentiles)[i]) * 100
  cat("P", p, ":", percentiles[i], "\n", sep = "")
}
cat("\n")

# Interpretation of quartiles
cat("QUARTILE INTERPRETATION:\n")
cat("=======================\n")
cat("• Q1 = ", quartiles[1], ": 25% of students scored below ", quartiles[1], "\n")
cat("• Q2 = ", quartiles[2], ": 50% of students scored below ", quartiles[2], " (median)\n")
cat("• Q3 = ", quartiles[3], ": 75% of students scored below ", quartiles[3], "\n")
cat("• Interquartile Range (IQR) = Q3 - Q1 = ", quartiles[3] - quartiles[1], "\n\n")

# 2. Z-SCORES
cat("2. Z-SCORES (STANDARDIZED SCORES)\n")
cat("=================================\n")

mean_score <- mean(test_scores)
sd_score <- sd(test_scores)

# Calculate z-scores for all data
z_scores <- (test_scores - mean_score) / sd_score

cat("Formula: z = (x - mean) / standard deviation\n")
cat("For this dataset: z = (x - ", round(mean_score, 1), ") / ", round(sd_score, 1), "\n\n")

# Show z-scores for a few examples
example_scores <- c(95, 87, 72)
cat("Z-score Examples:\n")
for(score in example_scores) {
  z <- (score - mean_score) / sd_score
  cat("Score ", score, ": z = (", score, " - ", round(mean_score, 1), ") / ", 
      round(sd_score, 1), " = ", round(z, 2), "\n")
}
cat("\n")

# 3. PERCENTILE RANKS
cat("3. PERCENTILE RANKS\n")
cat("===================\n")

# Function to calculate percentile rank
percentile_rank <- function(x, data) {
  rank_value <- sum(data <= x) / length(data) * 100
  return(round(rank_value, 1))
}

cat("Percentile rank tells us what percentage of data falls below a given value.\n\n")

# Calculate percentile ranks for example scores
cat("Percentile Rank Examples:\n")
for(score in example_scores) {
  rank <- percentile_rank(score, test_scores)
  cat("Score ", score, ": ", rank, "% of students scored at or below this\n")
}
cat("\n")

# Complete analysis for one specific score
analyze_score <- 87
cat("COMPLETE ANALYSIS FOR SCORE = ", analyze_score, ":\n")
cat("=====================================\n")

z_score_87 <- (analyze_score - mean_score) / sd_score
percentile_87 <- percentile_rank(analyze_score, test_scores)

# Determine quartile
if(analyze_score <= quartiles[1]) {
  quartile_position <- "1st quartile (bottom 25%)"
} else if(analyze_score <= quartiles[2]) {
  quartile_position <- "2nd quartile (25th-50th percentile)"
} else if(analyze_score <= quartiles[3]) {
  quartile_position <- "3rd quartile (50th-75th percentile)" 
} else {
  quartile_position <- "4th quartile (top 25%)"
}

cat("Raw score:", analyze_score, "\n")
cat("Z-score:", round(z_score_87, 2), "\n")
cat("Percentile rank:", percentile_87, "%\n")
cat("Quartile position:", quartile_position, "\n\n")

cat("INTERPRETATION:\n")
cat("• This score is", round(abs(z_score_87), 2), "standard deviations", 
    ifelse(z_score_87 > 0, "above", "below"), "the mean\n")
cat("• This student performed better than", percentile_87, "% of the class\n")
cat("• This score falls in the", quartile_position, "\n")

# Save summary table
summary_table <- data.frame(
  Score = sort(test_scores),
  Z_Score = round((sort(test_scores) - mean_score) / sd_score, 2),
  Percentile_Rank = sapply(sort(test_scores), function(x) percentile_rank(x, test_scores))
)

# Save to file
write.csv(summary_table, "output/relative_standing_summary.csv", row.names = FALSE)
cat("\n✓ Complete analysis saved to output/relative_standing_summary.csv\n")
