# Real-World Applications of Relative Standing
# Growth charts, standardized tests, and practical interpretation

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Real-World Applications of Relative Standing\n")
cat("===========================================\n\n")

# 1. Child Growth Percentiles Example
cat("1. CHILD GROWTH CHARTS\n")
cat("======================\n")

# Simulated height data for 3-year-old boys (based on CDC data)
# Mean ≈ 95 cm, SD ≈ 4 cm
set.seed(456)
child_heights <- round(rnorm(100, 95, 4), 1)

cat("Heights of 100 three-year-old boys (cm):\n")
cat("Mean height:", round(mean(child_heights), 1), "cm\n")
cat("Standard deviation:", round(sd(child_heights), 1), "cm\n\n")

# Function to interpret growth percentiles
interpret_growth_percentile <- function(height, data) {
  percentile <- sum(data <= height) / length(data) * 100
  z_score <- (height - mean(data)) / sd(data)
  
  if(percentile < 5) {
    interpretation <- "Below 5th percentile - may need medical evaluation"
  } else if(percentile < 25) {
    interpretation <- "Below average height"
  } else if(percentile <= 75) {
    interpretation <- "Normal/average height range"
  } else if(percentile <= 95) {
    interpretation <- "Above average height"
  } else {
    interpretation <- "Above 95th percentile - exceptionally tall"
  }
  
  cat("Height:", height, "cm\n")
  cat("Percentile:", round(percentile, 1), "\n")
  cat("Z-score:", round(z_score, 2), "\n")
  cat("Interpretation:", interpretation, "\n\n")
}

cat("Growth Chart Examples:\n")
example_heights <- c(87, 91, 95, 99, 103)
for(h in example_heights) {
  interpret_growth_percentile(h, child_heights)
}

# 2. Standardized Test Performance Analysis
cat("2. STANDARDIZED TEST PERFORMANCE\n")
cat("===============================\n")

# GRE Quantitative scores (mean ≈ 152, SD ≈ 9)
gre_mean <- 152
gre_sd <- 9

cat("GRE Quantitative Reasoning Section:\n")
cat("Mean =", gre_mean, ", SD =", gre_sd, "\n\n")

# Function for standardized test interpretation
interpret_standardized_score <- function(score, mean_val, sd_val, test_name) {
  z_score <- (score - mean_val) / sd_val
  percentile <- pnorm(z_score) * 100
  
  if(z_score >= 2) {
    performance <- "Excellent (top 2%)"
  } else if(z_score >= 1) {
    performance <- "Good (top 16%)"
  } else if(z_score >= 0) {
    performance <- "Average or above"
  } else if(z_score >= -1) {
    performance <- "Below average"
  } else {
    performance <- "Well below average"
  }
  
  cat(test_name, "Score:", score, "\n")
  cat("Z-score:", round(z_score, 2), "\n")
  cat("Percentile:", round(percentile, 1), "\n")
  cat("Performance level:", performance, "\n\n")
}

cat("GRE Score Interpretations:\n")
gre_scores <- c(135, 145, 152, 160, 170)
for(score in gre_scores) {
  interpret_standardized_score(score, gre_mean, gre_sd, "GRE")
}

# 3. Employee Performance Reviews
cat("3. EMPLOYEE PERFORMANCE ANALYSIS\n")
cat("================================\n")

# Sales performance data (monthly sales in thousands)
sales_data <- c(45, 52, 58, 61, 65, 68, 72, 75, 78, 82, 85, 89, 92, 95, 98,
                38, 43, 48, 53, 57, 62, 67, 73, 77, 81, 86, 91, 94, 97, 100)

cat("Monthly sales data for 30 employees (in thousands $):\n")
cat("Mean:", round(mean(sales_data), 1), "k\n")
cat("Median:", round(median(sales_data), 1), "k\n")
cat("Q1, Q2, Q3:", paste(round(quantile(sales_data, c(0.25, 0.5, 0.75)), 1), collapse = ", "), "\n\n")

# Performance classification
classify_performance <- function(score, data) {
  percentile <- sum(data <= score) / length(data) * 100
  quartiles <- quantile(data, c(0.25, 0.5, 0.75))
  
  if(percentile >= 90) {
    category <- "Top Performer (promotion candidate)"
  } else if(percentile >= 75) {
    category <- "High Performer (above Q3)"
  } else if(percentile >= 50) {
    category <- "Good Performer (above median)"
  } else if(percentile >= 25) {
    category <- "Average Performer (Q1-Q2)"
  } else {
    category <- "Needs Improvement (below Q1)"
  }
  
  return(list(percentile = percentile, category = category))
}

cat("Employee Performance Classifications:\n")
example_sales <- c(95, 85, 75, 65, 45)
for(sales in example_sales) {
  result <- classify_performance(sales, sales_data)
  cat("$", sales, "k sales: ", round(result$percentile, 1), "th percentile - ", 
      result$category, "\n", sep = "")
}
cat("\n")

# 4. Quality Control with Percentiles
cat("4. MANUFACTURING QUALITY CONTROL\n")
cat("================================\n")

# Product weights (target = 500g)
set.seed(789)
product_weights <- round(rnorm(200, 500, 8), 1)

cat("Product weights (n=200, target=500g):\n")
cat("Mean:", round(mean(product_weights), 1), "g\n")
cat("SD:", round(sd(product_weights), 1), "g\n\n")

# Quality control limits using percentiles
quality_percentiles <- quantile(product_weights, c(0.05, 0.95))
cat("Quality Control Using Percentiles:\n")
cat("5th percentile (lower limit):", quality_percentiles[1], "g\n")
cat("95th percentile (upper limit):", quality_percentiles[2], "g\n")
cat("Products outside 5th-95th percentile range need inspection\n\n")

# Identify products needing inspection - FIXED VERSION
needs_inspection_logical <- product_weights < quality_percentiles[1] | 
                           product_weights > quality_percentiles[2]
needs_inspection_indices <- which(needs_inspection_logical)
needs_inspection_values <- product_weights[needs_inspection_logical]

cat("Products flagged for inspection:", length(needs_inspection_values), "/", 
    length(product_weights), "(", 
    round(length(needs_inspection_values)/length(product_weights)*100, 1), "%)\n")

# Create comprehensive visualization
png("output/real_world_applications.png", width = 1400, height = 1200, res = 120)
par(mfrow = c(3, 2))

# Growth chart simulation
hist(child_heights, main = "Child Height Distribution", xlab = "Height (cm)", 
     col = "lightblue", breaks = 15)
abline(v = quantile(child_heights, c(0.05, 0.25, 0.5, 0.75, 0.95)), 
       col = c("red", "orange", "green", "orange", "red"), lwd = 2)
legend("topright", c("5th %ile", "Q1", "Median", "Q3", "95th %ile"), 
       col = c("red", "orange", "green", "orange", "red"), lwd = 2, cex = 0.7)

# GRE score distribution
x_gre <- seq(110, 190, by = 1)
y_gre <- dnorm(x_gre, gre_mean, gre_sd)
plot(x_gre, y_gre, type = "l", lwd = 2, col = "purple",
     main = "GRE Score Distribution", xlab = "GRE Score", ylab = "Density")
abline(v = gre_mean + c(-2, -1, 0, 1, 2) * gre_sd, 
       col = c("red", "orange", "black", "orange", "red"), lty = 2)

# Sales performance box plot
boxplot(sales_data, main = "Employee Sales Performance", ylab = "Sales ($k)",
        col = "lightgreen")
text(1.3, quantile(sales_data, c(0.25, 0.5, 0.75)), 
     c("Q1", "Median", "Q3"), pos = 4)

# Performance categories
performance_categories <- cut(sales_data, 
                             breaks = quantile(sales_data, c(0, 0.25, 0.5, 0.75, 0.9, 1)),
                             labels = c("Needs Improvement", "Average", "Good", "High", "Top"),
                             include.lowest = TRUE)
barplot(table(performance_categories), main = "Performance Distribution",
        col = rainbow(5), las = 2)

# Quality control chart - FIXED VERSION
plot(1:length(product_weights), product_weights, pch = 19, cex = 0.5,
     main = "Quality Control Chart", xlab = "Product Number", ylab = "Weight (g)")
abline(h = 500, col = "green", lwd = 2)
abline(h = quality_percentiles, col = "red", lwd = 2, lty = 2)
# Fixed the problematic line:
points(needs_inspection_indices, product_weights[needs_inspection_indices], 
       col = "red", pch = 19, cex = 1)

# Percentile interpretation guide
plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10),
     main = "Percentile Interpretation Guide", xlab = "", ylab = "", 
     xaxt = "n", yaxt = "n")
text(5, 9, "Understanding Percentiles", cex = 1.4, font = 2)
text(5, 7.5, "90th+ percentile: Exceptional performance", cex = 1)
text(5, 6.5, "75th-90th percentile: Above average", cex = 1)
text(5, 5.5, "25th-75th percentile: Average range", cex = 1)
text(5, 4.5, "10th-25th percentile: Below average", cex = 1)
text(5, 3.5, "Below 10th percentile: Concerning", cex = 1)

dev.off()
cat("✓ Real-world applications plot saved to output/real_world_applications.png\n")

# Create practical interpretation guide
sink("output/practical_interpretation_guide.txt")
cat("PRACTICAL INTERPRETATION GUIDE\n")
cat("==============================\n\n")
cat("PERCENTILES IN DIFFERENT CONTEXTS:\n\n")
cat("1. STANDARDIZED TESTS (SAT, GRE, etc.):\n")
cat("   • 99th percentile: Elite performance\n")
cat("   • 90th+ percentile: Excellent, competitive for top programs\n") 
cat("   • 75th+ percentile: Good, above average\n")
cat("   • 50th percentile: Average performance\n")
cat("   • Below 25th: May need additional preparation\n\n")

cat("2. GROWTH CHARTS (Medical):\n")
cat("   • Above 95th percentile: Monitor for abnormal growth\n")
cat("   • 25th-75th percentile: Normal range\n")
cat("   • Below 5th percentile: May need medical evaluation\n\n")

cat("3. EMPLOYEE PERFORMANCE:\n")
cat("   • Top 10%: Promotion candidates\n")
cat("   • 75th+ percentile: High performers\n")
cat("   • 25th-75th: Average performers\n")
cat("   • Bottom 10%: Performance improvement needed\n\n")

cat("REMEMBER:\n")
cat("• Percentiles show relative position, not absolute quality\n")
cat("• Same raw score can mean different things in different groups\n")
cat("• Z-scores allow comparison across different distributions\n")
cat("• Always consider the context and reference group\n")
sink()

cat("✓ Practical guide saved to output/practical_interpretation_guide.txt\n")
cat("\nKey takeaway: Relative standing measures are essential for meaningful interpretation!\n")
cat("Raw numbers mean nothing without context - percentiles provide that context.\n")
