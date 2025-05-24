# Practical Applications of Variability Measures
# Real-world interpretation and EPA data continuation

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Practical Applications of Variability\n")
cat("====================================\n\n")

# EPA mileage data (continuing from previous sections)
epa_data <- c(32.7, 36.3, 37.1, 35.9, 38.2, 41.0, 36.8, 37.9, 39.0, 33.9)

cat("EPA Mileage Data Analysis:\n")
cat("Data:", paste(sort(epa_data), collapse = ", "), "mpg\n\n")

# Complete analysis
n <- length(epa_data)
mean_mpg <- mean(epa_data)
range_mpg <- max(epa_data) - min(epa_data)
var_mpg <- var(epa_data)
sd_mpg <- sd(epa_data)

cat("Descriptive Statistics:\n")
cat("======================\n")
cat("Sample size (n):", n, "\n")
cat("Mean:", round(mean_mpg, 2), "mpg\n")
cat("Range:", range_mpg, "mpg (", min(epa_data), "to", max(epa_data), ")\n")
cat("Sample variance (s²):", round(var_mpg, 4), "\n")
cat("Sample std deviation (s):", round(sd_mpg, 4), "mpg\n\n")

# Interpretation
cat("INTERPRETATION:\n")
cat("===============\n")
cat("• Average mileage:", round(mean_mpg, 1), "mpg\n")
cat("• Typical variation: ±", round(sd_mpg, 1), "mpg around the mean\n")
cat("• Most cars get between", round(mean_mpg - sd_mpg, 1), 
    "and", round(mean_mpg + sd_mpg, 1), "mpg\n")
cat("• Range shows", range_mpg, "mpg difference between best and worst\n\n")

# Why n-1 vs n demonstration
cat("Why Use (n-1) Instead of n?\n")
cat("===========================\n")
pop_var <- sum((epa_data - mean_mpg)^2) / n  # Population formula
samp_var <- sum((epa_data - mean_mpg)^2) / (n - 1)  # Sample formula

cat("If we used n in denominator:", round(pop_var, 4), "\n")
cat("Using (n-1) in denominator:", round(samp_var, 4), "\n")
cat("Difference:", round(samp_var - pop_var, 4), "\n")
cat("The (n-1) version gives a slightly larger, unbiased estimate\n\n")

# Stock volatility example
cat("Stock Price Volatility Example:\n")
cat("===============================\n")
# Simulate two stocks with same average but different volatility
set.seed(456)
stock_stable <- round(rnorm(10, 50, 2), 2)  # Low volatility
stock_volatile <- round(rnorm(10, 50, 8), 2)  # High volatility

cat("Stable Stock (daily prices): $", paste(stock_stable, collapse = ", $"), "\n")
cat("Volatile Stock (daily prices): $", paste(stock_volatile, collapse = ", $"), "\n\n")

cat("Risk Analysis:\n")
cat("Stable Stock  - Mean: $", round(mean(stock_stable), 2), 
    ", Std Dev: $", round(sd(stock_stable), 2), "\n")
cat("Volatile Stock - Mean: $", round(mean(stock_volatile), 2), 
    ", Std Dev: $", round(sd(stock_volatile), 2), "\n\n")

# Create comprehensive visualization
png("output/practical_applications.png", width = 1200, height = 900, res = 120)
par(mfrow = c(3, 2))

# EPA data histogram with stats
hist(epa_data, main = "EPA Mileage Distribution", xlab = "MPG", 
     col = "lightblue", breaks = 6)
abline(v = mean_mpg, col = "red", lwd = 2)
abline(v = mean_mpg - sd_mpg, col = "orange", lwd = 2, lty = 2)
abline(v = mean_mpg + sd_mpg, col = "orange", lwd = 2, lty = 2)
legend("topright", c("Mean", "±1 Std Dev"), col = c("red", "orange"), lty = c(1, 2))

# EPA boxplot
boxplot(epa_data, main = "EPA Mileage Boxplot", ylab = "MPG", col = "lightgreen")

# Stock comparison
plot(1:10, stock_stable, type = "b", col = "blue", pch = 19, 
     main = "Stock Price Comparison", xlab = "Day", ylab = "Price ($)",
     ylim = c(min(c(stock_stable, stock_volatile)), max(c(stock_stable, stock_volatile))))
lines(1:10, stock_volatile, type = "b", col = "red", pch = 17)
legend("topright", c("Stable", "Volatile"), col = c("blue", "red"), pch = c(19, 17))

# Variability measures comparison
measures <- c("Range", "Variance", "Std Dev")
epa_values <- c(range_mpg, var_mpg, sd_mpg)
stable_values <- c(max(stock_stable) - min(stock_stable), var(stock_stable), sd(stock_stable))

barplot(rbind(epa_values, stable_values), beside = TRUE, 
        names.arg = measures, main = "Variability Measures Comparison",
        col = c("lightblue", "blue"), legend = c("EPA Data", "Stable Stock"))

# Table of all examples
plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10), xlab = "", ylab = "",
     main = "Summary of Examples", xaxt = "n", yaxt = "n")

text(5, 8, "Variability in Different Contexts", cex = 1.4, font = 2)
text(2, 6.5, "EPA Mileage:", cex = 1.1, font = 2)
text(2, 6, paste("SD =", round(sd_mpg, 2), "mpg"), cex = 1)
text(2, 4.5, "Stock Prices:", cex = 1.1, font = 2)
text(2, 4, paste("Stable: SD = $", round(sd(stock_stable), 2)), cex = 1)
text(2, 3.5, paste("Volatile: SD = $", round(sd(stock_volatile), 2)), cex = 1)

# Range limitations example
sample_same_range1 <- c(1, 5, 5, 5, 9)
sample_same_range2 <- c(1, 3, 5, 7, 9)

stripchart(list("Sample 1" = sample_same_range1, "Sample 2" = sample_same_range2),
           main = "Same Range, Different Variability", method = "stack",
           pch = 19, col = c("red", "blue"))
text(5, 1.5, paste("Both ranges = 8, but SD₁ =", round(sd(sample_same_range1), 2),
                   "SD₂ =", round(sd(sample_same_range2), 2)), cex = 0.9)

dev.off()
cat("✓ Applications plot saved to output/practical_applications.png\n")

# Create summary report
sink("output/variability_report.txt")
cat("VARIABILITY ANALYSIS REPORT\n")
cat("===========================\n\n")
cat("1. EPA MILEAGE DATA:\n")
cat("   Mean:", round(mean_mpg, 2), "mpg\n")
cat("   Standard Deviation:", round(sd_mpg, 2), "mpg\n")
cat("   Interpretation: Cars typically get", round(mean_mpg, 1), "±", round(sd_mpg, 1), "mpg\n\n")

cat("2. KEY INSIGHTS:\n")
cat("   • Standard deviation is in original units (mpg, dollars, etc.)\n")
cat("   • Larger SD = more variability\n")
cat("   • Range can be misleading with large datasets\n")
cat("   • Variance uses (n-1) for unbiased estimation\n\n")

cat("3. PRACTICAL APPLICATIONS:\n")
cat("   • Quality control: Lower variability = more consistent products\n")
cat("   • Risk assessment: Higher variability = more risk\n")
cat("   • Performance evaluation: Consider both average AND consistency\n")
sink()

cat("\n✓ Report saved to output/variability_report.txt\n")
cat("\nAnalysis complete! Files saved in output/ directory.\n")
cat("Key takeaway: Variability measures are essential for complete data description!\n")
