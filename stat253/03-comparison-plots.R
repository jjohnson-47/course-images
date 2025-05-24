# EPA Mileage Data Analysis - Plot Comparisons
# This script shows multiple visualizations side-by-side
# Plots saved to files (Wayland compatible)

# Create plots directory
if (!dir.exists("plots")) dir.create("plots")

mpg_data <- c(36.3, 41.0, 36.9, 37.1, 44.9, 32.7, 37.3, 41.2, 36.6, 32.9,
              40.5, 36.5, 37.6, 33.9, 40.2, 36.2, 37.9, 36.0, 37.9, 35.9)

cat("Visualization Comparisons\n")
cat("=========================\n")

# Create 2x2 layout for comparison
png("plots/03_four_plot_comparison.png", width = 1000, height = 800, res = 120)
par(mfrow = c(2, 2))

# Four different ways to visualize the same data
hist(mpg_data, main = "Histogram", xlab = "MPG", col = "lightblue", 
     border = "black")

plot(density(mpg_data), main = "Density Plot", xlab = "MPG", 
     lwd = 2, col = "blue")
polygon(density(mpg_data), col = "lightgreen", alpha = 0.7)

stripchart(mpg_data, main = "Dot Plot", xlab = "MPG", 
           method = "stack", pch = 19, col = "red", cex = 1.2)

boxplot(mpg_data, main = "Box Plot", ylab = "MPG", col = "lightyellow",
        border = "black")

par(mfrow = c(1, 1))  # Reset layout
dev.off()
cat("✓ Four-plot comparison saved to plots/03_four_plot_comparison.png\n")

# Side-by-side histogram comparison with different bin numbers
png("plots/03_histogram_bins.png", width = 1200, height = 400, res = 120)
par(mfrow = c(1, 3))

hist(mpg_data, main = "5 Bins", xlab = "MPG", col = "lightcoral", 
     breaks = 5)
hist(mpg_data, main = "8 Bins (Default)", xlab = "MPG", col = "lightblue", 
     breaks = 8)
hist(mpg_data, main = "12 Bins", xlab = "MPG", col = "lightgreen", 
     breaks = 12)

par(mfrow = c(1, 1))
dev.off()
cat("✓ Histogram bin comparison saved to plots/03_histogram_bins.png\n")

# Comprehensive statistics summary
png("plots/03_statistics_summary.png", width = 800, height = 600, res = 120)
par(mar = c(5, 4, 4, 2))

# Create a summary plot with text
plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
     xlab = "", ylab = "", main = "EPA Mileage Data Summary Statistics",
     xaxt = "n", yaxt = "n")

# Add statistics as text
text(5, 8.5, paste("Sample Size: n =", length(mpg_data)), cex = 1.2, font = 2)
text(5, 7.5, paste("Mean:", round(mean(mpg_data), 2), "mpg"), cex = 1.1)
text(5, 6.8, paste("Median:", round(median(mpg_data), 2), "mpg"), cex = 1.1)
text(5, 6.1, paste("Standard Deviation:", round(sd(mpg_data), 2)), cex = 1.1)
text(5, 5.4, paste("Range:", round(min(mpg_data), 1), "to", 
                   round(max(mpg_data), 1), "mpg"), cex = 1.1)
text(5, 4.7, paste("IQR:", round(quantile(mpg_data, 0.25), 1), "to", 
                   round(quantile(mpg_data, 0.75), 1)), cex = 1.1)

# Add a border
rect(1, 3, 9, 9, border = "black", lwd = 2)
dev.off()
cat("✓ Statistics summary saved to plots/03_statistics_summary.png\n")

# Summary comparison text
cat("\nVisualization Summary:\n")
cat("=====================\n")
cat("📊 Histogram: Shows overall shape and frequency distribution\n")
cat("🔴 Dot Plot: Shows every individual data point and clusters\n") 
cat("📈 Density Plot: Smooth estimate of the distribution shape\n")
cat("📦 Box Plot: Shows quartiles, median, and potential outliers\n\n")

cat("Key Statistics:\n")
cat("- Mean: ", round(mean(mpg_data), 2), " mpg\n")
cat("- Median: ", round(median(mpg_data), 2), " mpg\n")
cat("- Most fuel-efficient: ", max(mpg_data), " mpg\n")
cat("- Least fuel-efficient: ", min(mpg_data), " mpg\n\n")

cat("All comparison plots complete!\n")
cat("View all plots with: eog plots/\n")
cat("Or individually: eog plots/03_*.png\n")
