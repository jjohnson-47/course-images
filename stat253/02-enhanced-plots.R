# EPA Mileage Data Analysis - Enhanced Visualizations
# This script shows more customization options
# Plots saved to files (Wayland compatible)

# Create plots directory
if (!dir.exists("plots")) dir.create("plots")

# Load data
mpg_data <- c(36.3, 41.0, 36.9, 37.1, 44.9, 32.7, 37.3, 41.2, 36.6, 32.9,
              40.5, 36.5, 37.6, 33.9, 40.2, 36.2, 37.9, 36.0, 37.9, 35.9)

cat("Enhanced Visualizations\n")
cat("======================\n")

# Enhanced histogram with statistics
png("plots/02_enhanced_histogram.png", width = 800, height = 600, res = 120)
hist(mpg_data,
     main = "EPA Car Mileage with Statistics",
     xlab = "Miles per Gallon (mpg)",
     ylab = "Frequency",
     col = "steelblue",
     border = "white",
     breaks = 8)

# Add mean and median lines
abline(v = mean(mpg_data), col = "red", lwd = 2, lty = 2)
abline(v = median(mpg_data), col = "orange", lwd = 2, lty = 2)
legend("topright", 
       legend = c(paste("Mean =", round(mean(mpg_data), 1)), 
                  paste("Median =", round(median(mpg_data), 1))), 
       col = c("red", "orange"), 
       lty = 2, lwd = 2)
dev.off()
cat("✓ Enhanced histogram saved to plots/02_enhanced_histogram.png\n")

# Multiple dot plot styles
png("plots/02_dotplot_comparison.png", width = 800, height = 800, res = 120)
par(mfrow = c(2, 1))  # Two plots vertically

# Stacked dots
stripchart(mpg_data,
           main = "Dot Plot - Stacked Method",
           xlab = "Miles per Gallon (mpg)",
           method = "stack",
           pch = 19,
           col = "darkgreen",
           cex = 1.2)

# Jittered dots  
stripchart(mpg_data,
           main = "Dot Plot - Jittered Method",
           xlab = "Miles per Gallon (mpg)",
           method = "jitter",
           pch = 16,
           col = "red",
           cex = 1.2)

par(mfrow = c(1, 1))  # Reset
dev.off()
cat("✓ Dot plot comparison saved to plots/02_dotplot_comparison.png\n")

# Detailed stem-and-leaf with different scales
cat("\nDetailed Stem-and-Leaf Analysis:\n")
cat("================================\n")
cat("Standard scale:\n")
stem(mpg_data)
cat("\nDetailed scale (scale = 2):\n")
stem(mpg_data, scale = 2)

cat("\nEnhanced visualizations complete!\n")
cat("View your plots with: eog plots/02_*.png\n")
