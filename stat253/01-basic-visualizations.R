# EPA Mileage Data Analysis - Basic Visualizations
# This script covers the three main plot types for quantitative data
# Plots saved to files (Wayland compatible)

# Create plots directory
if (!dir.exists("plots")) dir.create("plots")

# Create the dataset
mpg_data <- c(36.3, 41.0, 36.9, 37.1, 44.9, 32.7, 37.3, 41.2, 36.6, 32.9,
              40.5, 36.5, 37.6, 33.9, 40.2, 36.2, 37.9, 36.0, 37.9, 35.9)

# Basic summary
cat("EPA Mileage Data Summary\n")
cat("========================\n")
cat("Sample size:", length(mpg_data), "\n")
summary(mpg_data)
cat("\n")

# 1. HISTOGRAM
png("plots/01_basic_histogram.png", width = 800, height = 600, res = 120)
hist(mpg_data,
     main = "EPA Gas Mileage Distribution",
     xlab = "Miles per Gallon (mpg)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")
dev.off()
cat("✓ Histogram saved to plots/01_basic_histogram.png\n")

# 2. STEM-AND-LEAF PLOT (text output)
cat("\nStem-and-Leaf Plot:\n")
cat("===================\n")
stem(mpg_data)
cat("\n")

# 3. DOT PLOT
png("plots/01_basic_dotplot.png", width = 800, height = 600, res = 120)
stripchart(mpg_data,
           main = "Dot Plot of EPA Gas Mileage",
           xlab = "Miles per Gallon (mpg)",
           method = "stack",
           pch = 19,
           col = "blue")
dev.off()
cat("✓ Dot plot saved to plots/01_basic_dotplot.png\n")

cat("\nBasic visualizations complete!\n")
cat("View your plots with: eog plots/01_*.png\n")
