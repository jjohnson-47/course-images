# Mode and Frequency Analysis
# Demonstrates mode calculation and frequency tables

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Mode and Frequency Analysis\n")
cat("===========================\n\n")

# Example with repeated values (like HTML example)
mpg_with_repeats <- c(37.0, 36.8, 37.0, 38.1, 36.5, 37.0, 39.2, 36.8, 37.5, 36.8)

cat("MPG Data with Repeated Values:\n")
cat("Data:", paste(mpg_with_repeats, collapse = ", "), "\n\n")

# Create frequency table
freq_table <- table(mpg_with_repeats)
cat("Frequency Table:\n")
print(freq_table)
cat("\n")

# Find mode(s)
max_freq <- max(freq_table)
modes <- names(freq_table)[freq_table == max_freq]

cat("Mode Analysis:\n")
cat("Highest frequency:", max_freq, "\n")
cat("Mode(s):", paste(modes, collapse = ", "), "\n")
cat("The value", modes[1], "appears most frequently (", max_freq, "times)\n\n")

# Different scenarios
cat("Different Mode Scenarios:\n")
cat("========================\n\n")

# No mode
no_mode_data <- c(1, 2, 3, 4, 5)
cat("1. No Mode Example:", paste(no_mode_data, collapse = ", "), "\n")
cat("   All values appear once - no mode\n\n")

# Bimodal
bimodal_data <- c(1, 1, 2, 3, 3, 4)
freq_bi <- table(bimodal_data)
modes_bi <- names(freq_bi)[freq_bi == max(freq_bi)]
cat("2. Bimodal Example:", paste(bimodal_data, collapse = ", "), "\n")
cat("   Modes:", paste(modes_bi, collapse = " and "), "\n\n")

# Multimodal
multimodal_data <- c(5, 5, 7, 7, 9, 9, 11)
freq_multi <- table(multimodal_data)
modes_multi <- names(freq_multi)[freq_multi == max(freq_multi)]
cat("3. Multimodal Example:", paste(multimodal_data, collapse = ", "), "\n")
cat("   Modes:", paste(modes_multi, collapse = ", "), "\n\n")

# Create frequency visualization
png("output/mode_analysis.png", width = 800, height = 600, res = 120)
par(mfrow = c(2, 1))

# Bar plot of frequencies
barplot(freq_table, 
        main = "Frequency Distribution - MPG Data",
        xlab = "Miles per Gallon", 
        ylab = "Frequency",
        col = "lightblue",
        border = "black")

# Highlight the mode
mode_position <- which(names(freq_table) == modes[1])
barplot(freq_table, 
        main = "Mode Highlighted",
        xlab = "Miles per Gallon", 
        ylab = "Frequency",
        col = ifelse(1:length(freq_table) == mode_position, "red", "lightblue"),
        border = "black")

text(mode_position * 1.2 - 0.5, max_freq + 0.1, 
     paste("Mode =", modes[1]), pos = 3, font = 2)

dev.off()
cat("✓ Mode analysis plot saved to output/mode_analysis.png\n")

# Summary function for any dataset
analyze_central_tendency <- function(data, dataset_name) {
  cat("\n", dataset_name, " Analysis:\n")
  cat(rep("=", nchar(dataset_name) + 10), "\n", sep = "")
  cat("Data:", paste(sort(data), collapse = ", "), "\n")
  cat("n =", length(data), "\n")
  cat("Mean:", round(mean(data), 2), "\n")
  cat("Median:", median(data), "\n")
  
  freq_table <- table(data)
  max_freq <- max(freq_table)
  if(max_freq == 1) {
    cat("Mode: No mode (all values unique)\n")
  } else {
    modes <- names(freq_table)[freq_table == max_freq]
    cat("Mode:", paste(modes, collapse = ", "), "(appears", max_freq, "times)\n")
  }
  cat("\n")
}

# Demonstrate with different datasets
analyze_central_tendency(mpg_with_repeats, "MPG Dataset")
analyze_central_tendency(c(32.7, 36.3, 37.1, 35.9, 38.2, 41.0, 36.8, 37.9, 39.0, 33.9), "EPA Dataset")

cat("Analysis complete! Check output/ folder for saved files.\n")
