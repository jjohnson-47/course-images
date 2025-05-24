# Interpreting Standard Deviation - Chebyshev's and Empirical Rules
# Understanding what mean ± k×SD tells us about data

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Interpreting Standard Deviation\n")
cat("==============================\n\n")

# Rat maze timing example from textbook
rat_times <- c(1.2, 1.8, 2.1, 2.5, 2.8, 3.1, 3.4, 3.7, 4.0, 4.3, 
               4.6, 4.9, 5.2, 5.5, 5.8, 6.1, 6.4, 6.7, 7.0, 7.3,
               2.3, 2.9, 3.5, 4.1, 4.7, 5.3, 5.9, 6.5, 3.2, 3.8)  # 30 rats

mean_time <- 3.74  # Given in textbook
sd_time <- 2.20    # Given in textbook

cat("Rat Maze Timing Study:\n")
cat("=====================\n")
cat("Sample size: 30 rats\n")
cat("Mean time:", mean_time, "minutes\n")
cat("Standard deviation:", sd_time, "minutes\n\n")

# Function to apply both rules
apply_interpretation_rules <- function(data, mean_val, sd_val, data_name) {
  n <- length(data)
  
  cat(data_name, "Analysis:\n")
  cat(rep("=", nchar(data_name) + 10), "\n", sep = "")
  cat("Mean =", mean_val, ", SD =", sd_val, "\n\n")
  
  # Calculate intervals
  intervals <- data.frame(
    k = 1:3,
    lower = round(mean_val - (1:3) * sd_val, 2),
    upper = round(mean_val + (1:3) * sd_val, 2)
  )
  
  cat("INTERVAL CALCULATIONS:\n")
  cat("=====================\n")
  for(i in 1:3) {
    cat("k =", i, ": Mean ± ", i, "×SD = ", mean_val, "± ", i, "×", sd_val, 
        " = [", intervals$lower[i], ", ", intervals$upper[i], "]\n", sep = "")
  }
  cat("\n")
  
  # Count actual data in intervals
  actual_counts <- sapply(1:3, function(k) {
    lower <- mean_val - k * sd_val
    upper <- mean_val + k * sd_val
    sum(data >= lower & data <= upper)
  })
  
  actual_percentages <- round(actual_counts / n * 100, 1)
  
  # Chebyshev's Rule predictions
  chebyshev_percentages <- round((1 - 1/(1:3)^2) * 100, 1)
  
  # Empirical Rule predictions (68-95-99.7)
  empirical_percentages <- c(68, 95, 99.7)
  
  cat("RULE COMPARISONS:\n")
  cat("================\n")
  comparison_table <- data.frame(
    Interval = paste("±", 1:3, "SD"),
    Actual_Count = actual_counts,
    Actual_Percent = paste0(actual_percentages, "%"),
    Chebyshev_Min = paste0(chebyshev_percentages, "%"),
    Empirical_Pred = paste0(empirical_percentages, "%")
  )
  
  print(comparison_table)
  cat("\n")
  
  # Interpretation
  cat("INTERPRETATION:\n")
  cat("===============\n")
  cat("• Chebyshev's Rule: Works for ANY data distribution\n")
  cat("  - Guarantees AT LEAST the predicted percentage\n")
  cat("  - Conservative estimates (often exceeded)\n\n")
  
  cat("• Empirical Rule: Works for MOUND-SHAPED data only\n")
  cat("  - More precise predictions\n")
  cat("  - Requires approximately normal distribution\n\n")
  
  # Check which rule fits better
  emp_diff <- abs(actual_percentages - empirical_percentages)
  cheb_diff <- abs(actual_percentages - chebyshev_percentages)
  
  cat("• For this dataset:\n")
  for(i in 1:3) {
    if(actual_percentages[i] >= chebyshev_percentages[i]) {
      cat("  ✓ Chebyshev's rule satisfied for ±", i, "SD\n")
    } else {
      cat("  ✗ Chebyshev's rule violated for ±", i, "SD (unusual!)\n")
    }
  }
  
  avg_emp_error <- mean(emp_diff)
  avg_cheb_error <- mean(cheb_diff)
  
  if(avg_emp_error < avg_cheb_error) {
    cat("  → Empirical Rule fits better (data appears mound-shaped)\n")
  } else {
    cat("  → Chebyshev's Rule more appropriate (data not mound-shaped)\n")
  }
  
  cat("\n")
  return(comparison_table)
}

# Apply to rat data
rat_results <- apply_interpretation_rules(rat_times, mean_time, sd_time, "Rat Maze Data")

# Test scores example
test_scores <- c(45, 52, 58, 63, 67, 71, 75, 78, 82, 85, 89, 92, 96, 
                 68, 72, 76, 79, 83, 86, 90, 74, 77, 80, 84, 87)

cat("\nTest Scores Example:\n")
cat("===================\n")
test_results <- apply_interpretation_rules(test_scores, mean(test_scores), 
                                          sd(test_scores), "Student Test Scores")

# Save summary
sink("output/interpretation_rules_summary.txt")
cat("INTERPRETATION RULES SUMMARY\n")
cat("============================\n\n")
cat("CHEBYSHEV'S RULE (Any Distribution):\n")
cat("• At least 0% within ±1 SD\n")
cat("• At least 75% within ±2 SD\n") 
cat("• At least 89% within ±3 SD\n\n")
cat("EMPIRICAL RULE (Mound-Shaped Only):\n")
cat("• Approximately 68% within ±1 SD\n")
cat("• Approximately 95% within ±2 SD\n")
cat("• Approximately 99.7% within ±3 SD\n\n")
cat("WHEN TO USE WHICH:\n")
cat("• Chebyshev's: Always safe, any data shape\n")
cat("• Empirical: More precise, but only for bell-shaped data\n")
sink()

cat("✓ Rules summary saved to output/interpretation_rules_summary.txt\n")
