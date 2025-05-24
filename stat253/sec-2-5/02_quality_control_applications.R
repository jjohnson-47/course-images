# Quality Control Applications of Mean ± k×SD
# Real-world uses in manufacturing and process control

# Create output directory
if (!dir.exists("output")) dir.create("output")

cat("Quality Control Applications\n")
cat("===========================\n\n")

# Battery life example
cat("Example 1: Battery Life Quality Control\n")
cat("=======================================\n")
battery_mean <- 36  # months
battery_sd <- 6     # months

cat("Manufacturer specifications:\n")
cat("Mean life:", battery_mean, "months\n")
cat("Standard deviation:", battery_sd, "months\n\n")

# Calculate control limits
cat("QUALITY CONTROL LIMITS:\n")
cat("======================\n")
for(k in 1:3) {
  lower <- battery_mean - k * battery_sd
  upper <- battery_mean + k * battery_sd
  cat("±", k, "SD limits: [", lower, ", ", upper, "] months\n")
  
  if(k == 2) {
    cat("  → 95% of batteries should last ", lower, "-", upper, " months\n")
    cat("  → Only 5% expected outside this range\n")
  }
  if(k == 3) {
    cat("  → 99.7% of batteries should last ", lower, "-", upper, " months\n")
    cat("  → Batteries failing before ", lower, " months are very unusual\n")
  }
}
cat("\n")

# Manufacturing example - bottle filling
cat("Example 2: Bottle Filling Process\n")
cat("=================================\n")
target_volume <- 500  # ml
process_sd <- 3       # ml

cat("Target volume:", target_volume, "ml\n")
cat("Process standard deviation:", process_sd, "ml\n\n")

# Control chart limits
cat("PROCESS CONTROL CHART:\n")
cat("=====================\n")
ucl_2sd <- target_volume + 2 * process_sd  # Upper Control Limit
lcl_2sd <- target_volume - 2 * process_sd  # Lower Control Limit
ucl_3sd <- target_volume + 3 * process_sd  # Upper Action Limit  
lcl_3sd <- target_volume - 3 * process_sd  # Lower Action Limit

cat("Warning limits (±2 SD): [", lcl_2sd, ", ", ucl_2sd, "] ml\n")
cat("Action limits (±3 SD):  [", lcl_3sd, ", ", ucl_3sd, "] ml\n\n")

cat("DECISION RULES:\n")
cat("==============\n")
cat("• Values within ±2 SD: Normal variation, continue production\n")
cat("• Values beyond ±2 SD: Investigate process, possible adjustment needed\n")
cat("• Values beyond ±3 SD: Stop production, process is out of control\n\n")

# Simulate some production data
set.seed(789)
production_data <- round(rnorm(20, target_volume, process_sd), 1)

cat("Sample Production Data (20 bottles):\n")
cat("====================================\n")
cat("Volumes:", paste(production_data, collapse = ", "), "ml\n\n")

# Quality assessment
within_2sd <- sum(abs(production_data - target_volume) <= 2 * process_sd)
within_3sd <- sum(abs(production_data - target_volume) <= 3 * process_sd)
beyond_2sd <- production_data[abs(production_data - target_volume) > 2 * process_sd]
beyond_3sd <- production_data[abs(production_data - target_volume) > 3 * process_sd]

cat("QUALITY ASSESSMENT:\n")
cat("==================\n")
cat("Bottles within ±2 SD:", within_2sd, "/", length(production_data), 
    "(", round(within_2sd/length(production_data)*100, 1), "%)\n")
cat("Bottles within ±3 SD:", within_3sd, "/", length(production_data), 
    "(", round(within_3sd/length(production_data)*100, 1), "%)\n")

if(length(beyond_2sd) > 0) {
  cat("WARNING - Bottles beyond ±2 SD:", paste(beyond_2sd, collapse = ", "), "ml\n")
}
if(length(beyond_3sd) > 0) {
  cat("CRITICAL - Bottles beyond ±3 SD:", paste(beyond_3sd, collapse = ", "), "ml\n")
} else {
  cat("✓ No bottles beyond ±3 SD - process in control\n")
}

# Create visualization
png("output/quality_control.png", width = 1200, height = 800, res = 120)
par(mfrow = c(2, 2))

# Battery life distribution
x_battery <- seq(10, 62, by = 1)
y_battery <- dnorm(x_battery, battery_mean, battery_sd)

plot(x_battery, y_battery, type = "l", lwd = 2, col = "blue",
     main = "Battery Life Distribution", xlab = "Months", ylab = "Density")
abline(v = battery_mean, col = "red", lwd = 2)
abline(v = battery_mean + c(-2, 2) * battery_sd, col = "orange", lwd = 2, lty = 2)
abline(v = battery_mean + c(-3, 3) * battery_sd, col = "purple", lwd = 2, lty = 3)
legend("topright", c("Mean", "±2 SD", "±3 SD"), 
       col = c("red", "orange", "purple"), lty = c(1, 2, 3))

# Control chart for bottles
plot(1:length(production_data), production_data, type = "b", pch = 19, col = "blue",
     main = "Bottle Filling Control Chart", xlab = "Bottle Number", ylab = "Volume (ml)",
     ylim = c(lcl_3sd - 1, ucl_3sd + 1))
abline(h = target_volume, col = "green", lwd = 2)
abline(h = c(lcl_2sd, ucl_2sd), col = "orange", lwd = 2, lty = 2)
abline(h = c(lcl_3sd, ucl_3sd), col = "red", lwd = 2, lty = 3)
legend("topright", c("Target", "±2 SD", "±3 SD"), 
       col = c("green", "orange", "red"), lty = c(1, 2, 3), cex = 0.8)

# Histogram of production data
hist(production_data, main = "Production Data Distribution", 
     xlab = "Volume (ml)", col = "lightblue", breaks = 8)
abline(v = target_volume, col = "red", lwd = 2)
abline(v = target_volume + c(-2, 2) * process_sd, col = "orange", lwd = 2, lty = 2)

# Quality zones diagram
plot(1, type = "n", xlim = c(-4, 4), ylim = c(-1, 1), 
     main = "Quality Control Zones", xlab = "Standard Deviations from Mean", ylab = "")

# Color zones
rect(-2, -0.5, 2, 0.5, col = "lightgreen", border = "black")  # Normal zone
rect(-3, -0.5, -2, 0.5, col = "yellow", border = "black")     # Warning zone
rect(2, -0.5, 3, 0.5, col = "yellow", border = "black")       # Warning zone  
rect(-4, -0.5, -3, 0.5, col = "red", border = "black")        # Action zone
rect(3, -0.5, 4, 0.5, col = "red", border = "black")          # Action zone

text(0, 0, "Normal\n(±2 SD)", cex = 0.8, font = 2)
text(-2.5, 0, "Warning", cex = 0.7, font = 2)
text(2.5, 0, "Warning", cex = 0.7, font = 2)
text(-3.5, 0, "Action", cex = 0.7, font = 2)
text(3.5, 0, "Action", cex = 0.7, font = 2)

dev.off()
cat("✓ Quality control plots saved to output/quality_control.png\n")

# Summary report
sink("output/quality_control_report.txt")
cat("QUALITY CONTROL APPLICATIONS REPORT\n")
cat("===================================\n\n")
cat("BATTERY LIFE CONTROL:\n")
cat("Mean ±1 SD: [", battery_mean - battery_sd, ", ", battery_mean + battery_sd, "] months\n")
cat("Mean ±2 SD: [", battery_mean - 2*battery_sd, ", ", battery_mean + 2*battery_sd, "] months\n")
cat("Mean ±3 SD: [", battery_mean - 3*battery_sd, ", ", battery_mean + 3*battery_sd, "] months\n\n")

cat("BOTTLE FILLING CONTROL:\n")
cat("Target: ", target_volume, " ml\n")
cat("Warning limits: [", lcl_2sd, ", ", ucl_2sd, "] ml\n")
cat("Action limits: [", lcl_3sd, ", ", ucl_3sd, "] ml\n\n")

cat("PRACTICAL INTERPRETATION:\n")
cat("• ±1 SD: Expected normal variation\n")
cat("• ±2 SD: 95% of normal production (5% outside = investigate)\n")
cat("• ±3 SD: 99.7% of normal production (0.3% outside = take action)\n")
sink()

cat("✓ Quality control report saved to output/quality_control_report.txt\n")
cat("\nKey insight: Mean ± k×SD creates practical decision boundaries!\n")
