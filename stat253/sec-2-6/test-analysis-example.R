##############################################################################
# Test‑Score Relative Standing – Clean‑layout version
#   • Non‑overlapping labels (dynamic offsets, use outer margins)
#   • Slightly smaller default label size (cex = 0.85)
##############################################################################

test_scores     <- c(68,72,74,75,76,77,78,79,81,82,83,84,85,
                     86,87,88,89,90,91,92,93,94,95,96,97)
analyzed_score  <- 87

mean_score      <- mean(test_scores)
sd_score        <- sd(test_scores)
quartiles       <- quantile(test_scores, probs = c(.25,.5,.75))
z_score         <- (analyzed_score - mean_score) / sd_score
percentile_rank <- mean(test_scores <= analyzed_score) * 100

##############################################################################
# 4‑panel figure -------------------------------------------------------------
##############################################################################
png("test_score_relative_standing.png", width = 1200, height = 800, res = 120)
par(mfrow = c(2, 2), mar = c(4.2, 4.2, 3.5, 1.2), cex = .85)  # add a touch of margin

## 1. Histogram --------------------------------------------------------------
h <- hist(test_scores, breaks = 8, col = "#c6e2ff", border = "#003366",
          main = "Test‑score distribution", xlab = "Score", ylab = "Frequency",
          xlim = c(65, 100))
abline(v = analyzed_score, col = "red",  lwd = 3)
abline(v = mean_score,    col = "blue", lwd = 2, lty = 2)
abline(v = quartiles,     col = "forestgreen", lwd = 2, lty = 3)

legend("topleft",
       legend = c(sprintf("Score = %d", analyzed_score), "Mean", "Quartiles"),
       col    = c("red", "blue", "forestgreen"),
       lwd    = c(3,2,2), lty = c(1,2,3), bg = "white", box.lty = 0, cex = .8)

## 2. Box‑plot ---------------------------------------------------------------
boxplot(test_scores, horizontal = TRUE, col = "#ccffcc",
        main = "Box‑plot  (quartiles marked)", xlab = "Score")

points(analyzed_score, 1, col = "red", pch = 19, cex = 1.8)
text(analyzed_score, 1.25, sprintf("Score = %d", analyzed_score),
     col = "red", font = 2, adj = c(.5,0))

# Quartile labels: place slightly above the line, centred
text(quartiles, rep(0.8, 3), labels = c("Q1","Median","Q3"),
     col = "blue4", font = 2, adj = .5)

## 3. Empirical‑percentile curve --------------------------------------------
sorted_scores       <- sort(test_scores)
percentile_positions <- (seq_along(sorted_scores) - 0.5) / length(sorted_scores) * 100

plot(sorted_scores, percentile_positions, type = "s", lwd = 2, col = "purple",
     main = "Cumulative percentile plot", xlab = "Score", ylab = "Percentile")

points(analyzed_score, percentile_rank, col = "red", pch = 19, cex = 1.8)

# place label to the left or right depending on location
h.offset  <- ifelse(analyzed_score > mean_score, -3,  3)
h.adjust  <- ifelse(h.offset < 0, 1, 0)  # right‑ or left‑justify
text(analyzed_score + h.offset, percentile_rank,
     sprintf("%.1fth %%", percentile_rank),
     col = "red", font = 2, adj = c(h.adjust, .5))

abline(h = c(25, 50, 75), col = "grey60", lty = 2)
abline(v = quartiles,       col = "grey60", lty = 2)

## 4. Z‑score context --------------------------------------------------------
z_scores_all <- (test_scores - mean_score) / sd_score
plot(test_scores, z_scores_all, pch = 19, col = "#003366",
     main = "Z‑score context", xlab = "Score", ylab = "Z‑score")
points(analyzed_score, z_score, col = "red", pch = 19, cex = 1.8)

v.offset <- ifelse(z_score > 0,  0.15, -0.15)
v.adjust <- ifelse(z_score > 0,  0,  1)
text(analyzed_score, z_score + v.offset,
     sprintf("z = %.2f", z_score),
     col = "red", font = 2, adj = c(.5, v.adjust))

abline(h = c(-2,-1,0,1,2), col = "grey60", lty = 2)
abline(h = 0, lwd = 2)           # mean line

# annotate SD ranges outside plotting region to keep it clear
usr <- par("usr"); xpd <- par("xpd"); par(xpd = NA)
text(usr[1] - 2,  1,  "±1 SD (68%)", cex = 0.8, adj = 0)
text(usr[1] - 2, -1.9,"±2 SD (95%)", cex = 0.8, adj = 0)
par(xpd = xpd)   # restore clipping

dev.off()

##############################################################################
# ── SINGLE‑PANEL “DASHBOARD”  (cleaned‑up layout) ───────────────────────────
##############################################################################
png("test_score_comprehensive.png", width = 1000, height = 650, res = 120)

layout(matrix(c(1,1,2,3), nrow = 2, byrow = TRUE), heights = c(2,1))
par(mar = c(4.2, 4.2, 3.8, 1.2), cex = .9)      # slightly larger text

## ── 1. HISTOGRAM ────────────────────────────────────────────────────────────
h <- hist(test_scores, breaks = 8, col = "#c6e2ff", border = "#003366",
          main = "Test‑score analysis  (score = 87)", xlab = "Score",
          ylab = "Frequency", xlim = c(65, 100))

hist_ymax <- max(h$counts)

# analysed score
abline(v = analyzed_score, col = "red", lwd = 4)
text(analyzed_score + 2.5,                  # → move right of red line
     hist_ymax * 0.80,                     # ↓ a bit lower
     sprintf("Score = %d\nz = %.2f\n%.1fth %%ile",
             analyzed_score, z_score, percentile_rank),
     col = "red", font = 2, adj = c(0, .5))

# quartile markings
abline(v = quartiles, col = "forestgreen", lwd = 2, lty = 2)
text(quartiles,
     rep(-0.6, 3),                         # ↓ farther below the axis
     labels = c("Q1", "Median", "Q3"),
     col = "forestgreen", font = 2, adj = .5, xpd = NA)

# mean
abline(v = mean_score, col = "blue", lwd = 2, lty = 3)
text(mean_score,
     hist_ymax * 0.92,
     sprintf("Mean = %.1f", mean_score),
     col = "blue", font = 2, adj = .5)

legend("topleft",
       legend = c("Score", "Mean", "Quartiles"),
       col    = c("red", "blue", "forestgreen"),
       lwd    = c(4, 2, 2), lty = c(1, 3, 2),
       bg = "white", box.lty = 0, cex = .8)

## ── 2. BOX‑PLOT ─────────────────────────────────────────────────────────────
boxplot(test_scores, horizontal = TRUE, col = "#ccffcc",
        main = "Quartiles and individual score", xlab = "Score")
points(analyzed_score, 1, col = "red", pch = 19, cex = 1.6)

## ── 3. SUMMARY “TABLE” ──────────────────────────────────────────────────────
plot.new(); plot.window(xlim = 0:1, ylim = 0:1)
mtext("Analysis summary", font = 2, cex = 1.05, side = 3, adj = 0.02, line = 1)

summary_lines <- c(
  sprintf("Raw score        : %d",   analyzed_score),
  sprintf("Z‑score          : %.2f", z_score),
  sprintf("Percentile rank  : %.1f%%", percentile_rank),
  "Quartile          : 3rd (above median)",
  "",
  "Interpretation:",
  "  • above‑average performance",
  "  • better than ≈60% of the class",
  "  • solidly in the upper half")

# print lines with consistent spacing
y <- 0.88
for (ln in summary_lines) {
  text(0.02, y, ln, adj = 0, family = "mono")
  y <- y - 0.08                     # uniform line spacing
}

dev.off()

cat("✓ test_score_comprehensive.png regenerated with improved layout\n")

