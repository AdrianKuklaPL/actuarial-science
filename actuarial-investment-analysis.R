##################### Investment Analysis #########################

# Prevent scientific notation for numbers and set seed for reproducibility.
options(scipen = 999)
set.seed(47893)

##################### Profitability Analysis of Portfolios A and B ####################

# Portfolio A: Simulate profitability levels without illness D
# Initialize parameters for Portfolio A
prem = 2748.76 * (1 + theta)  # Annual premium with profit loading
int = 0.04  # Interest rate
z = 0  # Placeholder for portfolio profitability
theta = 0.05  # Profit loading
M = 15000  # Sum assured for Portfolio A
q = c(0.05, 0.05, 0.05, 0.05, 0.05)  # Annual probability of policy lapse

# Simulate portfolio performance for 1000 experiments, each with 200 contracts
for (j in 1:1000) {
  prof = 0
  for (i in 1:200) {
    inforce = 1; t = 0
    while (inforce == 1) {
      prof = prof + prem / (1 + int)^t  # Add premium income
      t = t + 1
      r = runif(1, 0, 1)  # Generate random number to simulate lapse
      if (r < q[t]) {
        inforce = 0
        prof = prof - 10000 / (1 + int)^t  # Deduct payout on lapse
      }
      if (t == 5 & r > q[t]) {
        inforce = 0
        prof = prof - M / (1 + int)^t  # Deduct maturity payout
      }
    }
  }
  z[j] = prof  # Store portfolio profitability for experiment j
}

# Portfolio B: Simulate profitability levels without illness D
# Initialize parameters for Portfolio B
prem2 = 4260.75 * (1 + theta)  # Annual premium with profit loading
int = 0.04  # Interest rate
x = 0  # Placeholder for portfolio profitability
theta = 0.05  # Profit loading
M2 = 25000  # Sum assured for Portfolio B
q = c(0.05, 0.05, 0.05, 0.05, 0.05)  # Annual probability of policy lapse

# Simulate portfolio performance for 1000 experiments, each with 125 contracts
for (k in 1:1000) {
  prof = 0
  for (h in 1:125) {
    inforce = 1; t = 0
    while (inforce == 1) {
      prof = prof + prem2 / (1 + int)^t  # Add premium income
      t = t + 1
      r = runif(1, 0, 1)  # Generate random number to simulate lapse
      if (r < q[t]) {
        inforce = 0
        prof = prof - 10000 / (1 + int)^t  # Deduct payout on lapse
      }
      if (t == 5 & r > q[t]) {
        inforce = 0
        prof = prof - M2 / (1 + int)^t  # Deduct maturity payout
      }
    }
  }
  x[k] = prof  # Store portfolio profitability for experiment k
}

# Analyze results: Compute means, standard deviations, and confidence intervals
mean(z)  # Mean profitability for Portfolio A
mean(x)  # Mean profitability for Portfolio B
sd(z)  # Standard deviation for Portfolio A
sd(x)  # Standard deviation for Portfolio B
sd(z) / mean(z)  # Coefficient of variation (sample error) for Portfolio A
sd(x) / mean(x)  # Coefficient of variation (sample error) for Portfolio B
mean(z) - sd(z); mean(z) + sd(z)  # 68% confidence interval for Portfolio A
mean(x) - sd(x); mean(x) + sd(x)  # 68% confidence interval for Portfolio B

# Additional profitability metrics
length(subset(z, z > 0)) / 1000  # Proportion of positive outcomes for Portfolio A
length(subset(x, x > 0)) / 1000  # Proportion of positive outcomes for Portfolio B
quantile(z, 0.975); quantile(z, 0.025)  # 95% confidence interval bounds for Portfolio A
quantile(z, 0.84); quantile(z, 0.16)  # 68% confidence interval bounds for Portfolio A

# Graphical representation of portfolio profitability distributions.

# Portfolio A: Histogram and density plot
# Simulate random normal data to represent portfolio profits without profit loading.
y = rnorm(50000, 0, 28973.38)
hist(y, prob = TRUE, col = "#999999", 
     main = "Effect of profit loading on portfolio A's profits",
     xlim = c(-200000, 200000), breaks = 20,
     xlab = "Profit (or Loss) made on portfolio in money terms", cex.lab = 1)

# Add annotations for visual clarity to compare scenarios with and without profit loading.
text(x = -145000, y = 0.0000097, "Without profit loading", col = "#999999", cex = 1.3)
text(x = -153000, y = 0.0000105, "With profit loading", col = "firebrick3", cex = 1.3)
points(-199950, 0.0000097, pch = 15, cex = 1.2, col = "#999999")
points(-199950, 0.0000105, pch = 15, cex = 1.2, col = "firebrick3")

# Overlay the density curve for profits with profit loading.
lines(density(z), col = "firebrick3", lwd = 3)

# Portfolio B: Histogram and density plot
# Simulate random normal data to represent portfolio profits without profit loading.
y2 = rnorm(50000, 0, 35959.8)
hist(y2, prob = TRUE, col = "#999999", 
     main = "Effect of profit loading on portfolio B's profits",
     xlim = c(-200000, 250000), ylim = c(0, 0.000013), breaks = 25,
     xlab = "Profit (or Loss) made on portfolio in money terms")

# Add annotations for visual clarity to compare scenarios with and without profit loading.
text(x = -145000, y = 0.0000095, "Without profit loading", col = "#999999", cex = 1.3)
text(x = -153500, y = 0.0000105, "With profit loading", col = "#66CCFF", cex = 1.3)
points(-205950, 0.0000095, pch = 15, cex = 1.2, col = "#999999")
points(-205950, 0.0000105, pch = 15, cex = 1.2, col = "#66CCFF")

# Overlay the density curve for profits with profit loading.
lines(density(x), col = "#66CCFF", lwd = 3)

# Combined comparison: Overlay density plots for Portfolios A and B
plot(density(x), xlab = "", ylab = "", main = "", ylim = c(0, 0.000014))

# Highlight the density areas for both portfolios and overlay the density lines.
polygon(density(x), col = "firebrick3", border = "black")
polygon(density(z), col = "#66CCFF", border = "black")
lines(density(x), col = "firebrick3", lwd = 3)
lines(density(z), col = "#66CCFF", lwd = 3)

# Add text annotations to label the portfolios in the plot.
text(x = 10000, y = 0.00001, "Portfolio B", col = "#66CCFF", cex = 1.5)
text(x = 10000, y = 0.0000105, "Portfolio A", col = "firebrick3", cex = 1.5)
points(-12000, 0.00001, pch = 15, cex = 1.1, col = "#66CCFF")
points(-12000, 0.0000105, pch = 15, cex = 1.1, col = "firebrick3")

# Add plot title and axis labels to summarize the comparison.
title(main = "Comparison of Portfolios A's and B's Profits without illness",
      xlab = "Profit (or Loss) made on portfolio in money terms",
      ylab = "Density")

##################### Portfolio A and B with Allowance for Illness D ###########################
# Simulate the impact of higher policy lapse probability (q = 0.1) on profitability.
# Analyze variability in normal events and changes in profitability when illness D is priced in.

# Portfolio A: Simulate profitability levels with allowance for illness D
set.seed(47893)
prem3 = 2748.76 * (1 + theta)  # Annual premium with profit loading
int = 0.04  # Interest rate
zd = 0  # Placeholder for portfolio profitability
theta = 0.05  # Profit loading
M = 15000  # Sum assured for Portfolio A
q = c(0.1, 0.1, 0.1, 0.1, 0.1)  # Higher annual probability of policy lapse

# Simulate portfolio performance for 1000 experiments, each with 200 contracts
for (j in 1:1000) {
  prof = 0
  for (i in 1:200) {
    inforce = 1; t = 0
    while (inforce == 1) {
      prof = prof + prem3 / (1 + int)^t  # Add premium income
      t = t + 1
      r = runif(1, 0, 1)  # Generate random number to simulate lapse
      if (r < q[t]) {
        inforce = 0
        prof = prof - 10000 / (1 + int)^t  # Deduct payout on lapse
      }
      if (t == 5 & r > q[t]) {
        inforce = 0
        prof = prof - M / (1 + int)^t  # Deduct maturity payout
      }
    }
  }
  zd[j] = prof  # Store portfolio profitability for experiment j
}

# Portfolio B: Simulate profitability levels with allowance for illness D
set.seed(47893)
prem4 = 4260.75 * (1 + theta)  # Annual premium with profit loading
int = 0.04  # Interest rate
xd = 0  # Placeholder for portfolio profitability
theta = 0.05  # Profit loading
M = 25000  # Sum assured for Portfolio B
q = c(0.1, 0.1, 0.1, 0.1, 0.1)  # Higher annual probability of policy lapse

# Simulate portfolio performance for 1000 experiments, each with 125 contracts
for (k in 1:1000) {
  prof = 0
  for (h in 1:125) {
    inforce = 1; t = 0
    while (inforce == 1) {
      prof = prof + prem4 / (1 + int)^t  # Add premium income
      t = t + 1
      r = runif(1, 0, 1)  # Generate random number to simulate lapse
      if (r < q[t]) {
        inforce = 0
        prof = prof - 10000 / (1 + int)^t  # Deduct payout on lapse
      }
      if (t == 5 & r > q[t]) {
        inforce = 0
        prof = prof - M / (1 + int)^t  # Deduct maturity payout
      }
    }
  }
  xd[k] = prof  # Store portfolio profitability for experiment k
}

# Analyze results: Compute means, standard deviations, and confidence intervals
mean(zd)  # Mean profitability for Portfolio A with illness D
mean(xd)  # Mean profitability for Portfolio B with illness D
sd(zd)  # Standard deviation for Portfolio A
sd(xd)  # Standard deviation for Portfolio B
sd(zd) / mean(zd)  # Coefficient of variation (sample error) for Portfolio A
sd(xd) / mean(xd)  # Coefficient of variation (sample error) for Portfolio B

# Compute 68% and 95% confidence intervals for both portfolios
quantile(zd, 0.84); quantile(zd, 0.16)  # 68% CI for Portfolio A
quantile(zd, 0.975); quantile(zd, 0.025)  # 95% CI for Portfolio A
quantile(xd, 0.84); quantile(xd, 0.16)  # 68% CI for Portfolio B
quantile(xd, 0.975); quantile(xd, 0.025)  # 95% CI for Portfolio B

# Compute standard deviation-based confidence intervals for Portfolio B
mean(xd) - sd(xd); mean(xd) + sd(xd)

# Graphical representation of portfolio profitability distributions with allowance for illness.

# Portfolio A: Histogram and density plot to compare scenarios with and without profit loading and illness.
y = rnorm(50000, 0, 28973.38)
hist(y, prob = TRUE, col = "#999999",
     main = "Effect of profit loading & illness on portfolio A's profits",
     xlim = c(-200000, 200000),
     xlab = "Profit (or Loss) made on portfolio in money terms", cex.lab = 1)

# Annotate the histogram to distinguish scenarios with and without profit loading and illness.
text(x = -120000, y = 0.00001, "Without both profit loading and illness", col = "#999999", cex = 1.2)
text(x = -137000, y = 0.0000105, "With profit loading and illness", col = "firebrick3", cex = 1.2)
points(-203950, 0.00001, pch = 15, cex = 1.2, col = "#999999")
points(-203950, 0.0000105, pch = 15, cex = 1.2, col = "firebrick3")

# Overlay the density curve for profits with profit loading and illness.
lines(density(zd), col = "firebrick3", lwd = 3)

# Portfolio A: Histogram to show the impact of illness on profitability.
hist(z, prob = TRUE, col = "#999999",
     main = "Impact of illness on portfolio A's profits",
     xlim = c(-200000, 300000),
     xlab = "Profit (or Loss) made on portfolio in money terms", cex.lab = 1)

# Annotate the histogram to compare scenarios with and without illness.
text(x = -110000, y = 0.00001, "Portfolio A without illness", col = "#999999", cex = 1.3)
text(x = -119500, y = 0.0000105, "Portfolio A with illness", col = "firebrick3", cex = 1.3)
points(-189950, 0.00001, pch = 15, cex = 1.2, col = "#999999")
points(-189950, 0.0000105, pch = 15, cex = 1.2, col = "firebrick3")

# Overlay the density curve for profits with illness.
lines(density(zd), col = "firebrick3", lwd = 3)

# Portfolio B: Histogram to show the impact of illness on profitability.
hist(x, prob = TRUE, col = "#999999",
     main = "Impact of illness on portfolio B's profits",
     xlim = c(-200000, 350000),
     xlab = "Profit (or Loss) made on portfolio in money terms", cex.lab = 1)

# Annotate the histogram to compare scenarios with and without illness.
text(x = -90000, y = 0.00001, "Portfolio B without illness", col = "#999999", cex = 1.8)
text(x = -104500, y = 0.0000105, "Portfolio B with illness", col = "#66CCFF", cex = 1.8)
points(-209950, 0.00001, pch = 15, cex = 1.2, col = "#999999")
points(-209950, 0.0000105, pch = 15, cex = 1.2, col = "#66CCFF")

# Overlay the density curve for profits with illness.
lines(density(xd), col = "#66CCFF", lwd = 3)

# Portfolio B: Histogram to compare scenarios with and without profit loading and illness.
y2 = rnorm(50000, 0, 35959.8)
hist(y2, prob = TRUE, col = "#999999",
     main = "Effect of profit loading and illness on portfolio B's profits",
     xlim = c(-200000, 350000), ylim = c(0, 0.000013), breaks = 25,
     xlab = "Profit (or Loss) made on portfolio in money terms")

# Annotate the histogram to distinguish scenarios with and without profit loading and illness.
text(x = 210000, y = 0.00001, "Without both profit loading and illness", col = "#999999", cex = 1.5)
text(x = 177500, y = 0.0000105, "With profit loading and illness", col = "#66CCFF", cex = 1.5)
points(60000, 0.00001, pch = 15, cex = 1.3, col = "#999999")
points(60000, 0.0000105, pch = 15, cex = 1.3, col = "#66CCFF")

# Overlay the density curve for profits with profit loading and illness.
lines(density(xd), col = "#66CCFF", lwd = 3)

# Combined comparison of Portfolios A and B: Overlay density plots.
plot(density(xd), xlab = "", ylab = "", main = "",
     ylim = c(0, 0.00001), xlim = c(-150000, 350000))

# Highlight density areas and overlay density lines for both portfolios.
polygon(density(xd), col = "firebrick3", border = "black")
polygon(density(zd), col = "#66CCFF", border = "black")
lines(density(xd), col = "firebrick3", lwd = 3)
lines(density(zd), col = "#66CCFF", lwd = 3)

# Add text annotations to label the portfolios and distinguish their curves.
text(x = -88000, y = 0.0000078, "Portfolio B", col = "#66CCFF", cex = 2)
text(x = -88000, y = 0.00000835, "Portfolio A", col = "firebrick3", cex = 2)
points(-140000, 0.0000078, pch = 15, cex = 1.5, col = "#66CCFF")
points(-140000, 0.00000835, pch = 15, cex = 1.5, col = "firebrick3")

# Add plot title and axis labels for clarity.
title(main = "Comparison of Portfolios A's and B's Profits with Illness",
      xlab = "Profit (or Loss) made on portfolio in money terms",
      ylab = "Density")

################ Inference Made from Simulation Run with Illness D ######################

# Calculate the percentage change in profitability for each portfolio due to increased lapse probability (q = 0.1).
(mean(xd) - mean(x)) / mean(x)  # Portfolio B: Percentage change in profitability
(mean(zd) - mean(z)) / mean(z)  # Portfolio A: Percentage change in profitability

# Evaluate whether losses or lower profits are occurring for both portfolios.
length(subset(zd, zd > 0)) / 1000  # Proportion of profitable outcomes for Portfolio A
length(subset(xd, xd > 0)) / 1000  # Proportion of profitable outcomes for Portfolio B
mean(zd)  # Average profit for Portfolio A
mean(xd)  # Average profit for Portfolio B

# Assess solvency risks for both portfolios under the revised experience factor (q = 0.1).
length(subset(zd, zd > 0)) / 1000  # Solvency likelihood for Portfolio A
length(subset(xd, xd > 0)) / 1000  # Solvency likelihood for Portfolio B
mean(xd) + mean(zd) - (mean(x) + mean(z))  # Net change in expected present value (EPV) of profits
mean(xd) + mean(zd)  # Total EPV of profits under illness D

# Explore the possibility of offsetting effects to maintain overall profitability and solvency.
# Combine profits and losses from both portfolios to stabilize financial outcomes.
quantile(zd, 0.975)  # 95% Confidence Interval (upper bound) for Portfolio A
quantile(xd, 0.975)  # 95% Confidence Interval (upper bound) for Portfolio B
quantile(zd, 0.025)  # 95% Confidence Interval (lower bound) for Portfolio A
quantile(xd, 0.025)  # 95% Confidence Interval (lower bound) for Portfolio B

# Confidence intervals for combined EPV of both portfolios.
quantile(xd, 0.975) + quantile(zd, 0.975)  # Upper bound
quantile(xd, 0.025) + quantile(zd, 0.025)  # Lower bound

# Calculate the probability of overall loss for combined portfolios.
quantile(xd, 0.021) + quantile(zd, 0.021)
