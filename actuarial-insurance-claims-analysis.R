# Compound Poisson Distribution: Calculating probabilities P(S = r) for r ≤ 200.

options(scipen = 999)  # Display numbers in decimal format instead of scientific notation.

size = 201  # Define the maximum number of claims (size of the vector).
fsubxj <- c(length = size)  # Initialize a vector to represent the probabilities of claim sizes.
lambda = 12  # Average number of claims (Poisson parameter).

# Initialize all probabilities of claim sizes to 0.
for (j in 1:size) {
  fsubxj[j] = 0
}

# Assign probabilities to specific claim sizes (RV X) given a claim has occurred.
fsubxj[1] <- 0.2  # Probability of a claim size of 1
fsubxj[5] <- 0.3  # Probability of a claim size of 5
fsubxj[10] <- 0.5  # Probability of a claim size of 10

# Calculate probabilities P(S = r) using a recurrence relation.
fsubsr <- c()  # Initialize the vector to store P(S = r) values.
fsubsr[1] <- exp(-lambda)  # P(S = 0) = P(N = 0) for the Poisson process.

# Loop through all possible values of r to calculate P(S = r).
for (r in 2:size) {
  total_recursion <- 0  # Variable to hold the sum for P(S = r).
  
  # Inner loop for the recurrence formula summation.
  for (j in 2:r) {
    recursion <- (lambda * ((j - 1) / (r - 1))) * fsubxj[j - 1] * fsubsr[r - j + 1]
    total_recursion <- total_recursion + recursion  # Accumulate the sum for P(S = r).
  }
  
  fsubsr[r] <- total_recursion  # Store the calculated P(S = r) for this value of r.
}

# Output the vector representing P(S = r) for r ≤ 200.
fsubsr

# Calculate mode, median, and percentiles for the discrete distribution represented by `fsubsr`.

# Mode of the discrete distribution:
index <- which.max(fsubsr)  # Identify the index of the maximum probability in `fsubsr`.
mode <- (index - 1)  # Adjust the index to get the corresponding value of `r`.
print(mode)  # The mode of the distribution is 77.
S <- seq(0, 200, by = 1)  # Sequence representing possible values of monthly aggregate claims.
S[index]  # Verify the mode by extracting the value directly from the sequence `S`.

# Create a cumulative distribution function (CDF) for P(S ≤ r), bounded by r ≤ 200.
CDF <- c()  # Initialize the CDF vector.
for (r in 1:size) {
  total_cdf <- 0  # Sum probabilities up to `r`.
  for (j in 1:r) {
    total_cdf <- total_cdf + fsubsr[j]
  }
  CDF[r] <- total_cdf  # Store the cumulative probability for `r`.
}
# Note: Alternatively, `cumsum(fsubsr)` achieves the same result as the loop above.

# Median of the distribution:
which(CDF < 0.5)  # Find the last index where CDF is less than 0.5.
CDF[79]  # CDF value just below 0.5.
CDF[80]  # CDF value at or above 0.5, indicating the median lies here.
fsubsr[80] + (1 - CDF[80])  # Verify P(S ≥ s) for the median value.
# Median value is determined to be r = 79.

# 95th percentile of the distribution:
which(CDF < 0.95)  # Find the last index where CDF is less than 0.95.
CDF[126]  # CDF value just below 0.95.
CDF[127]  # CDF value at or above 0.95, indicating the 95th percentile lies here.
fsubsr[127] + (1 - CDF[127])  # Verify P(S ≥ s) for the 95th percentile.
# 95th percentile value is determined to be r = 126.

# 99th percentile of the distribution:
which(CDF < 0.99)  # Find the last index where CDF is less than 0.99.
CDF[148]  # CDF value just below 0.99.
CDF[149]  # CDF value at or above 0.99, indicating the 99th percentile lies here.
fsubsr[149] + (1 - CDF[149])  # Verify P(S ≥ s) for the 99th percentile.
# 99th percentile value is determined to be r = 148.



# Calculating Mean and Variance of S for a Compound Poisson Distribution

# Analytical Calculation of Mean and Variance
x <- c(1, 5, 10)  # Possible claim sizes
probx <- c(0.2, 0.3, 0.5)  # Probabilities associated with claim sizes
EX <- crossprod(x, probx)  # Expected value (mean) of claim size, E[X]
lambda <- 12  # Poisson distribution parameter (average number of claims)

# Calculate the mean of S using E[S] = λ * E[X].
ES <- lambda * EX
print(ES)  # Mean of S

# Calculate the variance of S using Var(S) = λ * E[X^2].
EX2 <- crossprod(x^2, probx)  # Second moment of X, E[X^2]
VarS <- lambda * EX2  # Variance of S
print(VarS)  # Variance of S

# Numerical Calculation of Mean and Variance Using Recurrence Relation
size = 351  # Define the size large enough to ensure the full distribution is captured.
fsubxj <- c(length = size)  # Initialize vector to represent claim size probabilities.
lambda = 12  # Poisson parameter

# Initialize all claim size probabilities to 0.
for (j in 1:size) {
  fsubxj[j] = 0
}

# Assign probabilities to specific claim sizes.
fsubxj[1] <- 0.2  # Probability of claim size 1
fsubxj[5] <- 0.3  # Probability of claim size 5
fsubxj[10] <- 0.5  # Probability of claim size 10

# Use recurrence relation to calculate P(S = r) for all r.
S.density <- c()  # Initialize vector to store P(S = r) values.
S.density[1] <- exp(-12)  # P(S = 0) = P(N = 0) for the Poisson process.

# Loop through all possible values of r to calculate P(S = r).
for (r in 2:size) {
  total_recursion <- 0  # Variable to accumulate probabilities for P(S = r).
  
  # Inner loop for summing contributions to P(S = r) using the recurrence relation.
  for (j in 2:r) {
    recursion <- (lambda * ((j - 1) / (r - 1))) * fsubxj[j - 1] * S.density[r - j + 1]
    total_recursion <- total_recursion + recursion
  }
  S.density[r] <- total_recursion  # Store the calculated P(S = r) for this value of r.
}

# Compute the cumulative distribution function (CDF) for S.
S.distr <- cumsum(S.density)

# Calculate the mean of S using the weighted sum of probabilities.
ES <- sum(S.density * 0:350)
print(ES)  # Mean of S

# Calculate the variance of S using the second moment formula: Var(S) = E[S^2] - E[S]^2.
VARS <- sum(S.density * (0:350)^2) - ES^2
print(VARS)  # Variance of S

