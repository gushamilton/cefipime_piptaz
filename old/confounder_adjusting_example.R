# Load necessary libraries
library(dplyr)
library(broom)
library(AER)  # For IV regression

# Set seed for reproducibility
set.seed(42)

# Simulation parameters
n <- 50000
p <- 0.5  # Probability of Z (Pip-Taz Shortage)
q <- 0.5  # Probability of U (Confounder)

# Generate data
Z <- rbinom(n, 1, p)  # Pip-Taz Shortage (Instrument)
U <- rbinom(n, 1, q)  # Confounder

# Generate X based on Z and U
X <- rbinom(n, 1, plogis(0.5 + 3 * Z + 2 * U))

# Generate Metronidazole (M) based on Z and U with strong effects
M <- rbinom(n, 1, plogis(0.5 + 2 * Z + 2 * U))

# Generate Mortality (Y) based on U only with strong effects, and no causal effect of X
Y <- rbinom(n, 1, plogis(0.5 + 2 * U))

# Create DataFrame
data <- data.frame(Z = Z, U = U, X = X, M = M, Y = Y)

# IV regression without adjusting for M
iv_model1 <- ivreg(Y ~ X | Z, data = data)
summary1 <- summary(iv_model1)

# IV regression adjusting for M
iv_model2 <- ivreg(Y ~ X + M | Z + M, data = data)
summary2 <- summary(iv_model2)

# Display the results
print("IV Regression without adjusting for M")
print(summary1)

print("IV Regression adjusting for M")
print(summary2)

