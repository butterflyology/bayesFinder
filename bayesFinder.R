# Bayesian method for finding things
# Chris Hamm
# 2023-08-24 - Initial commit


# I swear I read somewhere that Bayesian stats were used to find a missing nuclear bomb or some sunken ship. I just wanted to write some code to see what that would entail. 

# Preliminaries ----
library(ggplot2)


# Data ----
# Simulate underwater grid
grid_size <- 100
x <- rep(1:grid_size, each = grid_size)
y <- rep(1:grid_size, times = grid_size)

# Simulate sonar data strength
true_wreck_location <- c(70, 70)  # x, y coordinates of the wreck

# Simulate underwater terrain effect
underwater_terrain <- dnorm(sqrt((x - true_wreck_location[1])^2 + (y - true_wreck_location[2])^2), mean = 0, sd = 5)

# Simulate random noise
noise <- rnorm(grid_size^2, mean = 0, sd = 0.05)

# Calculate simulated signal strength
simulated_signal_strength <- 2 / (1 + underwater_terrain) + noise

# Plot ----
# Plot the simulated sonar data
simulated_data <- data.frame(x = x, y = y, signal_strength = simulated_signal_strength)

ggplot(simulated_data, aes(x, y, fill = signal_strength)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Simulated Sonar Data", x = "X Coordinate", y = "Y Coordinate", fill = "Signal Strength")



# Model ----
# Simulate random noise
noise <- rnorm(grid_size^2, mean = 0, sd = 0.1)

# Calculate signal strength
signal_strength <- 1 / (1 + underwater_terrain) + noise

# Create a simple Gaussian likelihood function
likelihood <- function(observed, predicted, noise_sd) {
  exp(-0.5 * ((observed - predicted) / noise_sd)^2) / (noise_sd * sqrt(2 * pi))
}

# Set prior belief
prior_belief <- matrix(1, nrow = grid_size, ncol = grid_size) / (grid_size^2)

# Initialize posterior belief
posterior_belief <- prior_belief

# Standard deviation for the likelihood function
noise_sd <- 0.1

# Update the posterior belief using Bayes' theorem
for (i in 1:length(simulated_signal_strength)) {
  likelihood_values <- likelihood(simulated_signal_strength[i], simulated_signal_strength, noise_sd)
  posterior_belief <- posterior_belief * likelihood_values
}

# Normalize the posterior belief
posterior_belief <- posterior_belief / sum(posterior_belief)

# Reshape the posterior belief matrix into a vector
posterior_vector <- as.vector(posterior_belief)

# Create a data frame for plotting
grid_data <- data.frame(x = rep(1:grid_size, each = grid_size), y = rep(1:grid_size, times = grid_size))
grid_data$posterior_belief <- posterior_vector


ggplot(grid_data, aes(x, y, fill = posterior_belief)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FAFAFA")) +
  labs(title = "Simulated Sonar Data and Posterior Belief", x = "X Coordinate", y = "Y Coordinate", fill = "Posterior Belief")
