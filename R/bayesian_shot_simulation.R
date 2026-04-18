# Title: Sequential Bayesian Shot Updating
# Author: Jacob Bustamante
# Description:
# Simulates Bernoulli shots and animates sequential Bayesian updating
# with prior, likelihood, and posterior shown at each step.

set.seed(1014)

library(ggplot2)
library(gganimate)

dir.create("plots", showWarnings = FALSE)

# User defined parameters
N <- 20
p_true <- 0.70
alpha_prior <- 10
beta_prior <- 10

# Simulate shots
shots <- ifelse(rbinom(N, size = 1, prob = p_true) == 1, "goal", "miss")

# Parameter grid
theta <- seq(0, 1, length.out = 1000)

# Fixed prior
prior_density <- dbeta(theta, alpha_prior, beta_prior)

# Storage
plot_list <- vector("list", length = N + 1)

# Step 0: prior only, no data yet
plot_list[[1]] <- data.frame(
  theta = theta,
  prior = prior_density,
  likelihood = rep(NA, length(theta)),
  posterior = prior_density,
  step = 0,
  goals = 0,
  alpha = alpha_prior,
  beta = beta_prior,
  shot_result = "prior"
)

# Track cumulative goals
goals_so_far <- 0

for (i in 1:N) {
  if (shots[i] == "goal") {
    goals_so_far <- goals_so_far + 1
  }

  alpha_current <- alpha_prior + goals_so_far
  beta_current <- beta_prior + (i - goals_so_far)

  # Likelihood for first i shots
  likelihood_raw <- dbinom(goals_so_far, size = i, prob = theta)

  # Rescale likelihood to match prior height for plotting
  likelihood_scaled <- (likelihood_raw / max(likelihood_raw)) * max(prior_density)

  posterior_density <- dbeta(theta, alpha_current, beta_current)

  plot_list[[i + 1]] <- data.frame(
    theta = theta,
    prior = prior_density,
    likelihood = likelihood_scaled,
    posterior = posterior_density,
    step = i,
    goals = goals_so_far,
    alpha = alpha_current,
    beta = beta_current,
    shot_result = shots[i]
  )
}

plot_df <- do.call(rbind, plot_list)

plot_df$frame_label <- ifelse(
  plot_df$step == 0,
  paste0(
    "Step 0: Prior only",
    "\nBeta(", plot_df$alpha, ", ", plot_df$beta, ")"
  ),
  paste0(
    "Step ", plot_df$step,
    " | Result: ", plot_df$shot_result,
    " | Goals: ", plot_df$goals, "/", plot_df$step,
    "\nPrior: Beta(", alpha_prior, ", ", beta_prior, ")",
    " | Posterior: Beta(", plot_df$alpha, ", ", plot_df$beta, ")"
  )
)

p_anim <- ggplot(plot_df, aes(x = theta)) +
  geom_area(aes(y = prior, fill = "Prior"), alpha = 0.30) +
  geom_area(aes(y = likelihood, fill = "Likelihood"), alpha = 0.30, na.rm = TRUE) +
  geom_area(aes(y = posterior, fill = "Posterior"), alpha = 0.30) +
  scale_fill_manual(values = c(
    "Prior" = "#4A90E2",
    "Likelihood" = "#E74C3C",
    "Posterior" = "#8E44AD"
  )) +
  labs(
    title = "Sequential Bayesian Updating",
    subtitle = "{closest_state}",
    x = "theta",
    y = "Density / Rescaled Likelihood",
    fill = NULL,
    caption = "Likelihood is rescaled in each frame for visual comparison"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) +
  transition_states(
    states = frame_label,
    transition_length = 2,
    state_length = 2,
    wrap = FALSE
  ) +
  ease_aes("cubic-in-out")

animate(
  p_anim,
  nframes = 140,
  fps = 7,
  width = 900,
  height = 550,
  renderer = gifski_renderer("plots/posterior_evolution_with_prior_likelihood.gif")
)