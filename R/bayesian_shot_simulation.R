# Title: Bayesian Shot Simulation
# Author: Jacob Bustamante
# Description:
# Simulates Bernoulli shots and visualizes prior, likelihood, and posterior.

set.seed(1014)

library(ggplot2)

N <- 20

# Simulate N Bernoulli trials with success probability 0.7
shots <- ifelse(rbinom(N, size = 1, prob = 0.7) == 1, "goal", "miss")

# Goals count
nGoal <- sum(shots == "goal")
theta <- seq(0, 1, length.out = 1000)

# beta prior centered at 0.5
prior <- dbeta(theta, 10, 10)

# Binomial likelihood as a function of theta
likelihood <- dbinom(nGoal, size = N, prob = theta)

posterior <- dbeta(theta, 10 + nGoal, 10 + (N - nGoal))

# Scale likelihood and posterior for visual comparison
prior_plot <- prior
likelihood_plot <- (likelihood / max(likelihood)) * max(prior)
posterior_plot <- posterior * max(prior) / max(posterior)

df <- data.frame(
    theta = theta,
    prior = prior_plot,
    likelihood = likelihood_plot,
    posterior = posterior_plot
)

prior_x <- theta[which.max(prior_plot)]
prior_y <- max(prior_plot)

lik_x <- theta[which.max(likelihood_plot)]
lik_y <- max(likelihood_plot)

post_x <- theta[which.max(posterior_plot)]
post_y <- max(posterior_plot)

p <- ggplot(df, aes(x = theta)) +
    geom_area(aes(y = prior, fill = "Prior"), alpha = 0.35) +  
    geom_area(aes(y = likelihood, fill = "Likelihood"), alpha = 0.35) +
    geom_area(aes(y = posterior, fill = "Posterior"), alpha = 0.35) +
    scale_fill_manual(values = c(
        "Prior" = "#4A90E2",
        "Likelihood" = "#E74C3C",
        "Posterior" = "#8E44AD"
    )) +
    labs(
        fill = NULL,
        title = "Prior, Likelihood, and Posterior",
        subtitle = "Beta prior with binomial data",
        x = "theta",
        y = NULL,
        caption = "Likelihood is rescaled for visual comparison"
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
        panel.grid.minor = element_blank()
    )  +
  annotate("text", x = prior_x, y = prior_y * 1.05,
           label = "Prior", color = "#4A90E2", fontface = "bold") +
  annotate("text", x = lik_x, y = lik_y * 1.05,
           label = "Likelihood", color = "#E74C3C", fontface = "bold") +
  annotate("text", x = post_x, y = post_y * 1.05,
           label = "Posterior", color = "#8E44AD", fontface = "bold")

print(sprintf("For %d shots, we had %d goals.", N, nGoal))
posterior_mean <- (10 + nGoal) / (20 + N)
mle <- nGoal / N
print(sprintf("Posterior mean estimate of theta: %.3f, with the MLE of theta: %.3f", posterior_mean, mle))

ggsave("plots/prior_likelihood_posterior.png", plot = p, width = 8, height = 5, dpi = 300)
