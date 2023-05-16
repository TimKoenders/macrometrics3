### Macroeconometrics Exercise 2
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(bayesplot)
library(ggpubr)
setwd("C:/Users/miria/Desktop/SoSe2023/Macroeconometrics/3rd assignment")

D <- read.csv("UNRATE.csv")
names(D) <- c("Date", "u")

# demean u
D$u <- D$u - mean(D$u)

# compute lag(u) and exclude first row
D <- D %>%
  mutate(u_lag = lag(u))

D <- D[-1,]

################# Normal Prior for AR(1) ####################
#############################################################

######## Prior: N(0,1)
pri_s1 <- rnorm(10000, mean = 0, sd = 1)

# plot
pri1 <- data.frame(x = pri_s1) %>%
  ggplot(aes(x = x))+
  geom_density() +
  xlab(expression("u"[-1]))+
  ggtitle("Prior N(0,1)")+
  theme_bw()

# run regression
mod <- stan_glm(u ~ u_lag-1, data = D,
                family = gaussian(link = "identity"),
                prior = normal(location = 0, scale = 1))

# plot posterior
post1 <- mcmc_dens(mod, pars = c("u_lag")) + 
  xlab(expression("u"[-1]))+
  ggtitle("Posterior N(0,1)")+
  theme_bw()

######## Prior: N(1,1)
pri_s2 <- rnorm(10000, mean = 1, sd = 1)

# plot
pri2 <- data.frame(x = pri_s2) %>%
  ggplot(aes(x = x))+
  geom_density() +
  xlab(expression("u"[-1]))+
  ggtitle("Prior N(1,1)")+
  theme_bw()

# run regression
mod <- stan_glm(u ~ u_lag-1, data = D,
                family = gaussian(link = "identity"),
                prior = normal(location = 1, scale = 1))

# plot posterior
post2 <- mcmc_dens(mod, pars = c("u_lag")) + 
  xlab(expression("u"[-1]))+
  ggtitle("Posterior N(1,1)")+
  theme_bw()

ggarrange(pri1, post1, pri2, post2)

################# Beta Prior for AR(1) ######################
#############################################################


## Function to calculate log-likelihood of autoregressive model
ar_log_likelihood <- function(phi, u, u_lag) {
  residuals <- u - phi * u_lag
  log_likelihood <- -0.5 * sum(log(2 * pi) + residuals^2)
  
  return(log_likelihood)
}

# Beta priors
priors <- list(
  list(shape1 = 0.1, shape2 = 0.1, name = "B(0.1, 0.1)"),
  list(shape1 = 1, shape2 = 1, name = "B(1, 1)"),
  list(shape1 = 10, shape2 = 10, name = "B(10, 10)")
)

prior_plots <- list()
posterior_plots <- list()

for (prior in priors) {
  shape1 <- prior$shape1
  shape2 <- prior$shape2
  prior_name <- prior$name
  
  # Generate random samples from beta distribution
  prior_samples <- rbeta(10000, shape1, shape2)
  
  # Plot prior distribution
  prior_plot <- ggplot(data.frame(x = prior_samples), aes(x = x)) +
    geom_density(fill = "lightblue", color = "blue") +
    labs(x = expression("u"[~-1]), title = paste("Prior ", prior_name)) +
    theme_bw()
  
  # Store prior plot
  prior_plots[[prior_name]] <- prior_plot
  
  # Estimate parameters using maximum likelihood estimation
  mle_phi <- cor(D$u[-1], D$u_lag[-1])
  
  # Calculate log-likelihood
  log_likelihood <- ar_log_likelihood(mle_phi, D$u[-1], D$u_lag[-1])
  
  # Plot posterior distribution
  posterior_samples <- rbeta(10000, sum(D$u_lag[-1]) + shape1, length(D$u_lag[-1]) + shape2)
  posterior_plot <- ggplot(data.frame(x = posterior_samples), aes(x = x)) +
    geom_density(fill = "lightblue", color = "blue") +
    labs(x = expression("u"[~-1]), title = paste("Posterior ", prior_name)) +
    theme_bw()
  
  # Store posterior plot
  posterior_plots[[prior_name]] <- posterior_plot
  
  # Print the results
  cat("Prior:", prior_name, "\n")
  cat("Estimated phi:", mle_phi, "\n")
  cat("Log-likelihood:", log_likelihood, "\n\n")
}

# Arrange and display the plots
prior_arrange <- do.call(gridExtra::grid.arrange, prior_plots)
posterior_arrange <- do.call(gridExtra::grid.arrange, posterior_plots)

# Display the plots
gridExtra::grid.arrange(prior_arrange, posterior_arrange, ncol = 2)