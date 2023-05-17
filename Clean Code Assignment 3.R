##Question 1##
#1
library("broom")
library("sandwich")
library("stargazer")
library("brms")
data <- read.csv("rugged_data.csv")
data$log_rgdppc_2000 <- log(data$rgdppc_2000)
model1 <- lm(data$log_rgdppc_2000 ~ (data$rugged + data$dist_coast) * data$cont_africa)
tidy(model1)
se <- sqrt(diag(vcovHC(model1, type = "HC1")))
stargazer(model1, 
          type = "html", 
          title = "Regression Results", 
          align = TRUE, 
          font.size = "small", 
          out = "regression_table.html")
#2
library("rstudioapi")
model2 <- brm(
  log_rgdppc_2000 ~ (rugged + dist_coast) * cont_africa,
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(gamma(0.5, 0.5), class = "sigma")
  ),
  control = list(adapt_delta = 0.99),
  cores = 4,
  chains = 4
)
posterior_model2 <- posterior_samples(model2)
tidy(posterior_model2)
library("ggplot2")
ggplot(data = posterior_model2, aes(x = b_rugged, y = `b_rugged:cont_africa`)) + 
  geom_point(alpha = 0.5, size = 0.8, color = "darkblue") +
  xlab("Ruggedness Effect in Rest of World") + ylab("Ruggedness Effect in Africa") +
  ggtitle("Scatterplot of Ruggedness Effects") +
  theme_bw()

#3
model3a <- brm(
  log_rgdppc_2000 ~ rugged * cont_africa,
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(gamma(0.5, 0.5), class = "sigma")
  ),
  control = list(adapt_delta = 0.99),
  cores = 4,
  chains = 4
)
posterior_model3a <- posterior_samples(model3a)
summary(posterior_model3a)

data$log_pop_1400 <- log(data$pop_1400 + 1)
model3b <- brm(
  log_rgdppc_2000 ~ rugged + log_pop_1400 + rugged:cont_africa,
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(gamma(0.5, 0.5), class = "sigma")
  ),
  control = list(adapt_delta = 0.99),
  cores = 4,
  chains = 4
  )


chooseCRANmirror(graphics=FALSE, ind=1) 

posterior_model3b <- posterior_samples(model3b)
tidy(posterior_model3b)

model3c <- brm(
  log_rgdppc_2000 ~ (rugged + dist_coast) * cont_africa + log_pop_1400,
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(gamma(0.5, 0.5), class = "sigma")
  ),
  control = list(adapt_delta = 0.99),
  cores = 4,
  chains = 4
)
posterior_model3c <- posterior_samples(model3c)
tidy(posterior_model3c)

#4
install.packages("loo")
library("loo")
waic_model2 <- waic(as.matrix(posterior_model2))
waic_model3a <- waic(as.matrix(posterior_model3a))
waic_model3b <- waic(as.matrix(posterior_model3b))
waic_model3c <- waic(as.matrix(posterior_model3c))
waic_values <- c(waic_model2$waic, waic_model3a$waic, waic_model3b$waic, waic_model3c$waic)
print(waic_values)

#5
library("brms")
model3c_prior_new_1 <- c(
  prior(normal(0, 10), class = "b"), 
  prior(gamma(0.5, 0.5), class = "sigma") 
)
model3c_new_1 <- update(model3c, prior = model3c_prior_new_1)
posterior_model3c_new_1 <- posterior_samples(model3c_new_1)

model3c_prior_new_2 <- c(
  prior(normal(1, 1), class = "b"), 
  prior(gamma(0.5, 0.5), class = "sigma") 
)
model3c_new_2 <- update(model3c, prior = model3c_prior_new_2)
posterior_model3c_new_2 <- posterior_samples(model3c_new_2)

library("broom.mixed")
tidy(posterior_model3c)
tidy(posterior_model3c_new_1)
tidy(posterior_model3c_new_2)

library(brms)

# Fit the models for each prior variance
model3c_a<- brm(
  log_rgdppc_2000 ~ (rugged + dist_coast) * cont_africa + log_pop_1400,
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(0, sqrt(0.0001)), class = "b"),
    prior(gamma(0.5, 0.5), class = "sigma")
  ),
  save_pars= save_pars("all=TRUE"),
  sample_prior ="yes",
  control = list(adapt_delta = 0.99),
  cores = 4,
  chains = 4
)

model3c_b <- brm(
  log_rgdppc_2000 ~ (rugged + dist_coast) * cont_africa + log_pop_1400,
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(0, sqrt(0.01)), class = "b"),
    prior(gamma(0.5, 0.5), class = "sigma")),
  save_pars= save_pars("all=TRUE"),
  sample_prior ="yes",
  control = list(adapt_delta = 0.99),
  cores = 4,
  chains = 4
)

model3c_c <- brm(
  log_rgdppc_2000 ~ (rugged + dist_coast) * cont_africa + log_pop_1400,
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(0, sqrt(1)), class = "b"),
    prior(gamma(0.5, 0.5), class = "sigma" )),
  save_pars= save_pars("all=TRUE"),
  sample_prior ="yes",
  control = list(adapt_delta = 0.99),
  cores = 4,
  chains = 4
)

model3c_d <- brm(
  log_rgdppc_2000 ~ (rugged + dist_coast) * cont_africa + log_pop_1400,
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(0, sqrt(100)), class = "b"),
    prior(gamma(0.5, 0.5), class = "sigma" )),
  save_pars= save_pars("all=TRUE"),
  sample_prior ="yes",
  control = list(adapt_delta = 0.99),
  cores = 4,
  chains = 4
)

library("brms")
library("loo")
library("bridgesampling")

# Compute the marginal likelihood and lo for each model

ml_model3c <- bridge_sampler(model3c, thin = 100)$logml
ml_model3c_a <- bridge_sampler(model3c, thin = 100)$logml
ml_model3c_b <- bridge_sampler(model3b, thin = 100)$logml
ml_model3c_c <- bridge_sampler(model3c, thin = 100)$logml
ml_model3c_d <- bridge_sampler(model3d, thin = 100)$logml

# Instead, using the Loo

library("loo")
loo_model3c_a <- loo(model3c_a)
estimates_model3c_a <- c(loo_model3c_a$looic, loo_model3c_a$elpd_loo, loo_model3c_a$p_loo)
loo_model3c_b <- loo(model3c_b)
estimates_model3c_b <- c(loo_model3c_b$looic, loo_model3c_b$elpd_loo, loo_model3c_b$p_loo)
loo_model3c_c <- loo(model3c_c)
estimates_model3c_c <- c(loo_model3c_c$looic, loo_model3c_c$elpd_loo, loo_model3c_c$p_loo)
loo_model3c_d <- loo(model3c_d)
estimates_model3c_d <- c(loo_model3c_d$looic, loo_model3c_d$elpd_loo, loo_model3c_d$p_loo)
print(estimates_model3c_a)
print(estimates_model3c_b)
print(estimates_model3c_c)
print(estimates_model3c_d)

# 
loo_model3a <- loo(model3a)
estimates_model3a <- c(loo_model3a$looic, loo_model3a$elpd_loo, loo_model3a$p_loo)
loo_model3b <- loo(model3b)
estimates_model3b <- c(loo_model3b$looic, loo_model3b$elpd_loo, loo_model3b$p_loo)
loo_model3c<- loo(model3c)
estimates_model3c <- c(loo_model3c$looic, loo_model3c$elpd_loo, loo_model3c$p_loo)

approx.bayesfactor_model3a_andmodel3b<- loo_model3a$looic/loo_model3b$looic
print(approx.bayesfactor_model3a_andmodel3b)

approx.bayesfactor_model3a_andmodel3c <- loo_model3a$looic/loo_model3c$looic
print(approx.bayesfactor_model3a_andmodel3c)

approx.bayesfactor_model3b_andmodel3c <- loo_model3b$looic/loo_model3c$looic
print(approx.bayesfactor_model3b_andmodel3c)


#8
library("brms")
predictions <- posterior_predict(model3c)
subset_data <- data[1:100, ] 
simulated_data <- posterior_linpred(model3c, newdata = subset_data)
install.packages("Hmisc")
library("Hmisc")
ggplot(data = subset_data, aes(x = rugged, y = log_rgdppc_2000)) +
  geom_point() +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 2), geom = "ribbon", alpha = 0.2, color = "red", aes(x = rugged, y = log_rgdppc_2000)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 2), color = "red", size = 1, aes(x = rugged, y = log_rgdppc_2000)) +
  labs(title = "Predictive Uncertainty", x = "rugged", y = "log_rgdppc_2000")

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
  
  # MCMC algorithm for estimating the posterior
  n_iterations <- 10000
  n_burn <- 1000
  phi_samples <- numeric(n_iterations)
  
  # Initial value for phi
  phi_samples[1] <- runif(1)
  
  for (i in 2:n_iterations) {
    # Sample from proposal distribution (normal)
    phi_star <- rnorm(1, mean = phi_samples[i-1], sd = 0.05)
    
    # Compute acceptance ratio
    acceptance_ratio <- exp(ar_log_likelihood(phi_star, D$u[-1], D$u_lag[-1]) -
                              ar_log_likelihood(phi_samples[i-1], D$u[-1], D$u_lag[-1]))
    
    # Accept or reject the sample
    if (runif(1) < acceptance_ratio) {
      phi_samples[i] <- phi_star
    } else {
      phi_samples[i] <- phi_samples[i-1]
    }
  }
  
  # Discard burn-in samples
  phi_samples <- phi_samples[-(1:n_burn)]
  
  # Plot posterior distribution
  posterior_plot <- ggplot(data.frame(x = phi_samples), aes(x = x)) +
    geom_density(fill = "lightblue", color = "blue") +
    labs(x = expression("u"[~-1]), title = paste("Posterior ", prior_name)) +
    theme_bw()
  
  # Store posterior plot
  posterior_plots[[prior_name]] <- posterior_plot
  
  # Print the results
  cat("Prior:", prior_name, "\n")
  cat("Estimated phi:", mean(phi_samples), "\n")
  cat("Log-likelihood:", log_likelihood, "\n\n")
}

# Arrange and display the plots
prior_arrange <- do.call(gridExtra::grid.arrange, prior_plots)
posterior_arrange <- do.call(gridExtra::grid.arrange, posterior_plots)

# Display the plots
gridExtra::grid.arrange(prior_arrange, posterior_arrange, ncol = 2)