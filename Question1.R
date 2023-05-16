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



