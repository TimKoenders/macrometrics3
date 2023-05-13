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

#6
library("rstanarm")
library("bridgesampling")
model_prior_1 <- prior(normal(0, 1), class = "b")
model_prior_2 <- prior(normal(0, 10), class = "b")
model_prior_3 <- prior(normal(0, 100), class = "b")
marg_likelihood_1 <- bridge_sampler(model3c, prior = model_prior_1)$logml
marg_likelihood_2 <- bridge_sampler(model3c, prior = model_prior_2)$logml
marg_likelihood_3 <- bridge_sampler(model3c, prior = model_prior_3)$logml
marg_likelihood_4 <- bridge_sampler(model3c, prior = model_prior_4)$logml
print(c(marg_likelihood_1,marg_likelihood_2, marg_likelihood_3, marg_likelihood_4))

#7

marg_likelihood_3a <- bridge_sampler(posterior_model_3a)$logml
marg_likelihood_3b <- bridge_sampler(posterior_model_3b)$logml
marg_likelihood_3c <- bridge_sampler(posterior_model_3c)$logml
bayes_factor_3a_3b <- marg_likelihood_3a / marg_likelihood_3b
bayes_factor_3a_3c <- marg_likelihood_3a / marg_likelihood_3c
bayes_factor_3b_3c <- marg_likelihood_3b / marg_likelihood_3c
print(c(bayes_factor_3a_3b, bayes_factor_3a_3c, bayes_factor_3b_3c))

#8

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



