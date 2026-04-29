# Bayes factor calibration
fit_model_H0 <- function(dat) {
  brm(
    formula = y ~ 1 + (1 | participant),
    data = dat,
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.4), class = "Intercept"),
      prior(exponential(1), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    iter = 2000,
    warmup = 1000,
    chains = 2,
    cores = 2,
    refresh = 0,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  )
}

fit_model_H1 <- function(dat) {
  brm(
    formula = y ~ condition + (1 | participant),
    data = dat,
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.4), class = "b"),
      prior(normal(0, 0.4), class = "Intercept"),
      prior(exponential(1), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    iter = 2000,
    warmup = 1000,
    chains = 2,
    cores = 2,
    refresh = 0,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  )
}


fit_model_H0 <- function(dat) {
  brm(
    formula = y ~ 1 + (1 | participant),
    data = dat,
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.4), class = "Intercept"),
      prior(exponential(1), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    iter = 2000,
    warmup = 1000,
    chains = 2,
    cores = 2,
    refresh = 0,
    save_pars = save_pars(all = TRUE),
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  )
}

fit_model_H1 <- function(dat) {
  brm(
    formula = y ~ condition + (1 | participant),
    data = dat,
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.4), class = "b"),
      prior(normal(0, 0.4), class = "Intercept"),
      prior(exponential(1), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    iter = 2000,
    warmup = 1000,
    chains = 2,
    cores = 2,
    refresh = 0,
    save_pars = save_pars(all = TRUE),
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  )
}

run_one_bf_sim <- function(n_participants = 25) {
  
  # 一半概率 H0 为真，一半概率 H1 为真
  true_model <- sample(c("H0", "H1"), size = 1)
  
  # H0: treatment_effect = 0
  # H1: treatment_effect = 0.30
  true_effect <- ifelse(true_model == "H0", 0, 0.30)
  
  dat <- simulate_paper_like_data(
    n_participants = n_participants,
    typical_improvement = 0.44,
    treatment_effect = true_effect,
    heterogeneity_sd = 0.15,
    measurement_error_sd = 0.20
  )
  
  fit0 <- fit_model_H0(dat)
  fit1 <- fit_model_H1(dat)
  
  bf10 <- bayes_factor(fit1, fit0)$bf
  
  # posterior probability of H1 assuming equal priors
  post_H1 <- bf10 / (1 + bf10)
  post_H0 <- 1 / (1 + bf10)
  
  tibble(
    true_model = true_model,
    bf10 = bf10,
    post_H1 = post_H1,
    post_H0 = post_H0,
    supports_H1 = post_H1 > 0.5,
    supports_H0 = post_H0 > 0.5
  )
}

set.seed(123)
one_test <- run_one_bf_sim(20)
one_test

run_bf_calibration <- function(n_participants = 25, n_sim = 10) {
  
  all_res <- map_dfr(1:n_sim, ~run_one_bf_sim(n_participants))
  
  tibble(
    N = n_participants,
    avg_posterior_H1 = mean(all_res$post_H1),
    pct_support_H1_when_H1_true = mean(all_res$supports_H1[all_res$true_model == "H1"]),
    pct_support_H0_when_H0_true = mean(all_res$supports_H0[all_res$true_model == "H0"])
  )
}
set.seed(123)

bf20 <- run_bf_calibration(20, n_sim = 5)
bf25 <- run_bf_calibration(25, n_sim = 5)
bf30 <- run_bf_calibration(30, n_sim = 5)

bf_results <- bind_rows(bf20, bf25, bf30)
bf_results

