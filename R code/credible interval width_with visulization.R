install.packages(c("tidyverse", "brms"))
library(tidyverse)
library(brms)

simulate_paper_like_data <- function(
    n_participants = 25,
    typical_improvement = 0.44,
    treatment_effect = 0.30,
    heterogeneity_sd = 0.15,
    measurement_error_sd = 0.20
) {
  
  # 每个 participant 的个体差异
  participant_intercept <- rnorm(
    n_participants,
    mean = 0,
    sd = heterogeneity_sd
  )
  
  # 每人两个条件：fROM 和 pROM
  dat <- expand.grid(
    participant = factor(1:n_participants),
    condition = c("fROM", "pROM")
  ) %>%
    arrange(participant)
  
  # 给每个人复制各自的随机截距
  dat$participant_intercept <- rep(participant_intercept, each = 2)
  
  # 设定两种条件的真实效应
  dat$true_effect <- ifelse(
    dat$condition == "fROM",
    typical_improvement,
    typical_improvement + treatment_effect
  )
  
  # 生成观测值 = 个体差异 + 条件效应 + 测量误差
  dat$y <- dat$participant_intercept +
    dat$true_effect +
    rnorm(nrow(dat), mean = 0, sd = measurement_error_sd)
  
  dat
}
dat25 <- simulate_paper_like_data(n_participants = 25)
head(dat25)
fit_bayes_model <- function(dat) {
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
    iter = 4000,
    warmup = 2000,
    chains = 4,
    cores = 4,
    refresh = 0,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  )
}
fit25 <- fit_bayes_model(dat25)
summary(fit25)

get_ci_width <- function(fit) {
  post <- posterior_summary(fit)
  
  lower <- post["b_conditionpROM", "Q2.5"]
  upper <- post["b_conditionpROM", "Q97.5"]
  
  width <- upper - lower
  return(width)
}
get_ci_width(fit25)

run_paper_like_simulation <- function(
    n_participants,
    n_sim = 10,
    typical_improvement = 0.44,
    treatment_effect = 0.30,
    heterogeneity_sd = 0.15,
    measurement_error_sd = 0.20
) {
  
  widths <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    dat <- simulate_paper_like_data(
      n_participants = n_participants,
      typical_improvement = typical_improvement,
      treatment_effect = treatment_effect,
      heterogeneity_sd = heterogeneity_sd,
      measurement_error_sd = measurement_error_sd
    )
    
    fit <- fit_bayes_model(dat)
    widths[i] <- get_ci_width(fit)
  }
  
  tibble(
    N = n_participants,
    mean_CrI_width = mean(widths),
    lower_95 = quantile(widths, 0.025),
    upper_95 = quantile(widths, 0.975)
  )
}
set.seed(123)

res20 <- run_paper_like_simulation(20, n_sim = 10)
res25 <- run_paper_like_simulation(25, n_sim = 10)
res30 <- run_paper_like_simulation(30, n_sim = 10)

results <- bind_rows(res20, res25, res30)
results

# -----------------------------------------
# Store CrI width from each simulation run
# -----------------------------------------

run_paper_like_simulation_details <- function(
    n_participants,
    n_sim = 10,
    typical_improvement = 0.44,
    treatment_effect = 0.30,
    heterogeneity_sd = 0.15,
    measurement_error_sd = 0.20
) {
  
  widths <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    
    dat <- simulate_paper_like_data(
      n_participants = n_participants,
      typical_improvement = typical_improvement,
      treatment_effect = treatment_effect,
      heterogeneity_sd = heterogeneity_sd,
      measurement_error_sd = measurement_error_sd
    )
    
    fit <- fit_bayes_model(dat)
    widths[i] <- get_ci_width(fit)
  }
  
  tibble(
    N = n_participants,
    run = 1:n_sim,
    CrI_width = widths
  )
}

set.seed(261)

details20 <- run_paper_like_simulation_details(20, n_sim = 10)
details25 <- run_paper_like_simulation_details(25, n_sim = 10)
details30 <- run_paper_like_simulation_details(30, n_sim = 10)

details_all <- bind_rows(details20, details25, details30)

details_all

summary_details <- details_all %>%
  group_by(N) %>%
  summarise(
    mean_CrI_width = mean(CrI_width),
    lower_95 = quantile(CrI_width, 0.025),
    upper_95 = quantile(CrI_width, 0.975),
    min_width = min(CrI_width),
    max_width = max(CrI_width),
    .groups = "drop"
  )

summary_details

library(ggplot2)

ggplot(details_all, aes(x = factor(N), y = CrI_width)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 5,
    shape = 18
  ) +
  labs(
    x = "Sample Size N",
    y = "95% Credible Interval Width",
    title = "CrI Width Across 10 Simulation Runs"
  ) +
  theme_minimal(base_size = 14)

ggplot(summary_details, aes(x = factor(N), y = mean_CrI_width)) +
  geom_point(size = 4) +
  geom_errorbar(
    aes(ymin = lower_95, ymax = upper_95),
    width = 0.15,
    linewidth = 1
  ) +
  labs(
    x = "Sample Size N",
    y = "CrI Width",
    title = "Mean CrI Width with 2.5% and 97.5% Quantiles"
  ) +
  theme_minimal(base_size = 14)

ggplot(details_all, aes(x = CrI_width)) +
  geom_histogram(bins = 6, color = "white") +
  facet_wrap(~ N, scales = "free_y") +
  labs(
    x = "95% Credible Interval Width",
    y = "Count",
    title = "Distribution of CrI Widths Across 10 Simulation Runs"
  ) +
  theme_minimal(base_size = 14)

