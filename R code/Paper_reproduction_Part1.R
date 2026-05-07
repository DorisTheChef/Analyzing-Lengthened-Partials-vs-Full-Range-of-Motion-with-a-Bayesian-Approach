library(tidyverse)
setwd("/Users/baixuezhang/Documents/SJSU/study/Math 261B/Project/R code")


muscle <- read_csv("Data Sheet1.csv")
strength <- read_csv("Data Sheet2.csv")
muscle <- muscle[1:50, 1:13]


View(muscle)
View(strength)


library(tidyverse)

# 假设你的数据叫 muscle
muscle <- muscle

# 转成长表（只做一个变量先，比如 BB55）
muscle_long <- muscle %>%
  select(GROUP, BB55_Pre = `BB55-Pre`, BB55_Post = `BB55-Post`) %>%
  pivot_longer(
    cols = c(BB55_Pre, BB55_Post),
    names_to = "time",
    values_to = "value"
  ) %>%
  mutate(
    time = ifelse(time == "BB55_Pre", "pre", "post"),
    condition = ifelse(GROUP == 1, "pROM", "fROM")
  )

# 计算均值和SD
muscle_summary <- muscle_long %>%
  group_by(condition, time) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    .groups = "drop"
  )

muscle_summary

# frequentistic
muscle_change <- muscle %>%
mutate(
  BB55_change = `BB55-Post` - `BB55-Pre`
)

muscle_change_summary <- muscle_change %>%
  group_by(GROUP) %>%
  summarise(
    mean_change = mean(BB55_change),
    sd_change = sd(BB55_change)
  )

muscle_change_summary
t.test(BB55_change ~ GROUP, data = muscle_change)

lm(BB55_change ~ GROUP, data = muscle_change)

brm(BB55_change ~ GROUP)
muscle_summary
muscle_change_summary
t.test(BB55_change ~ GROUP, data = muscle_change)


# 样本量
n1 <- sum(muscle_change$GROUP == 1)
n2 <- sum(muscle_change$GROUP == 2)

# 均值
mean1 <- mean(muscle_change$BB55_change[muscle_change$GROUP == 1])
mean2 <- mean(muscle_change$BB55_change[muscle_change$GROUP == 2])

# 标准差
sd1 <- sd(muscle_change$BB55_change[muscle_change$GROUP == 1])
sd2 <- sd(muscle_change$BB55_change[muscle_change$GROUP == 2])

# pooled sd
sd_pooled <- sqrt(((n1 - 1)*sd1^2 + (n2 - 1)*sd2^2) / (n1 + n2 - 2))

# effect size (Cohen's d)
effect_size <- (mean1 - mean2) / sd_pooled

effect_size

install.packages("pwr")
library(pwr)

power_result <- pwr.t2n.test(
  n1 = n1,
  n2 = n2,
  d = effect_size,
  sig.level = 0.05,
  alternative = "two.sided"
)

power_result

# paper
library(brms)

fit_bayes <- brm(
  BB55_change ~ GROUP,
  data = muscle_change,
  family = gaussian(),
  prior = c(
    prior(normal(0, 0.4), class = "b"),
    prior(normal(0, 0.4), class = "Intercept")
  ),
  chains = 2,
  iter = 2000
)
summary(fit_bayes)

# to match the paper
# -----------------------------
# Bayesian mixed model like paper
# Muscle thickness data first
# -----------------------------

library(tidyverse)
library(brms)

# Clean muscle data
muscle_clean <- muscle %>%
  mutate(
    row_id = row_number(),
    GROUP = as.character(GROUP)
  ) %>%
  group_by(GROUP) %>%
  mutate(
    participant = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    participant = factor(participant),
    condition = ifelse(GROUP == "1", "pROM", "fROM"),
    condition = factor(condition, levels = c("fROM", "pROM"))
  )

muscle_change <- muscle_clean %>%
  mutate(
    BB55_change = `BB55-Post` - `BB55-Pre`,
    BB45_change = `BB45-Post` - `BB45-Pre`,
    TB55_change = `TB55-Post` - `TB55-Pre`,
    TB45_change = `TB45-Post` - `TB45-Pre`
  )

fit_BB55_H1 <- brm(
  formula = BB55_change ~ condition + (1 | participant),
  data = muscle_change,
  family = gaussian(),
  prior = c(
    prior(normal(0, 5), class = "Intercept"),
    prior(normal(0, 5), class = "b"),
    prior(exponential(1), class = "sd"),
    prior(exponential(1), class = "sigma")
  ),
  iter = 4000,
  warmup = 2000,
  chains = 4,
  cores = 4,
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

summary(fit_BB55_H1)

# full model for BB55
fit_BB55_H0 <- brm(
  formula = BB55_change ~ 1 + (1 | participant),
  data = muscle_change,
  family = gaussian(),
  prior = c(
    prior(normal(0, 5), class = "Intercept"),
    prior(exponential(1), class = "sd"),
    prior(exponential(1), class = "sigma")
  ),
  iter = 4000,
  warmup = 2000,
  chains = 4,
  cores = 4,
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

summary(fit_BB55_H0)

# --------------------------------------------------
# Make Figure 1-style posterior distribution plot
# --------------------------------------------------

library(tidyverse)
library(brms)
library(posterior)
library(ggridges)

# If ggridges is not installed, run:
#install.packages("ggridges")

# -----------------------------
# 1. Prepare muscle data
# -----------------------------

muscle_clean <- muscle %>%
  mutate(
    GROUP = as.character(GROUP)
  ) %>%
  group_by(GROUP) %>%
  mutate(
    participant = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    participant = factor(participant),
    condition = ifelse(GROUP == "1", "pROM", "fROM"),
    condition = factor(condition, levels = c("fROM", "pROM")),
    
    BB55_change = `BB55-Post` - `BB55-Pre`,
    BB45_change = `BB45-Post` - `BB45-Pre`,
    TB55_change = `TB55-Post` - `TB55-Pre`,
    TB45_change = `TB45-Post` - `TB45-Pre`
  )

# -----------------------------
# 2. Prepare strength data
# -----------------------------

strength_clean <- strength %>%
  mutate(
    GROUP = as.character(GROUP)
  ) %>%
  group_by(GROUP) %>%
  mutate(
    participant = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    participant = factor(participant),
    condition = ifelse(GROUP == "1", "pROM", "fROM"),
    condition = factor(condition, levels = c("fROM", "pROM")),
    
    Full10RM_change = `LP-Full10rmPost` - `LP-Full10rmPre`,
    Partial10RM_change = `LP-Part10rmPost` - `LP-Part10rmPre`
  )
# -----------------------------
# 3. Function to fit model
# -----------------------------

fit_change_model <- function(data, outcome) {
  
  form <- as.formula(
    paste0(outcome, " ~ condition + (1 | participant)")
  )
  
  brm(
    formula = form,
    data = data,
    family = gaussian(),
    prior = c(
      prior(normal(0, 5), class = "Intercept"),
      prior(normal(0, 5), class = "b"),
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
# -----------------------------
# 4. Fit models
# -----------------------------

fit_BB45 <- fit_change_model(muscle_clean, "BB45_change")
fit_BB55 <- fit_change_model(muscle_clean, "BB55_change")
fit_TB45 <- fit_change_model(muscle_clean, "TB45_change")
fit_TB55 <- fit_change_model(muscle_clean, "TB55_change")

fit_Full10RM <- fit_change_model(strength_clean, "Full10RM_change")
fit_Partial10RM <- fit_change_model(strength_clean, "Partial10RM_change")

# -----------------------------
# 5. Function to extract posterior SMDs
# -----------------------------

extract_condition_draws <- function(fit, outcome_label, standardizer) {
  
  draws <- as_draws_df(fit)
  
  tibble(
    fROM = draws$b_Intercept / standardizer,
    pROM = (draws$b_Intercept + draws$b_conditionpROM) / standardizer
  ) %>%
    pivot_longer(
      cols = c(fROM, pROM),
      names_to = "condition",
      values_to = "smd"
    ) %>%
    mutate(
      outcome = outcome_label
    )
}
# -----------------------------
# 6. Standardizers: pooled pre SD
# -----------------------------

sd_BB45 <- sd(muscle_clean$`BB45-Pre`, na.rm = TRUE)
sd_BB55 <- sd(muscle_clean$`BB55-Pre`, na.rm = TRUE)
sd_TB45 <- sd(muscle_clean$`TB45-Pre`, na.rm = TRUE)
sd_TB55 <- sd(muscle_clean$`TB55-Pre`, na.rm = TRUE)

sd_Full10RM <- sd(strength_clean$`LP-Full10rmPre`, na.rm = TRUE)
sd_Partial10RM <- sd(strength_clean$`LP-Part10rmPre`, na.rm = TRUE)

# -----------------------------
# 7. Combine posterior draws
# -----------------------------

plot_data <- bind_rows(
  extract_condition_draws(fit_BB45, "Elbow Flexor 45%", sd_BB45),
  extract_condition_draws(fit_BB55, "Elbow Flexor 55%", sd_BB55),
  extract_condition_draws(fit_TB45, "Elbow Extensor 45%", sd_TB45),
  extract_condition_draws(fit_TB55, "Elbow Extensor 55%", sd_TB55),
  extract_condition_draws(fit_Full10RM, "10RM full", sd_Full10RM),
  extract_condition_draws(fit_Partial10RM, "10RM partial", sd_Partial10RM)
)

plot_data$outcome <- factor(
  plot_data$outcome,
  levels = c(
    "10RM partial",
    "10RM full",
    "Elbow Extensor 55%",
    "Elbow Extensor 45%",
    "Elbow Flexor 55%",
    "Elbow Flexor 45%"
  )
)
# -----------------------------
# 8. Plot Figure 1-style density plot
# -----------------------------

fig1_reanalysis <- ggplot(
  plot_data,
  aes(
    x = smd,
    y = outcome,
    fill = condition
  )
) +
  geom_density_ridges(
    alpha = 0.65,
    scale = 1.2,
    color = "black",
    linewidth = 0.4,
    rel_min_height = 0.01
  ) +
  geom_vline(xintercept = 0, linewidth = 0.6) +
  geom_vline(
    xintercept = c(-0.1, 0.1, 0.35, 0.65),
    linetype = "dashed",
    color = "gray70"
  ) +
  annotate("text", x = -0.1, y = 6.55, label = "Small", color = "gray60", size = 4) +
  annotate("text", x = 0.1, y = 6.55, label = "Small", color = "gray60", size = 4) +
  annotate("text", x = 0.35, y = 6.55, label = "Medium", color = "gray60", size = 4) +
  annotate("text", x = 0.65, y = 6.55, label = "Large", color = "gray60", size = 4) +
  labs(
    x = "Standardized Mean Difference",
    y = NULL,
    fill = "Intervention"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

fig1_reanalysis
ggsave(
  filename = "reanalysis_figure1_density_plot.png",
  plot = fig1_reanalysis,
  width = 9,
  height = 6,
  dpi = 300
)

# posterior distribution
plot(fit_BB55_H1)
