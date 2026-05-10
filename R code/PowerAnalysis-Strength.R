library(ggplot2)
library(dplyr)

set.seed(261)

#EMPIRICAL PARAMETERS
#Design: within-person crossover (bilateral lat pulldown)
#Outcome: 10RM load change (Post – Pre) in lbs, per arm per condition
#  Paired t-test: t = 0.328, p = 0.744  (n = 50 participants)

mu_fROM      <- 5.300
mu_pROM      <- 4.900
true_effect  <- mu_fROM - mu_pROM

sigma_within <- 8.621

n_vec  <- seq(20, 200, by = 10)
n_sim  <- 1000
alpha  <- 0.05

#SIMULATION FUNCTION

simulate_power <- function(n, n_sim, true_effect, sigma_within, alpha) {
  reject <- logical(n_sim)
  for (s in seq_len(n_sim)) {
    d         <- rnorm(n, mean = true_effect, sd = sigma_within)
    p_val     <- t.test(d, mu = 0)$p.value
    reject[s] <- p_val < alpha
  }
  mean(reject)
}

#POWER CURVE (observed = 0.400 lbs)

results <- data.frame(n_per_group = n_vec, power = NA_real_, se = NA_real_)

for (i in seq_len(nrow(results))) {
  n  <- results$n_per_group[i]
  pw <- simulate_power(n, n_sim, true_effect, sigma_within, alpha)
  results$power[i] <- pw
  results$se[i]    <- sqrt(pw * (1 - pw) / n_sim)
}

print(results)

p1 <- ggplot(results, aes(x = n_per_group, y = power)) +
  geom_ribbon(aes(ymin = power - 1.96 * se,
                  ymax = power + 1.96 * se),
              alpha = 0.2, fill = "darkolivegreen") +
  geom_line(color = "darkolivegreen", linewidth = 1.1) +
  geom_point(color = "darkolivegreen", size = 2.8) +
  scale_x_continuous(breaks = n_vec,
                     name = "Participants per Treatment (n)") +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.05),
                     name = expression("Estimated Power (" * alpha * " = 0.05)")) +
  labs(
    title    = "Power Curve: fROM vs pROM Muscle Strength (10RM Lat Pulldown)",
    subtitle = sprintf(
      "Paired t-test simulation | true gain difference = %.3f lbs | %d simulations per cell",
      true_effect, n_sim),
    caption  = "Shaded band = 95% CI on estimated power"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )
print(p1)

#True effect of 0.400 lbs is essentially indistinguishable from zero. 
#Power stays near nominal α = 5% regardless
#of n, producing a flat line just like the muscle-thickness analysis.


#SENSITIVITY: VARY EFFECT SIZE
#The observed effect (0.400 lbs) is trivially small.
#Sweep 0.5-4 lbs (using paper's CrI) to show where studies become adequately powered

effect_sizes <- seq(0.5, 4.0, by = 0.5)

sens <- expand.grid(n_per_group = n_vec, true_effect = effect_sizes)
sens$power    <- NA_real_
sens$se       <- NA_real_
sens$cohen_d  <- round(sens$true_effect / sigma_within, 3)

for (i in seq_len(nrow(sens))) {
  n  <- sens$n_per_group[i]
  te <- sens$true_effect[i]
  pw <- simulate_power(n, n_sim, te, sigma_within, alpha)
  sens$power[i] <- pw
  sens$se[i]    <- sqrt(pw * (1 - pw) / n_sim)
}

sens$effect_label <- factor(
  sens$true_effect,
  levels = effect_sizes,
  labels = sprintf("\u0394 = %.1f lbs", effect_sizes)
)

p2 <- ggplot(sens,
             aes(x = n_per_group, y = power,
                 color = effect_label, group = effect_label)) +
  geom_ribbon(aes(ymin = power - 1.96 * se,
                  ymax = power + 1.96 * se,
                  fill  = effect_label),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = alpha, linetype = "dashed",
             color = "grey50", linewidth = 0.6) +
  annotate("text", x = 155, y = 0.05 - alpha,
           label = sprintf("observed \u0394 = 0.400 lbs"), color = "grey40") +
  geom_point(size = 2.8) +
  scale_color_brewer(palette = "Set1", name = "Gain Size") +
  scale_fill_brewer(palette  = "Set1", guide  = "none") +
  scale_x_continuous(breaks = n_vec,
                     name = "Participants per Treatment (n)") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.20),
                     name = expression("Estimated Power (" * alpha * " = 0.05)")) +
  labs(
    title   = "Sensitivity: Power by Gain Size (fROM vs pROM Strength)",
    caption = "Shaded bands = 95% CI on estimated power."
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )
print(p2)

#SUMMARY TABLES

cat("\nPRIMARY POWER TABLE (observed effect = 0.400 lbs)n")
print(
  results |>
    mutate(cohen_d = round(true_effect / sigma_within, 3),
           true_effect = true_effect,
           power = round(power, 3),
           se    = round(se, 3)) |>
    select(n_per_group, power, se)
)

cat("\nSENSITIVITY TABLE (effect size x n)n")
print(
  sens |>
    select(n_per_group, true_effect, cohen_d, power, se) |>
    mutate(power = round(power, 3), se = round(se, 3)) |>
    arrange(true_effect, n_per_group)
)

#Wolf et al. used n = 25 per condition.
#The observed strength difference between fROM and pROM (0.400 lbs) is
#below any curve in the sensitivity plot.
#This mirrors the muscle-thickness finding: the study is severely underpowered
#to detect the small (if any) strength difference actually present.
#A Bayesian approach is better suited here — the posterior can characterise
#the near-zero effect with credible intervals, rather than simply failing to
#reject H₀ with very low power.