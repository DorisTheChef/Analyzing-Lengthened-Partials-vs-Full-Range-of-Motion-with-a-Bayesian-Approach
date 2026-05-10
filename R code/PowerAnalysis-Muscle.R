library(lme4)       
library(lmerTest)
library(ggplot2)
library(dplyr)

set.seed(261)

#EMPIRICAL PARAMETERS (estimated from the paper)

mu_pROM      <- 1.847
mu_fROM      <- 1.822
true_effect  <- mu_pROM - mu_fROM

sigma_within <- 2.12

sigma_total  <- sigma_within
sigma_b      <- sqrt(icc * sigma_total^2)
sigma_e      <- sqrt((1 - icc) * sigma_total^2)

n_vec        <- seq(20, 200, by=10)
n_sim        <- 1000 
alpha        <- 0.05

simpson_cors <- c(-0.5, 0, 0.5)

#SIMULATION FUNCTION

simulate_power <- function(n, rho, n_sim, true_effect,
                           mu_fROM, sigma_b, sigma_e, alpha) {

  sd_diff <- sigma_e * sqrt(2 * (1 - rho))
  reject <- logical(n_sim)
  
  for (s in seq_len(n_sim)) {
    d <- rnorm(n, mean = true_effect, sd = sd_diff)
    
    p_val     <- t.test(d, mu = 0)$p.value
    reject[s] <- p_val < alpha
  }
  mean(reject)
}

#RUN SIMULATION

results <- data.frame(n_per_group = n_vec)
results$power <- NA_real_
results$se    <- NA_real_

for (i in seq_len(nrow(results))) {
  n <- results$n_per_group[i]
  
  pw <- simulate_power(
    n           = n,
    rho         = 0,#fixed: no within-participant correlation
    n_sim       = n_sim,
    true_effect = true_effect,
    mu_fROM     = mu_fROM,
    sigma_b     = sigma_b,
    sigma_e     = sigma_e,
    alpha       = alpha
  )
  
  results$power[i] <- pw
  results$se[i]    <- sqrt(pw * (1 - pw) / n_sim)
  
}

print(results)

#POWER CURVES PLOT

p <- ggplot(results, aes(x = n_per_group, y = power)) +
  geom_ribbon(aes(ymin = power - 1.96 * se,
                  ymax = power + 1.96 * se),
              alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(color = "steelblue", size = 2.8) +
  scale_x_continuous(breaks = n_vec,
                     name = "Participants per Treatment (n)") +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.05),
                     name = expression("Estimated Power (" * alpha * " = 0.05)")) +
  labs(
    title    = "Power Curve: pROM vs fROM Muscle Thickness (BB55)",
    subtitle = sprintf("Paired t-test simulation | true gain = %.3f mm | %d simulations per cell",
                       true_effect, n_sim),
    caption  = "Shaded band = 95% CI on estimated power"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )
print(p)

#Very low power, essentially a flat line regardless of how large n gets

# SENSITIVITY: VARYING EFFECT SIZE
#what would power look like if the true effect were larger than what we observed?
#The observed effect (~0.025 mm) is very smalL.
#If the true effect were actually 0.5 mm, or 1 mm, or 2 mm, 
#how large would n need to be to detect it?

effect_sizes <- seq(0.25, 2.0, by = 0.25)
rho_fixed    <- 0.0   #neutral within-participant correlation
#because theres no individual participant IDs linking the two limbs in our data

sens <- expand.grid(
  n_per_group  = n_vec,
  true_effect  = effect_sizes
)
sens$power <- NA_real_
sens$se    <- NA_real_
sens$cohen_d <- round(sens$true_effect / sigma_within, 3)

for (i in seq_len(nrow(sens))) {
  n  <- sens$n_per_group[i]
  te <- sens$true_effect[i]
  
  pw <- simulate_power(
    n           = n,
    rho         = rho_fixed,
    n_sim       = n_sim,
    true_effect = te,
    mu_fROM     = mu_fROM,
    sigma_b     = sigma_b,
    sigma_e     = sigma_e,
    alpha       = alpha
  )
  sens$power[i] <- pw
  sens$se[i]    <- sqrt(pw * (1 - pw) / n_sim)
  
}

sens$effect_label <- factor(
  sens$true_effect,
  levels = effect_sizes,
  labels = sprintf("Δ = %.3f mm", 
                   effect_sizes)
)

p2 <- ggplot(sens,
             aes(x = n_per_group,
                 y = power,
                 color = effect_label,
                 group = effect_label)) +
  geom_ribbon(aes(ymin = power - 1.96 * se,
                  ymax = power + 1.96 * se,
                  fill  = effect_label),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0.025, linetype = "dashed",
             color = "grey50", linewidth = 0.6)+
  annotate("text", x=80, y=0.065, label="true gain = 0.025")+
  geom_point(size = 2.8) +
  scale_color_brewer(palette = "Set1", name = "Gain Size") +
  scale_fill_brewer(palette  = "Set1", guide = "none") +
  scale_x_continuous(breaks = n_vec, name = "Participants per Treatment (n)") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.20),
                     name = expression("Estimated Power (" * alpha * " = 0.05)")) +
  labs(
    title    = "Sensitivity: Power by Gain Size (pROM vs fROM)",
    caption  = "Shaded bands = 95% CI on estimated power."
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
print(p2)

#SUMMARY TABLE

print(
  results |>
    select(n_per_group, power, se) |>
    mutate(power = round(power, 3), se = round(se, 3)) |>
    arrange(n_per_group)
)

cat("\n=== SENSITIVITY TABLE\n")
print(
  sens |>
    select(n_per_group, true_effect, cohen_d, power, se) |>
    mutate(power = round(power, 3), se = round(se, 3)) |>
    arrange(true_effect, n_per_group)
)

#Wolf et al. used n = 25 per condition. 
#Reading vertically at n = 25, only effect sizes >= 1.25 m were 
#adequately powered for the 8-% threshold.
#The observed effect in their data was 0.025 mm, which is far below any of these curves. 
#From a frequentist perspective, this study in severely underpowered 
#to detect the kind of small difference that might actually exist between pROM and fROM.
#This is exactly where the Bayesian framing becomes relevant: 
#rather than concluding "we failed to reject H₀," the Bayesian approach can instead say 
#"our posterior is precise enough that we're confident the effect, if any, 
#is too small to be practically meaningful"
#a much more informative conclusion from a small sample.
