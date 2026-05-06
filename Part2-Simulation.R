library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(here)

set.seed(261)
setwd(here())

# Read and clean the data


muscle_raw <- read_csv("Datasets/Data Sheet1.csv")
strength_raw <- read_csv("Datasets/Data Sheet2.csv")

muscle <- muscle_raw |>
  slice(1:50) |>
  select(1:13) |>
  mutate(
    GROUP = as.integer(GROUP),
    condition = if_else(GROUP == 1, "pROM", "fROM")
  ) |>
  group_by(condition) |>
  mutate(participant = row_number()) |>
  ungroup() |>
  mutate(
    participant = factor(participant),
    condition = factor(condition, levels = c("fROM", "pROM"))
  )

muscle_long <- muscle |>
  select(
    participant,
    condition,
    `BB55-Pre`, `BB55-Post`,
    `BB45-Pre`, `BB45-Post`,
    `TB55-Pre`, `TB55-Post`,
    `TB45-Pre`, `TB45-Post`
  ) |>
  pivot_longer(
    cols = -c(participant, condition),
    names_to = c("site", "time"),
    names_pattern = "(.+)-(Pre|Post)",
    values_to = "value"
  ) |>
  mutate(time = str_to_lower(time)) |>
  pivot_wider(
    names_from = time,
    values_from = value
  ) |>
  mutate(
    change = post - pre,
    site = factor(site)
  )

muscle_summary <- muscle_long |>
  group_by(condition, site) |>
  summarise(
    n = n(),
    mean_change = mean(change, na.rm = TRUE),
    sd_change = sd(change, na.rm = TRUE),
    .groups = "drop"
  )|>print()

n_participants <- 25
n_simulations  <- 1000

# Means
baseline_growth  <- 2.24   # fROM
treatment_effect <- -0.32  # Observed difference

# Variances
sigma_between <- 1.42  # Variation between individuals
sigma_error   <- 0.84  # Variation between limbs

params <- list(
  n = n_participants,
  mu_fROM = baseline_growth,
  mu_LP = baseline_growth + treatment_effect,
  sd_subject = sigma_between,
  sd_error = sigma_error
)

