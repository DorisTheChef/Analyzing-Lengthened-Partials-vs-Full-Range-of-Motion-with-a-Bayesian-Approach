library(tidyverse)
setwd("/Users/baixuezhang/Documents/SJSU/study/Math 261B/Project/")


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
muscle_change <- muscle_change %>%
  mutate(
    condition01 = ifelse(GROUP == 1, 0, 1)
  )

lm(BB55_change ~ condition01, data = muscle_change)

fit_bayes <- brm(
  BB55_change ~ condition01,
  data = muscle_change,
  family = gaussian(),
  prior = c(
    prior(normal(0, 5), class = "b"),
    prior(normal(0, 5), class = "Intercept")
  ),
  chains = 2,
  iter = 2000
)
summary(fit_bayes)
