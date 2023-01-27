require(brms)
require(sjPlot)
#require(emmeans)
#require(tidybayes)
require(dplyr)

d1 <- read.csv("./data/exp1_data.csv")
d2 <- read.csv("./data/exp2_data.csv")

d1$I <- as.factor(as.integer(d1$I))
d2$I <- as.factor(as.integer(d2$I))

##### exp 1 #####
m1 <- brm(update ~ I * scoreOptimismo_std * ep_std + I * scoreAnsiedad_std * ep_std + I * pT + ( 1 | subj), data = d1)
summary(m1)

conditions <- list(
  scoreAnsiedad_std = setNames(c(-1, 0, 1), c("low ans", "mid ans", "high ans"))
)
plot(conditional_effects(m1, effects = "ep_std:I", conditions = conditions))

conditions <- list(
  scoreOptimismo_std = setNames(c(-1, 0, 1), c("low opt", "mid opt", "high opt"))
)
plot(conditional_effects(m1, effects = "ep_std:I", conditions = conditions))


##### exp 2 #####

m2 <- brm(update ~ I * scoreOptimismo_std * ep_std + 
                   I * scoreAnsiedad1_std * ep_std + 
                   I * scoreAnsiedad2_std * ep_std + 
                   I * scoreAnsiedad3_std * ep_std + 
                   I * pT + ( 1 | subj), data = d2)
summary(m2)

conditions <- list(
  scoreAnsiedad1_std = setNames(c(-1, 0, 1), c("low ans1", "mid ans1", "high ans1"))
)
plot(conditional_effects(m2, effects = "ep_std:I", conditions = conditions))

conditions <- list(
  scoreAnsiedad2_std = setNames(c(-1, 0, 1), c("low ans2", "mid ans2", "high ans2"))
)
plot(conditional_effects(m2, effects = "ep_std:I", conditions = conditions))

conditions <- list(
  scoreAnsiedad3_std = setNames(c(-1, 0, 1), c("low ans3", "mid ans3", "high ans3"))
)
plot(conditional_effects(m2, effects = "ep_std:I", conditions = conditions))

conditions <- list(
  scoreOptimismo_std = setNames(c(-1, 0, 1), c("low opt", "mid opt", "high opt"))
)
plot(conditional_effects(m2, effects = "ep_std:I", conditions = conditions))


