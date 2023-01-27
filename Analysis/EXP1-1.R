# EXPERIMENTO 1
# Efecto trait-anxiety y trait-optimism en p1: overestimation vs underestimation?

library(tidyverse)
library(jtools)
library(lme4)
d1 <- read.csv("exp1/data/exp1_data.csv")

# 1- trait anxiety
m1    <- lm(p1 ~ scoreAnsiedad, data = d1)
summ(m1, confint = TRUE, pvals = FALSE)

m1.me <- lmer(p1 ~ scoreAnsiedad + (1 | pregID) + (1 | subj), data = d1)
summ(m1.me, confint = TRUE, pvals = FALSE)

ggplot(d1, aes(x=scoreAnsiedad, y=p1)) + 
  geom_smooth(method=lm, color='#2C3E50') +
  xlab("trait anxiety score") + 
  ylab("First prediction (p1)") + 
  theme_classic(base_size = 25) + coord_cartesian(ylim = c(0.25, 0.5))

# 2- trait optimism
m2    <- lm(p1 ~ scoreOptimismo, data = d1)
summ(m2, confint = TRUE, pvals = FALSE)

m2.me <- lmer(p1 ~ scoreOptimismo + (1 | pregID) + (1 | subj), data = d1)
summ(m2.me, confint = TRUE, pvals = FALSE)

ggplot(d1, aes(x=scoreOptimismo, y=p1)) + 
  geom_smooth(method=lm, color='#2C3E50') +
  xlab("trait optimism (LOT) score") + 
  ylab("First prediction (p1)") + 
  theme_classic(base_size = 25) + coord_cartesian(ylim = c(0.25, 0.5))


# 3- 
ggplot(d1, aes(x=scoreOptimismo, y=scoreAnsiedad)) + 
  geom_point() + 
  geom_smooth(method=lm, color='#2C3E50') +
  xlab("trait optimism (LOT) score") + 
  ylab("trait anxiety score") + 
  theme_classic(base_size = 25)




library(effects)

effects_m1 <- effects::effect(term = "scoreAnsiedad", mod = m1.me, xlevels = 10)
x1 <- as.data.frame(effects_m1)

ggplot() + 
  geom_line(data=x1, aes(x=scoreAnsiedad, y=fit), color="blue") +
  geom_ribbon(data= x1, aes(x=scoreAnsiedad, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  xlab("trait anxiety score") + 
  ylab("First prediction (p1)") + 
  theme_classic(base_size = 25) + coord_cartesian(ylim = c(0.25, 0.5))

effects_m2 <- effects::effect(term = "scoreOptimismo", mod = m2.me, xlevels = 10)
x2 <- as.data.frame(effects_m2)

ggplot() + 
  geom_line(data=x2, aes(x=scoreOptimismo, y=fit), color="blue") +
  geom_ribbon(data= x2, aes(x=scoreOptimismo, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  xlab("trait optimism (LOT) score") + 
  ylab("First prediction (p1)") + 
  theme_classic(base_size = 25) + coord_cartesian(ylim = c(0.25, 0.5))

library(sjPlot)
sjPlot::tab_model(m1.me, 
                  show.re.var= TRUE, digits = 3,
                  pred.labels =c("(Intercept)", "scoreAnsiedad"),
                  dv.labels= "Effects of Herbivores on Coral Cover")

sjPlot::tab_model(m2.me, 
                  show.re.var= TRUE, digits = 3,
                  pred.labels =c("(Intercept)", "scoreOptimismo"),
                  dv.labels= "Effects of Herbivores on Coral Cover")



m.me <- lmer(p1 ~ scoreAnsiedad*scoreOptimismo + (1 | pregID) + (1 | subj), data = d1)
summ(m.me, confint = TRUE)

# 
# library(broom.mixed)
# # Extract out the parameter estimates and confidence intervals
# coef_estimates <-
#   tidy(m1me, conf.int = TRUE) %>%
#   filter(effect == "fixed")
# 
# # Print the new dataframe
# print(coef_estimates)
# 
# # Plot the results using ggplot2
# ggplot(coef_estimates, aes(x = term, y = estimate,
#                            ymin = conf.low, ymax = conf.high)) +
#   geom_hline( yintercept = 0, color = 'red' ) +
#   geom_linerange() + geom_point() + coord_flip() + theme_minimal()
# 
# 
# mydf <- ggpredict(m1me)
# ggplot(mydf, aes(x, scoreAnsiedad_std)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
# 
# ggpredict(m1me, terms = "scoreAnsiedad_std")  
