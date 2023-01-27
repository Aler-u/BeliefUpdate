# Experiment 1
# 
# 1- % de casos con update negativo (se alejan de pT en lugar de acercarse)
#    por participante.
# 2- verificamos si + ansiedad -> + casos de este tipo
#
# last update: 6.Apr.22 

require(tidyverse)
require(brms)
require(sjPlot)
require(patchwork)

load("data/exp1_data.RData")
dexp1 <- mutate(dexp1, 
                good = factor(ifelse(I==0, "bad news", "good news")),
                upneg = factor(ifelse(update<0, 1, 0)))


# 1- % de casos con update negativo (se alejan de pT en lugar de acercarse)
#    por participante.

dexp1 %>% 
  group_by(subj) %>% 
  summarise(subj = first(subj), 
            scoreAnsiedad = first(scoreAnsiedad), 
            scoreOpt = first(scoreOptimismo), 
            rate.update.neg = mean(update<0))  %>%
  ggplot(aes(x=scoreAnsiedad, y=rate.update.neg)) + 
  theme_bw() +
  geom_point() + 
  geom_smooth(method = "lm")




# 2- verificamos si + ansiedad -> + casos de este tipo
# (el smooth anterior es medio trucho, hago un modelo mejor aca)


m <- brm(upneg ~ ep + scoreAnsiedad + scoreOptimismo + (1 | subj), 
         data = dexp1, 
         family = bernoulli(link = "logit"))
summary(m)
p <- plot_model(m, type = "pred", terms = "scoreAnsiedad [all]")

p <- p + 
  theme_bw() +
  labs(y = "% update neg", x = "ansiedad", title = "") + 
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank()) 

p
