library(tidyverse)

#Load data
datos <- load('data/exp1_data.RData')

#Compute influence score based on Zanete and factor column for good/bad news 
dexp1 <- dexp1 %>%
  mutate(
    influencia = (p2-p1)/(pT-p1),
    news = factor(ifelse(I==0, "bad news", "good news"))
  )

# Plot relationship between influence score and update score
ggplot(
  dexp1,
  aes(influencia, update, color = good)
) +
  geom_point() + theme_bw() 
  
require(brms)
require(sjstats)
require(broom.mixed)
require(sjPlot)
require(bayesplot)
require(patchwork)
require(ggExtra)


m.fit1 <- brm(update ~ 
            news * scoreOptimismo * ep +
            news * scoreAnsiedad * ep +
            news * pT +
            (1 | subj),
            data = dexp1,
            seed = 1234, # for reproducibility
            cores = 4, chains = 4,
            iter = 20000,
            save_pars = save_pars(all = T),
            file = "Analysis/model_fits/FitTest_exp1"
            )

tidy(m.fit1, conf.level = 0.95, conf.method = 'HPDinterval')
plot(m.fit1, variable = 'sigma')

m.fit2 <- brm(influencia ~ 
                news * scoreOptimismo +
                news * scoreAnsiedad +
                news * pT +
                (1 | subj),
              data = dexp1,
              seed = 1234, # for reproducibility
              cores = 4, chains=4,
              iter = 10000,
              save_pars = save_pars(all = T),
              file = "Analysis/model_fits/InfluenceFit_exp1"
)

tidy(m.fit2, conf.level = 0.95, conf.method = 'HPDinterval')
plot(m.fit2, variable = 'sigma')

dexp1 %>% group_by(subj, news) %>% summarise(desvio = sd(update)) %>%
  ggplot(
    aes(x = news, y = desvio)
  ) +
  geom_point() +
  geom_line(aes(group = subj))
