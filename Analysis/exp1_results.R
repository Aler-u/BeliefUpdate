library(tidyverse)
require(brms)
require(bayesplot)

#Load data
datos <- load('data/exp1_data.RData')

#Compute factor column for good/bad news 
dexp1 <- dexp1 %>%
  mutate(
    news = factor(ifelse(I==0, "bad news", "good news"))
  )


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

mcmc_trace(m.fit1, regex_pars = 'b_')

mcmc_dens_overlay(m.fit1, regex_pars = 'b_')

mcmc_acf(m.fit1, regex_pars = 'b_')

neff_ratio(m.fit1)

rhat(m.fit1)
