# Experiment 1
# 
# 0- Model
# 1- belief updating is larger for good than for bad news
# 2- the effect of anxiety and optimism on belief updating
# 3- correlation anxiety - optimism
# 4- optimism and anxiety affect p1?
#
# last update: 4.Jul.22

require(tidyverse)
require(brms)
require(sjstats)
require(sjPlot)
require(patchwork)
require(ggExtra)

load("data/exp1_data.RData")
dexp1 <- mutate(dexp1, good = factor(ifelse(I==0, "bad news", "good news")))

# 0- Model

# m.fit <- brm(update ~ 
#                good * scoreOptimismo * ep + 
#                good * scoreAnsiedad * ep + 
#                good * pT + 
#                (1 | subj), 
#              data = dexp1, 
#              seed = 100, # for reproducibility
#              cores = 2, chains=4,
#              save_pars = save_pars(all = T),
#              file = "Analysis/model_fits/fit_exp1"
#              )

m.fit = brm(file = "Analysis/model_fits/fit_exp1")

tidy_stan(m.fit)

# 1- belief updating is larger for good than for bad news

p1 <- plot_model(m.fit, type = "pred", terms = "good")
p1 <- p1 + 
  theme_bw() +
  labs(y = "update", title = "") + 
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(vjust=+6)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank()) 


p2 <- plot_model(m.fit, type = "pred", terms = c("ep", "good"))
p2 <- p2 + 
  theme_bw() +
  labs(x = "prediction error", y = "update", title = "") + 
  theme(axis.title.x=element_text(vjust=-4)) +
  theme(axis.title.y=element_text(vjust=+6)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        legend.title=element_blank()) 

p1 + p2


# 2- the effect of anxiety and optimism on belief updating

p3 <- plot_model(m.fit, type = "pred", terms = c("scoreOptimismo", "good"))
p4 <- plot_model(m.fit, type = "pred", terms = c("scoreAnsiedad", "good"))

p3 <- p3 + 
  theme_bw() +
  labs(x = "optimism", y = "update", title = "") + 
  ylim(0,0.3) +
  xlim(13,22) +
  theme(axis.title.x=element_text(vjust=-4)) +
  theme(axis.title.y=element_text(vjust=+6)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        legend.position='none') +
  labs(colour = "") 


p4 <- p4 + 
  theme_bw() +
  labs(x = "anxiety", y = "update", title = "") + 
  ylim(0,0.3) +
  theme(axis.title.x=element_text(vjust=-4)) +
  theme(axis.title.y=element_text(vjust=+6)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank()) +
  labs(colour = "") 

p3 + p4


# 2b- the effect of anxiety and optimism on belief updating

p1 <- plot_model(m.fit, type = "pred", terms = c("ep", "scoreOptimismo", "good"))
p2 <- plot_model(m.fit, type = "pred", terms = c("ep", "scoreAnsiedad", "good"))

p1 <- p1 + 
  theme_bw() +
  labs(x = "prediction error", y = "update", title = "") + 
  theme(axis.title.x=element_text(vjust=-4)) +
  theme(axis.title.y=element_text(vjust=+6)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank()) +
  guides(col= guide_legend(title= "optimism score")) +
  labs(colour = "") 

p2 <- p2 + 
  theme_bw() +
  labs(x = "prediction error", y = "update", title = "") + 
  theme(axis.title.x=element_text(vjust=-4)) +
  theme(axis.title.y=element_text(vjust=+6)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank()) +
  guides(col= guide_legend(title= "anxiety score")) +
  labs(colour = "") 

p1 / p2

# 3- correlation anxiety - optimism

d <- dexp1 %>% 
  group_by(subj) %>%
  mutate(anx = first(scoreAnsiedad), opt = first(scoreOptimismo)) 

p <- ggplot(aes(x = anx, y = opt), data = d) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw() +
  labs(x = "anxiety", y = "optimism", title = "")

p <- ggMarginal(p, type="histogram", 'binwidth = x')
p



# 4- optimism and anxiety affect p1?

m1 <- brm(p1 ~ scoreOptimismo + (1 | subj), data = dexp1, file = "Analysis/model_fits/p1opt_exp1")
m2 <- brm(p1 ~ scoreAnsiedad + (1 | subj), data = dexp1, file = "Analysis/model_fits/p1ans_exp1")

m1 = brm(file = "Analysis/model_fits/p1opt_exp1")
m2 = brm(file = "Analysis/model_fits/p1ans_exp1")

p1 <- plot_model(m1, type = "pred", terms = c("scoreOptimismo"))
p2 <- plot_model(m2, type = "pred", terms = c("scoreAnsiedad"))

p1 <- p1 + theme_bw() +
  labs(x = "optimism", y = "p1", title = "") + 
  xlim(13,25) +
  ylim(0.2,0.5) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank()) +
  labs(colour = "") 

p2 <- p2 + theme_bw() +
  labs(x = "anxiety", y = "p1", title = "") + 
  xlim(18,35) +
  ylim(0.2,0.5) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank()) +
  labs(colour = "") 

p1 + p2


