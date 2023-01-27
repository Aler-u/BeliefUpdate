# Experiment 2
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
require(sjPlot)
require(patchwork)
require(ggExtra)

load("data/exp2_data.RData")
dexp2 <- mutate(dexp2, good = factor(ifelse(I==0, "bad news", "good news")))

# 0- Model

m.fit1 <- brm(update ~
                good * scoreOptimismo * ep +
                good * scoreAnsiedad1 * ep +
                good * pT +
                (1 | subj),
              data = dexp2,
              seed = 100, # for reproducibility
              cores = 2, chains=4,
              save_pars = save_pars(all = T),
              file = "Analysis/model_fits/fit1_exp2"
              )

m.fit2 <- brm(update ~
                good * scoreOptimismo * ep +
                good * scoreAnsiedad2 * ep +
                good * pT +
                (1 | subj),
              data = dexp2,
              seed = 100, # for reproducibility
              cores = 2, chains=4,
              save_pars = save_pars(all = T),
              file = "Analysis/model_fits/fit2_exp2"
              )

m.fit3 <- brm(update ~
                good * scoreOptimismo * ep +
                good * scoreAnsiedad3 * ep +
                good * pT +
                (1 | subj),
              data = dexp2,
              seed = 100, # for reproducibility
              cores = 2, chains=4,
              save_pars = save_pars(all = T),
              file = "Analysis/model_fits/fit3_exp2")

m.fit1 = brm(file = "Analysis/model_fits/fit1_exp2")
m.fit2 = brm(file = "Analysis/model_fits/fit2_exp2")
m.fit3 = brm(file = "Analysis/model_fits/fit3_exp2")

# 1- belief updating is larger for good than for bad news

p1 <- plot_model(m.fit3, type = "pred", terms = "good")
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


p2 <- plot_model(m.fit3, type = "pred", terms = c("ep", "good"))
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

p3 <- plot_model(m.fit1, type = "pred", terms = c("scoreOptimismo", "good"))
p4 <- plot_model(m.fit1, type = "pred", terms = c("scoreAnsiedad1", "good"))

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

p1 <- plot_model(m.fit1, type = "pred", terms = c("ep", "scoreOptimismo", "good"))
p2 <- plot_model(m.fit1, type = "pred", terms = c("ep", "scoreAnsiedad1", "good"))

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
  guides(col= guide_legend(title= "anxiety1 score")) +
  labs(colour = "") 

p1 / p2

# 3- correlation anxiety - optimism

d <- dexp2 %>% 
  group_by(subj) %>%
  mutate(anx = first(scoreAnsiedad1), opt = first(scoreOptimismo)) 

p <- ggplot(aes(x = anx, y = opt), data = d) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw() +
  labs(x = "anxiety 1", y = "optimism", title = "")

p <- ggMarginal(p, type="histogram", 'binwidth = x')
p

d <- dexp2 %>% 
  group_by(subj) %>%
  mutate(anx = first(scoreAnsiedad2), opt = first(scoreOptimismo)) 

p <- ggplot(aes(x = anx, y = opt), data = d) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw() +
  labs(x = "anxiety 2", y = "optimism", title = "")

p <- ggMarginal(p, type="histogram", 'binwidth = x')
p

d <- dexp2 %>% 
  group_by(subj) %>%
  mutate(anx = first(scoreAnsiedad3), opt = first(scoreOptimismo)) 

p <- ggplot(aes(x = anx, y = opt), data = d) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw() +
  labs(x = "anxiety 3", y = "optimism", title = "")

p <- ggMarginal(p, type="histogram", 'binwidth = x')
p


# 4- optimism and anxiety affect p1?

m1 <- brm(p1 ~ scoreOptimismo + (1 | subj), data = dexp2, file = "Analysis/model_fits/p1opt_exp2")
m2 <- brm(p1 ~ scoreAnsiedad1 + (1 | subj), data = dexp2, file = "Analysis/model_fits/p1ans1_exp2")
m3 <- brm(p1 ~ scoreAnsiedad2 + (1 | subj), data = dexp2, file = "Analysis/model_fits/p1ans2_exp2")
m4 <- brm(p1 ~ scoreAnsiedad3 + (1 | subj), data = dexp2, file = "Analysis/model_fits/p1ans3_exp2")

m1 = brm(file = "Analysis/model_fits/p1opt_exp2")
m2 = brm(file = "Analysis/model_fits/p1ans1_exp2")
m3 = brm(file = "Analysis/model_fits/p1ans2_exp2")
m4 = brm(file = "Analysis/model_fits/p1ans3_exp2")

p1 <- plot_model(m1, type = "pred", terms = c("scoreOptimismo"))
p2 <- plot_model(m2, type = "pred", terms = c("scoreAnsiedad1"))
p3 <- plot_model(m3, type = "pred", terms = c("scoreAnsiedad2"))
p4 <- plot_model(m4, type = "pred", terms = c("scoreAnsiedad3"))



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

