# EXP 1
library(tidyverse)
library(broom)
load("data/exp2_data.RData")

# Pendientes:
# - que son Ansiedad1, 2 y 3?



#----
#anxiety-optimism correlation


dexp2 %>% 
  group_by(subj) %>% 
  summarise(id = first(subj), 
            anx = first(scoreAnsiedad1),
            opt = first(scoreOptimismo)) %>% 
  ungroup() %>% 
  ggplot(aes(x=opt, y=anx)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(10,30,1)) +
  scale_y_continuous(breaks = seq(10,60,1)) +
  labs(x ="LOT-r score", y = "trait anxiety score") + 
  geom_smooth(method = "lm", color = "black") + 
  theme_bw() + 
  theme(axis.title.x=element_text(vjust=-4)) +
  theme(axis.title.y=element_text(vjust=+6)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank()) 



#----
#p1-anxiety 

summary(lm(p1 ~ scoreAnsiedad1, data = dexp2))

dexp2 %>% 
  group_by(pregID) %>% 
  do(m =coef(lm(p1 ~ scoreAnsiedad1, data = .))) %>% 
  unnest_wider(m) %>%   
  select(scoreAnsiedad1) %>% 
  summary()

dexp2 %>% 
  group_by(pregID) %>% 
  do(m =coef(lm(p1 ~ scoreAnsiedad1, data = .))) %>% 
  unnest_wider(m) %>%   
  select(pregID, scoreAnsiedad1) %>% 
  ggplot(aes(x=scoreAnsiedad1, y=reorder(pregID, scoreAnsiedad1))) +
  geom_point() +
  geom_vline(xintercept = 0) + 
  theme_bw() + 
  labs(y ="item ID", x = "slope in: p1 ~ trait anxiety score") + 
  theme(axis.title.x=element_text(vjust=-4)) +
  theme(axis.title.y=element_text(vjust=+6)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank()) 
