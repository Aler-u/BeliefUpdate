d <- dexp1 %>% filter(memory1 < 1 & memory2 < 1) %>% mutate(memdiff = memory2 - memory1)

m1 <- brm(memory1 ~ 
           good * scoreOptimismo + 
           good * scoreAnsiedad + 
           good * ep +
           (1 | subj), 
         data = d)

summary(m1)



m2 <- brm(memdiff ~ scoreOptimismo + scoreAnsiedad + good * ep + (1 | subj), data = d)

summary(m2)

