
d <- dexp1 %>% filter(scr1>0)

m <- brm(update ~ 
           good * scoreOptimismo * ep + 
           good * scoreAnsiedad * ep + 
           good * pT + 
           scr1 * ep +
           (1 | subj), 
         data = dexp1)

summary(m)


ms <- brm(update ~ 
           good * scoreOptimismo * ep + 
           good * scoreAnsiedad * ep + 
           good * pT + 
           scr1 * ep +
           (1 | subj), 
         data = d)

summary(ms)



ms1 <- brm(update ~ 
             good * ep + 
             good * pT + 
             scr1 * ep +
             (1 | subj), 
           data = d)

summary(ms1)


ms2 <- brm(update ~ scr1 * ep + (1 | subj), data = d)

summary(ms2)
