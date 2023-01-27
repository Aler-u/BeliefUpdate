library(brms)

d1 <- read.csv("exp1/data/exp1_data.csv")
d2 <- read.csv("exp2/data/exp2_data.csv")




# datos del exp 1
d          <- data.frame(id = d1$subj, p1 = d1$p1, p2 = d1$p2, pT = d1$pT, ansiedad = d1$scoreAnsiedad_std, optimismo = d1$scoreOptimismo_std)
d          <- d[d$p2>0,]
d$Igood    <- ifelse(d$p1 > d$pT, 1, 0)
d$pTc      <- d$pT-0.5
d$p1c      <- d$p1-0.5

m1   <- bf( p2 ~ 1 + p1 + pT, phi ~ 1, family = Beta())
fit1 <- brm( m1, data = d)

m2   <- bf( p2 ~ 1 + p1 + pT + pT:Igood, phi ~ 1, family = Beta())
fit2 <- brm( m2, data = d)

m3   <- bf( p2 ~ 1 + optimismo + p1 + pT + pT:optimismo + pT:Igood + pT:Igood:optimismo, phi ~ 1, family = Beta())
fit3 <- brm( m3, data = d)

m4   <- bf( p2 ~ 1 + ansiedad + p1 + pT + pT:ansiedad + pT:Igood + pT:Igood:ansiedad, phi ~ 1, family = Beta())
fit4 <- brm( m4, data = d)

m5   <- bf( p2 ~ 1 + ansiedad + optimismo + p1 + pT + pT:ansiedad + pT:optimismo + pT:Igood + pT:Igood:ansiedad + pT:Igood:optimismo, phi ~ 1, family = Beta())
fit5 <- brm( m5, data = d)

m2m   <- bf( p2 ~ 1 + p1 + pT + pT:Igood + (1 + pT + pT:Igood | id), phi ~ 1, family = Beta())
fit2m <- brm( m2m, data = d)

m3m   <- bf( p2 ~ 1 + optimismo + p1 + pT + pT:optimismo + pT:Igood + pT:Igood:optimismo
             + (1 + pT + pT:optimismo + pT:Igood + pT:Igood:optimismo | id), phi ~ 1, family = Beta())
fit3m <- brm( m3m, data = d)

m6   <- bf( p2  ~ 1 + p1c + pTc + pTc:Igood + (pTc+pTc:Igood|id), 
            phi ~ 1 + (1|id), family = Beta())
fit6 <- brm( m6, data = d, iter = 2000)
x    <- posterior_predict(fit6, newdata = data.frame(p1c = -0.3, pTc = 0, Igood = 0))

####################
ep <- 0.1
pT <- 0.2
# good news
p1good <- pT + ep
I      <- 1
p2good <- posterior_predict(fit6, newdata = data.frame(p1c = p1good-0.5, pTc = pT-0.5, Igood = I), re_formula = NA)
upgood <- p1good - p2good
# bad news
p1bad  <- pT - ep
I      <- 0
p2bad  <- posterior_predict(fit6, newdata = data.frame(p1c = p1bad-0.5, pTc = pT-0.5, Igood = I), re_formula = NA)
upbad  <- p2bad - p1bad

plot(density(p2bad), xlim = c(0,1))
lines(density(p2good), col="red")
abline(v=p1bad)
abline(v=p1good, col="red")
abline(v=pT, lwd = 4)



