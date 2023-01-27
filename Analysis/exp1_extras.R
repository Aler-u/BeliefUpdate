require(tidyverse)
require(brms)
require(sjPlot)
require(patchwork)
require(ggExtra)

load("data/exp1_data.RData")
dexp1 <- mutate(dexp1, good = factor(ifelse(I==0, "bad news", "good news")))


####### 
beta.scr1 = rep(NA, 33)
ans.ind = rep(NA, 33)
opt.ind = rep(NA, 33)
for (i in 1:33){
  dexp.ind = dexp1[dexp1$subj==i,]
  beta.scr1[i] = coef(lm(scr1 ~ good*ep, data=dexp.ind))[4]
  ans.ind[i] = dexp.ind$scoreAnsiedad[1]
  opt.ind[i] = dexp.ind$scoreOptimismo[1]
}
plot(ans.ind,beta.scr1)
summary(lm(ans.ind~beta.scr1))

plot(opt.ind,beta.scr1)
summary(lm(opt.ind~beta.scr1))

####### 
Ns = unique(dexp1[dexp1$memory2<3,'subj'])

beta1.mem2 = rep(NA, 23)
beta2.mem2 = rep(NA, 23)
ans.ind = rep(NA, 23)
k = 1
for (i in Ns){
  dexp.ind = dexp1[dexp1$subj==i & dexp1$memory2<3,]
  beta1.mem2[k] = coef(lm(memory2 ~ good * ep + good * update, data=dexp.ind))[5]
  beta2.mem2[k] = coef(lm(memory2 ~ good * ep + good * update, data=dexp.ind))[6]
  ans.ind[k] = dexp.ind$scoreAnsiedad[1]
  k = k + 1
}
plot(ans.ind,beta1.mem2)
plot(ans.ind,beta2.mem2)

summary(lm(ans.ind~beta1.mem2))
summary(lm(ans.ind~beta2.mem2))


#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################










####### scr1: que pasa con scoreAnsiedad (correlacion entre beta's ind con ansiedad / optimismo )

# sujeto x sujeto
dexp1.suj1 = dexp1[dexp1$subj==7,]
summary(lm(scr1 ~ good*ep, data=dexp1.suj1))




# rstan_options(auto_write = TRUE)
# options(mc.cores = 4)

fit1 <- brm(scr1 ~ good*ep,
            data = dexp1.suj1,
            seed = 100
            )


summary(fit1)


dexp1$scr1.new = dexp1$scr1
dexp1$scr1.new[dexp1$scr1.new==0] = 1e-3
dexp1$scr1.new = log(dexp1$scr1.new)
hist(dexp1$scr1.new)

summary(lm(scr1.new ~ good*ep, data=dexp1))



#####
p1 <- plot_model(fit1, type = "pred", terms = c("ep", "good"))
p2 <- plot_model(fit1, type = "pred", terms = c("update", "good"))

p1+p2

####### mem1
d = dexp1[dexp1$memory1<3,]
fit2 <- brm(memory1 ~ good * ep + good * update,
            data = d,
            seed = 100, # for reproducibility
            cores = 2, chains=4,
            save_pars = save_pars(all = T),
            file = "Analysis/model_fits/mem1_exp1"
)


p3 <- plot_model(fit2, type = "pred", terms = c("ep", "good"))
p4 <- plot_model(fit2, type = "pred", terms = c("update", "good"))

p3+p4


####### mem2 (beta's vs scoreAnsiedad y scoreOptimismo)
d = dexp1[dexp1$memory2<3,]
fit2 <- brm(memory2 ~ good * ep + good * update,
            data = d,
            seed = 100, # for reproducibility
            cores = 2, chains=4,
            save_pars = save_pars(all = T),
            file = "Analysis/model_fits/mem2_exp1"
)


p3 <- plot_model(fit2, type = "pred", terms = c("ep", "good"))
p4 <- plot_model(fit2, type = "pred", terms = c("update", "good"))

p3+p4



###### Relación entre conductancia y memoria. Correlacion entre beta?