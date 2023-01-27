d <- data.frame(x = runif(100, -3, 3), I = sample(x = c(0,1), size = 100, replace = T))

a1 <- 2
a2 <- 3
b1 <- 0.1
b2 <- 0.4

for (i in 1:100){
  if( d$I[i]==0 )
    d$y[i] <- a1 + b1 * d$x[i] + rnorm(1, 0, 0.1)
  if( d$I[i]==1 )
    d$y[i] <- a2 + b2 * d$x[i] + rnorm(1, 0, 0.1)
}

d$I  <- factor(d$I)
m1   <- lm(y ~ I*x, d)
coef(m1)
# (Intercept) I2          x           I2:x 
# 1.99250389  1.00119907  0.09331408  0.31509120 

m2   <- lm(y ~ 0 + I*x, d)
coef(m2)
# I1         I2         x          I2:x 
# 1.99250389 2.99370296 0.09331408 0.31509120 

# YO QUISIERA QUE EL COEFICIENTE I2:X SEA b2 = 0.4
# O bien, como obtengo el CI para b2 conociendo el CI para b1 y (b2-b1) 
# que son los parametros que estoy estimando

