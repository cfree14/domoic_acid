

dbeta_mode <- (a-1) / (a + B - 2)

a <- 2
b <- 5
beta_mode <- (a-1) / (a + b - 2)
beta_median <- (a - 1/3) / (a + b - 2.3)
beta_mean <- 1 / (1 + b/a)
x <- seq(0,1,0.01)
y <- dbeta(x=x, shape1=a, shape2=b)
plot(y ~ x, type="l")
abline(v=beta_mode)
abline(v=beta_median, lty=2)
abline(v=beta_mean, lty=3)
