#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework3 Question2
#--------------------------------------------------------------------------------------


#answer a
y=5
lambda=2
beta=seq(from=-10,to=10,by=0.1)
ridge = (y - beta)^2 + lambda * beta
plot(beta, ridge, pch = 20, xlab = "beta", ylab = "Ridge Optimization")
beta.est = y / (1 + lambda)
points(beta.est, (y - beta.est)^2 + lambda * beta.est^2, col = "red", pch = 4, lwd = 5)


#answer b
y1=5
lambda1=2
beta1=seq(from=-10,to=10,by=0.1)
lasso = (y1 - beta1)^2 + lambda1 * abs(beta1)
plot(beta1, lasso, pch = 20, xlab = "beta", ylab = "Lasso Optimization")
beta.est1 = y1 - lambda1 / 2
points(beta.est1, (y1 - beta.est1)^2 + lambda1 * abs(beta.est1), col = "blue", pch = 4, lwd = 5)
