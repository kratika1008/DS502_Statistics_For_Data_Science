#Question 5

#Part A
library(ISLR)
library(boot)

deltaVar = rep(NA, 10)
for (i in 1:10){
	fit = glm(wage~poly(age,i), data=Wage)
	deltaVar[i] = cv.glm(Wage, fit, K=10)$delta[1]
}

plot(1:10, deltaVar, xlab="Degree", ylab="Test MSE", type="l")
d.min=which.min(deltaVar)
points(which.min(deltaVar), deltaVar[which.min(deltaVar)], col="red", cex=2, pch=20)


fit1 = lm(wage~age, data=Wage)
fit2 = lm(wage~poly(age, 2), data=Wage)
fit3 = lm(wage~poly(age, 3), data=Wage)
fit4 = lm(wage~poly(age, 4), data=Wage)
fit5 = lm(wage~poly(age, 5), data=Wage)
anova(fit1,fit2,fit3,fit4,fit5)


plot(wage~age, data=Wage, col="black")
ageRange=range(Wage$age)
ages=seq(from=ageRange[1], to=ageRange[2])
fit=lm(wage~poly(age,3), data=Wage)
prediction=predict(fit, newdata=list(age=ages))
lines(ages, prediction, col="red", lwd=2)

#Part B
deltaVar2 = rep(NA, 10)
for (i in 2:10){
	Wage$ageCutoff=cut(Wage$age, i)
	fit = glm(wage~ageCutoff, data=Wage)
	deltaVar2[i] = cv.glm(Wage, fit, K=10)$delta[1]
}

plot(2:10, deltaVar2[-1], xlab="Number of Cuts", ylab="Test MSE", type="l")
d.min=which.min(deltaVar2)
points(which.min(deltaVar2), deltaVar2[which.min(deltaVar2)], col="red", cex=2, pch=20)

plot(wage~age, data=Wage, col="black")
ageRange2=range(Wage$age)
ages2=seq(from=ageRange2[1], to=ageRange2[2])
fit=glm(wage~poly(age,8), data=Wage)
prediction=predict(fit, data.frame(age=ages2))
lines(ages2, prediction, col="red", lwd=2)
