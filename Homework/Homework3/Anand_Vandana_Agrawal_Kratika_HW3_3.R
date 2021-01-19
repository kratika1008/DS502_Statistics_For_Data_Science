#Homework3 Question3

X = rnorm(100)

E = rnorm(100)

Y = 3 + 5*X + 4*X^2 + 1*X^3 + E

library(leaps)

dataX = data.frame(X,Y)

#best subset method

bestsubset = regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = dataX, nvmax = 10)

bestsubsetSummary = summary(bestsubset)

par(mfrow = c(2,2))

plot(bestsubsetSummary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(which.min(bestsubsetSummary$cp), bestsubsetSummary$cp[which.min(bestsubsetSummary$cp)], col = "red", cex = 2, pch = 20)

plot(bestsubsetSummary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(which.min(bestsubsetSummary$bic), bestsubsetSummary$bic[which.min(bestsubsetSummary$bic)], col = "red", cex = 2, pch = 20)

plot(bestsubsetSummary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
points(which.max(bestsubsetSummary$adjr2), bestsubsetSummary$adjr2[which.max(bestsubsetSummary$adjr2)], col = "red", cex = 2, pch = 20)

coef(bestsubset, which.max(bestsubsetSummary$adjr2))

#forward method

forward = regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = dataX, nvmax = 10, method="forward")

forwardSummary = summary(forward)

par(mfrow = c(2,2))

plot(forwardSummary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(which.min(forwardSummary$cp), forwardSummary$cp[which.min(forwardSummary$cp)], col = "red", cex = 2, pch = 20)

plot(forwardSummary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(which.min(forwardSummary$bic), forwardSummary$bic[which.min(forwardSummary$bic)], col = "red", cex = 2, pch = 20)

plot(forwardSummary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
points(which.max(forwardSummary$adjr2), forwardSummary$adjr2[which.max(forwardSummary$adjr2)], col = "red", cex = 2, pch = 20)

coef(forward, which.max(forwardSummary$adjr2))

# backward method 

backward = regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = dataX, nvmax = 10, method="backward")

backwardSummary = summary(forward)

par(mfrow = c(2,2))

plot(backwardSummary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(which.min(backwardSummary$cp), backwardSummary$cp[which.min(backwardSummary$cp)], col = "red", cex = 2, pch = 20)

plot(backwardSummary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(which.min(backwardSummary$bic), backwardSummary$bic[which.min(backwardSummary$bic)], col = "red", cex = 2, pch = 20)

plot(backwardSummary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
points(which.max(backwardSummary$adjr2), backwardSummary$adjr2[which.max(backwardSummary$adjr2)], col = "red", cex = 2, pch = 20)

coef(backward, which.max(backwardSummary$adjr2))

#Lasso Method

library(glmnet)

lasso = model.matrix(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = dataX)[,-1]

crossvalLasso = cv.glmnet(lasso, Y, alpha=1)
plot(crossvalLasso)

bestLambda = crossvalLasso$lambda.min

lassoBestLambda = glmnet(lasso, Y, alpha = 1)
predict(lassoBestLambda, s = bestLambda, type = "coefficients")[1:11,]

#Generate response vector 

	#Best subset method

newY = 3 + 10*X^7 + E

datanewX = data.frame(X,newY)

bestsubset2 = regsubsets(newY ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = datanewX, nvmax = 10)

bestsubsetSummary2 = summary(bestsubset2)

par(mfrow = c(2,2))

plot(bestsubsetSummary2$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(which.min(bestsubsetSummary2$cp), bestsubsetSummary2$cp[which.min(bestsubsetSummary2$cp)], col = "red", cex = 2, pch = 20)

plot(bestsubsetSummary2$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(which.min(bestsubsetSummary2$bic), bestsubsetSummary2$bic[which.min(bestsubsetSummary2$bic)], col = "red", cex = 2, pch = 20)

plot(bestsubsetSummary2$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
points(which.max(bestsubsetSummary2$adjr2), bestsubsetSummary2$adjr2[which.max(bestsubsetSummary2$adjr2)], col = "red", cex = 2, pch = 20)

coef(bestsubset2, 1)
coef(bestsubset2, 2)
coef(bestsubset2, 4)

	#Lasso method
	
lasso2 = model.matrix(newY ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = datanewX)[,-1]

crossvalLasso2 = cv.glmnet(lasso2, newY, alpha=1)

bestLambda2 = crossvalLasso$lambda.min

lassoBestLambda2 = glmnet(lasso2, newY, alpha = 1)
predict(lassoBestLambda2, s = bestLambda2, type = "coefficients")[1:11,]

