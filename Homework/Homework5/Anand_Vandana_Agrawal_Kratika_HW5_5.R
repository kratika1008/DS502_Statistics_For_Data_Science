#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework5 Question5
#--------------------------------------------------------------------------------------

set.seed(11)

#answer a
x1=runif (500) - 0.5
x2=runif (500) - 0.5
y=1*( x1^2-x2^2 > 0)

#answer b
plot(x1,x2,xlab="X1",ylab="X2",col=(4-y),pch=(3-y))

#answer c
lr.fit = glm(y ~ x1+x2,family="binomial")
summary(lr.fit)

#answer d
data = data.frame(x1 = x1, x2 = x2, y = y)
data.probs = predict(lr.fit, data, type = "response")
data.preds = rep(0, 500)
data.preds[data.probs > 0.47] = 1
plot(data[data.preds == 1, ]$x1, data[data.preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[data.preds == 0, ]$x1, data[data.preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

#answer e
lr.nl.fit = glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), family = "binomial")
summary(lr.nl.fit)

#answer f
lr.nl.probs = predict(lr.nl.fit, data, type = "response")
lr.nl.preds = rep(0, 500)
lr.nl.preds[lr.nl.probs > 0.47] = 1
plot(data[lr.nl.preds == 1, ]$x1, data[lr.nl.preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[lr.nl.preds == 0, ]$x1, data[lr.nl.preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))


#answer g
library(e1071)
data$y = as.factor(data$y)
svm.fit = svm(y~x1+x2, data, kernel = "linear", cost = 0.01)
summary(svm.fit)
svm.preds = predict(svm.fit, data)
plot(data[svm.preds == 0, ]$x1, data[svm.preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[svm.preds == 1, ]$x1, data[svm.preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

#answer h
data$y = as.factor(data$y)
svm.nl.fit = svm(y~x1+x2, data, kernel = "radial", gamma = 1)
summary(svm.nl.fit)
svm.nl.preds = predict(svm.nl.fit, data)
plot(data[svm.nl.preds == 0, ]$x1, data[svm.nl.preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[svm.nl.preds == 1, ]$x1, data[svm.nl.preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))