# HW5 Question 4

library(e1071)

# create the data separating the 2 classes
x=rnorm(100)
y= 8*x^5 + 4*x^2 +5 + rnorm(100)
class = sample(100,50)
y[class] = y[class] + 3
y[-class] = y[-class] - 3

plot(x[class], y[class], col = "purple", xlab = "X", ylab = "Y", ylim = c(-6,30))
points(x[-class], y[-class], col = "pink")

# fit the support vector classifier model
z = rep(-1,100)
z[class] = 1
data = data.frame(x=x, y=y, z=as.factor(z))
train = sample(100,50)
data.train=data[train,]
data.test=data[-train,]
svm.linear=svm(z~., data = data.train, kernel ="linear", cost=10)
plot(svm.linear, data.train)

table(predict = predict(svm.linear, data.train), truth=data.train$z)

# fit the support vector machine with a polynomial kernel
svm.polynomial = svm(z~., data = data.train, kernel ="polynomial", cost =10)
plot(svm.polynomial, data.train)

table(predict = predict(svm.polynomial, data.train), truth=data.train$z)

# fit the support vector machine with a radial kernel and a gamma of 1
svm.radial = svm(z~., data = data.train, kernel ="radial", gamma=1, cost =10)
plot(svm.radial, data.train)

table(predict = predict(svm.radial, data.train), truth=data.train$z)

#Apply models to test data

plot(svm.linear, data.test)
table(predict = predict(svm.linear, data.test), truth=data.test$z)

plot(svm.polynomial, data.test)
table(predict = predict(svm.polynomial, data.test), truth=data.test$z)

plot(svm.radial, data.test)
table(predict = predict(svm.radial, data.test), truth=data.test$z)
