#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework3 Question4
#--------------------------------------------------------------------------------------

library("ISLR")
set.seed(3)

#answer a
split = sample(NROW(College), NROW(College)*0.7)
train = College[split,]
test = College[-split,]

#answer b
fit.lm = lm(Apps~., data=train)
fit.lm
pred.lm = predict(fit.lm, test)
err.lm = mean((test$Apps - pred.lm)^2)
err.lm


require(glmnet)
train.mat = model.matrix(Apps~., data = train)[,-1]
test.mat = model.matrix(Apps~., data = test)[,-1]
grid = 10^seq(4,-2,length=100)

#answer c
fit.ridge = cv.glmnet(train.mat, train$Apps, alpha=0,lambda=grid)
ridge.lambda = fit.ridge$lambda.min
ridge.lambda
pred.ridge = predict(fit.ridge, s=ridge.lambda, newx=test.mat)
err.ridge = mean((test$Apps - pred.ridge)^2)
err.ridge

#answer d
fit.lasso = cv.glmnet(train.mat, train$Apps, alpha=1,lambda=grid)
lasso.lambda = fit.lasso$lambda.min
lasso.lambda
pred.lasso = predict(fit.lasso, s=lasso.lambda, newx=test.mat)
err.lasso = mean((test$Apps - pred.lasso)^2)
err.lasso
coef.lasso = predict(fit.lasso, type="coefficients", s=lasso.lambda)[1:NCOL(College),]
coef.lasso[coef.lasso != 0]

require(pls)
set.seed(4)

#answer e
fit.pcr <- pcr(Apps~., data=train, scale=T, validation="CV")
validationplot(fit.pcr, val.type="MSEP")
#Visualizing the validation plot shows that MSEP is minimum for M=16
pred.pcr=predict(fit.pcr,test,ncomp=16)
err.pcr= mean((test$Apps-pred.pcr)^2)
err.pcr
summary(fit.pcr)

#answer f
fit.plsr <- plsr(Apps~., data=train, scale=T, validation="CV")
validationplot(fit.plsr, val.type="MSEP")
#Visualizing the validation plot shows that MSEP is minimum for M=10
pred.plsr=predict(fit.plsr,test,ncomp=10)
err.plsr= mean((test$Apps-pred.plsr)^2)
err.plsr
summary(fit.plsr)

#answer g
test.avg <- mean(test$Apps)

r2.lm = 1 - mean((pred.lm - test$Apps)^2) / mean((test.avg - test$Apps)^2)
r2.ridge = 1 - mean((pred.ridge - test$Apps)^2) / mean((test.avg - test$Apps)^2)
r2.lasso = 1 - mean((pred.lasso - test$Apps)^2) / mean((test.avg - test$Apps)^2)
r2.pcr = 1 - mean((pred.pcr - test$Apps)^2) / mean((test.avg - test$Apps)^2)
r2.plsr = 1 - mean((pred.plsr - test$Apps)^2) / mean((test.avg - test$Apps)^2)

r2.all = c(r2.lm, r2.ridge, r2.lasso, r2.pcr, r2.plsr)
r2.all
names(r2.all) = c("Linear", "Ridge", "Lasso", "PCR", "PLS")
barplot(r2.all,ylab="R2 value",main="R2 for various Plots on College Data")

err.all = c(err.lm, err.ridge, err.lasso, err.pcr, err.plsr)
names(err.all) = c("Linear", "Ridge", "Lasso", "PCR", "PLS")
err.all
