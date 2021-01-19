#Question 2

#Part A
library(MASS)
library(leaps)
library(glmnet)
library(pls)
data(Boston)

#Best Subset selection

predict.regsubsets=function(object,newdata,id,...){
	form = as.formula(object$call[[2]])
	mat=model.matrix(form,newdata)
	coefi=coef(object,id=id)
	xvars=names(coefi)
	mat[,xvars]%*%coefi
}

k=10
folds=sample(1:k,nrow(Boston), replace=TRUE)
cv.errors=matrix(NA,k,13, dimnames=list(NULL,paste(1:13)))

for(i in 1:k){
	best.fit=regsubsets(crim~.,data=Boston[folds != i, ],nvmax=13)
	for(j in 1:13){
		pred=predict(best.fit,Boston[folds == i, ], id=j)
		cv.errors[i,j]=mean((Boston$crim[folds == i] -pred)^2)
	}
}

mean.cv.errors = apply(cv.errors,2,mean)
plot(mean.cv.errors, type="b", xlab= "Number of Variables", ylab="CV Error")
mean.cv.errors[which.min(mean.cv.errors)]

#Ridge Regression
x=model.matrix(crim~.,Boston)[,-1]
y=Boston$crim
cv.ridge = cv.glmnet(x,y,alpha=0,type.measure="mse")
plot(cv.ridge)
cv.ridge

#Lasso
cv.lasso = cv.glmnet(x,y,alpha=1,type.measure="mse")
plot(cv.lasso)
cv.lasso

#PCR
pcr.fit=pcr(crim~., data=Boston, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
