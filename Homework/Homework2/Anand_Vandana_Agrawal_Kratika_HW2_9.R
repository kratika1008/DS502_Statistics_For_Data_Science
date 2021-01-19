library(ISLR)
summary(Default)

#part a
set.seed(1)
glm.fit= glm(default ~ income + balance, family=binomial, data= Default)
summary(glm.fit)

#part b
boot.fn = function(data, index_obs){
	glmfit2 = glm(default ~ income + balance, data=data, family="binomial", subset=index_obs)
	return (coef(glmfit2))
}

library(boot)
boot(Default, boot.fn, 50)