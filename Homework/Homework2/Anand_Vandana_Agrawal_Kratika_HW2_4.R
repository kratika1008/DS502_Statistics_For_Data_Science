#part a
library(ISLR)
summary(Weekly)

plot(Weekly)

Weekly[,]

#part b
glm.fits=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5 + Volume, family=binomial, data = Weekly)
summary(glm.fits)

#part c
glm.probs=predict(glm.fits,type="response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Weekly$Direction)

#part d
train=(Weekly$Year<2009)
Weeklydata=Weekly[!train,]
fit.glm2=glm(Direction ~ Lag2, data = Weekly, family=binomial, subset=train)
summary(fit.glm2)
glm.probs2=predict(fit.glm2, Weeklydata, type="response")
pred.glm2=rep("Down",length(glm.probs2))
pred.glm2[glm.probs2>0.5]="Up"
table(pred.glm2,Weeklydata$Direction)

#part e
library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly, subset=train)
lda.pred = predict(lda.fit, Weeklydata)
table(lda.pred$class, Weeklydata$Direction)

#part f
qda.fit = qda(Direction ~ Lag2, data = Weekly, subset=train)
qda.pred = predict(qda.fit, Weeklydata)$class
table(qda.pred, Weeklydata$Direction)

#part g
library(class)
train.X = as.matrix(Weekly$Lag2[train])
test.X = as.matrix(Weekly$Lag2[!train])
train.Direction = Weekly$Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Weeklydata$Direction)

#part i

#logistic regression with Lag2*Volume
train=(Weekly$Year<2009)
Weeklydata=Weekly[!train,]
fit.glm2=glm(Direction ~ Lag2*Volume, data = Weekly, family=binomial, subset=train)
summary(fit.glm2)
glm.probs2=predict(fit.glm2, Weeklydata, type="response")
pred.glm2=rep("Down",length(glm.probs2))
pred.glm2[glm.probs2>0.5]="Up"
table(pred.glm2,Weeklydata$Direction)
mean(pred.glm2 == Weeklydata$Direction)

#QDA with Lag2*Volume
qda.fit = qda(Direction ~ Lag2*Volume, data = Weekly, subset=train)
qda.pred = predict(qda.fit, Weeklydata)$class
table(qda.pred, Weeklydata$Direction)
mean(qda.pred == Weeklydata$Direction)

#KNN with k = 32
library(class)
train.X = as.matrix(Weekly$Lag2[train])
test.X = as.matrix(Weekly$Lag2[!train])
train.Direction = Weekly$Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=32)
table(knn.pred, Weeklydata$Direction)
mean(knn.pred == Weeklydata$Direction)

#KNN with k = 100
library(class)
train.X = as.matrix(Weekly$Lag2[train])
test.X = as.matrix(Weekly$Lag2[!train])
train.Direction = Weekly$Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=100)
table(knn.pred, Weeklydata$Direction)
mean(knn.pred == Weeklydata$Direction)
