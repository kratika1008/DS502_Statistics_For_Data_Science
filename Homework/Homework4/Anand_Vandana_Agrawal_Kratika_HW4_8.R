#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework4 Question8
#--------------------------------------------------------------------------------------

library(ISLR)
set.seed(3)

#answer a
split = sample(NROW(Carseats), NROW(Carseats)*0.7)
train = Carseats[split,]
test = Carseats[-split,]

#answer b
library('tree')
fit.tree = tree(Sales~., data=train)
summary(fit.tree)
plot(fit.tree)
text(fit.tree,pretty=0)

pred.tree = predict(fit.tree, test)
err.tree = mean((test$Sales - pred.tree)^2)
err.tree

#answer c
cv.tree = cv.tree(fit.tree)
min.tree=which.min(cv.tree$dev)
plot(cv.tree$size,cv.tree$dev,type='o')
points(min.tree,cv.tree$dev[min.tree],col="red",cex=2,pch=20)
min.tree
#pruning the tree at min.tree=8
tree.prune=prune.tree(fit.tree,best=8)
plot(tree.prune)
text(tree.prune,pretty=0)

pred.prune=predict(tree.prune,newdata=test)
err.prune = mean((test$Sales-pred.prune)^2)
err.prune

#answer d
library('randomForest')

fit.bag=randomForest(Sales~.,data=train,mtry=10,ntree=50,importance=T)
pred.bag=predict(fit.bag,test)
err.bag = mean((test$Sales-pred.bag)^2)
err.bag

importance(fit.bag)

#answer e
fit.rf=randomForest(Sales~.,data=train,mtry=5,ntree=20,importance=T)
pred.rf=predict(fit.rf,test)
err.rf = mean((test$Sales-pred.rf)^2)
err.rf

importance(fit.rf)