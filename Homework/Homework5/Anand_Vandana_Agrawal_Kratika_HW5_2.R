#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework5 Question2
#--------------------------------------------------------------------------------------

library(ISLR)
set.seed(3)

#answer a
n_sample = sample(1:NROW(OJ),800)
train_OJ = OJ[n_sample,]
test_OJ = OJ[-n_sample,]

#answer b
library(tree)
oj.tree = tree(Purchase~.,data=train_OJ)
summary(oj.tree)

#answer c
oj.tree

#answer d
plot(oj.tree)
text(oj.tree,pretty=TRUE)

#answer e
oj.pred=predict(oj.tree,test_OJ,type="class")
table(test_OJ$Purchase,oj.pred)

#answer f
oj.cv=cv.tree(oj.tree,FUN=prune.tree)
oj.cv

#answer g
plot(oj.cv$size, oj.cv$dev, type = "b", xlab = "Tree size", ylab = "CV Classification Error Rate")

#answer i
oj.pruned=prune.tree(oj.tree,best=7)
plot(oj.pruned)
text(oj.pruned,pretty=TRUE)

#answer j
summary(oj.pruned)

#answer k
oj.pred.pruned=predict(oj.pruned,test_OJ,type="class")
table(test_OJ$Purchase,oj.pred.pruned)