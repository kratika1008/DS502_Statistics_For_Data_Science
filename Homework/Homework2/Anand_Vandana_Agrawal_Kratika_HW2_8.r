#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework2 Question8
#--------------------------------------------------------------------------------------


library(ISLR)
set.seed(2)

#answer a
glm.fit=glm(default ~ income+balance, family='binomial',data=Default)
glm.fit


#answer b - i
train = sample(NROW(Default), NROW(Default)/2)

#answer b - ii
glm.fit2 = glm(default ~ income + balance, data=Default, family="binomial",subset = train)
glm.fit2

#answer b - iii
glm.probs2=predict(glm.fit2,Default[-train,],type="response")
glm.pred2=rep('No',NROW(glm.probs2))
glm.pred2[glm.probs2>0.5]='Yes'
table(glm.pred2,Default[-train,'default'])

#answer b - iv
mean(glm.pred2!=Default[-train,'default'])


#answer c
lr_eval = function() {
       train2 = sample(NROW(Default), NROW(Default)/2)
       glm.fit3 = glm(default ~ income + balance, data = Default, family = binomial, subset = train2)
       glm.probs3 = predict(glm.fit3,Default[-train2,],type="response")
       glm.pred3 = rep("No",NROW(glm.probs3))
       glm.pred3[glm.probs3 > 0.5] = "Yes"
       val_er = mean(glm.pred3 != Default[-train2, ]$default)
       return(val_er)
   }
lr_eval()
lr_eval()
lr_eval()


#answer d
train3 = sample(NROW(Default), NROW(Default)/2)
glm.fit4=glm(default ~ income+balance+student,data=Default,family=binomial,subset=train3)
glm.probs4=predict(glm.fit4,Default[-train3,],type="response")
glm.pred4=rep("No",NROW(glm.probs4))
glm.pred4[glm.probs4>0.5]="Yes"
table(glm.pred4,Default[-train3,'default'])
mean(glm.pred4 != Default[-train3,]$default)