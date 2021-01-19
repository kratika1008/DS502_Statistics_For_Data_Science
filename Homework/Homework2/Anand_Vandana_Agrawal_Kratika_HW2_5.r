#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework2 Question5
#--------------------------------------------------------------------------------------
library("ISLR")
library (MASS)
library(class)
setwd("C:/Users/Arpit/Desktop/Kratika/Semester2/DS502_Statistics/Assignments/Homework2/")
head(Auto)
summary(Auto)

#answer a
mpg01 = rep(0,NROW(Auto))
mpg01[Auto$mpg>median(Auto$mpg)]=1
Auto_data = data.frame(Auto,mpg01)
table(mpg01)


#answer b
par(mfrow=c(3,3))
plot(cylinders ~ mpg01,data = Auto_data,main = "cyl-mpg01")
plot(displacement ~ mpg01,data = Auto_data,main = "disp-mpg01")
plot(horsepower ~ mpg01,data = Auto_data,main = "horsepwr-mpg01")
plot(weight ~ mpg01,data = Auto_data,main = "wt-mpg01")
plot(acceleration ~ mpg01,data = Auto_data,main = "acc-mpg01")
plot(year ~ mpg01,data = Auto_data,main = "year-mpg01")

boxplot(cylinders ~ mpg01, data = Auto_data,main = "cyl-mpg01")
boxplot(displacement ~ mpg01,data = Auto_data,main = "disp-mpg01")
boxplot(horsepower ~ mpg01,data = Auto_data,main = "horsepwr-mpg01")
boxplot(weight ~ mpg01,data = Auto_data,main = "wt-mpg01")
boxplot(acceleration ~ mpg01,data = Auto_data,main = "acc-mpg01")
boxplot(year ~ mpg01,data = Auto_data,main = "year-mpg01")


#answer c
split_set = sample(x=NROW(Auto_data), size=0.75*NROW(Auto_data))
training_set = Auto_data[split_set, ]
test_set = Auto_data[-split_set, ]


#answer d
lda.fit = lda(mpg01 ~ cylinders+displacement+horsepower+weight, data=training_set)
lda.fit
lda.pred = predict(lda.fit, test_set)
table(lda.pred$class,test_set$mpg01)
mean(lda.pred$class != test_set$mpg01)
#Test Error: 0.1326531


#answer e
qda.fit = qda(mpg01 ~ cylinders+displacement+horsepower+weight, data=training_set)
qda.fit
qda.pred = predict(qda.fit, test_set)
table(qda.pred$class,test_set$mpg01)
mean(qda.pred$class != test_set$mpg01)
#Test Error: 0.122449


#answer f
glm.fit = glm(mpg01 ~ cylinders+displacement+horsepower+weight, data=training_set,family="binomial")
glm.fit
glm.probs=predict(glm.fit,test_set,type="response")
glm.pred=rep(0,NROW(glm.probs))
glm.pred[glm.pred>0.5]=1
table(glm.pred,test_set$mpg01)
mean(glm.pred!=test_set$mpg01)
#Test Error: 0.4081633



#answer g
train.X=cbind(training_set$cylinders,training_set$weight,training_set$displacement,training_set$horsepower)
test.X=cbind(test_set$cylinders,test_set$weight,test_set$displacement,test_set$horsepower)
train.mpg01=training_set$mpg01
set.seed(1)
knn.pred=knn(train.X,test.X,train.mpg01,k=1)
table(knn.pred,test_set$mpg01)
mean(knn.pred!=test_set$mpg01)
knn.pred=knn(train.X,test.X,train.mpg01,k=10)
table(knn.pred,test_set$mpg01)
mean(knn.pred!=test_set$mpg01)
knn.pred=knn(train.X,test.X,train.mpg01,k=50)
table(knn.pred,test_set$mpg01)
mean(knn.pred!=test_set$mpg01)
knn.pred=knn(train.X,test.X,train.mpg01,k=100)
table(knn.pred,test_set$mpg01)
mean(knn.pred!=test_set$mpg01)
