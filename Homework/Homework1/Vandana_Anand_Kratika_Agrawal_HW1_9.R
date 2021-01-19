#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework1 Question9
#--------------------------------------------------------------------------------------

setwd("C:/Users/Arpit/Desktop/Kratika/Semester2/DS502_Statistics/Assignments/Homework1/")
auto=read.table ("Auto.data",header =T,na.strings ="?")
auto=na.omit(auto)
fix(auto)
lm.fit =lm(formula = mpg ~ horsepower, data=auto)
summary(lm.fit)

#answer a.i
#Since the p-value is extremely small, i.e. <2e-16, the confidence interval is very high. 
#Thus, we can reject the null hypothesis as ??1 not equal to 0 and can say that a relationship exists between horsepower and mpg.

#answer a.ii
#The value of R-squared is 0.6059, i.e. 60.59% of variation in the model is explained by linear regression.
#Therefore, we can say that there is a strong relation between horsepower and mpg.

#answer a.iii
#As the value of horsepower coefficient is -0.157845, which is negative, the relation between horsepower and mpg is negative linear relation, i.e. with increase in horsepower value, mpg decreases.

#answer a.iv
predict(lm.fit ,data.frame(horsepower=98),interval ="confidence")
predict(lm.fit ,data.frame(horsepower=98),interval ="prediction")

#answer b
plot(auto$horsepower, auto$mpg)
abline(lm.fit, col="blue")

#answer c
plot(lm.fit)
#Plot of least square regression fit suggest that there exists a linear relationship between horsepower and mpg, 
#however the relation is not perfectly linear and consists of few non-linearities.
