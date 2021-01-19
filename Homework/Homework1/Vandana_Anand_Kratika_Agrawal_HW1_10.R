#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework1 Question10
#--------------------------------------------------------------------------------------


setwd("C:/Users/Arpit/Desktop/Kratika/Semester2/DS502_Statistics/Assignments/Homework1/")
auto=read.table ("Auto.data",header =T,na.strings ="?")
auto=na.omit(auto)
fix(auto)


#answer a. 
pairs(auto)


#answer b.
auto$year=as.numeric(auto$year)
auto$origin=as.numeric(auto$origin)
cor(auto[,!(names(auto)=="name")])

#answer c.
lm.fit = lm(formula= auto$mpg ~.,data=auto[,!(names(auto)=="name")])
summary(lm.fit)

#i.
#Overall p-value is very small, i.e. 2.2e-16, which shows that there exists a relation between various predictors and mpg

#ii
#The p-values for predictors displacement, weight, year and origin are less that 0.05, thus they have statistically significant relationship to the response.

#iii
#Coefficient for the year is 0.750773, which shows that with an increase of each year, mpg is estimated to increase by 0.75.

#answer d.
plot(lm.fit)
#Residuals vs Fitted plot shows that there exists some non-linearilty in the data and there are some outliers in the plot as shown in Scale-location plot.

#answer e
lm.fit_interation <- lm(auto$mpg ~ auto$cylinders * auto$displacement+auto$displacement * auto$weight, data=auto[,!(names(auto)=="name")])
summary(lm.fit_interation)
#Interaction with weight and displacement improved the relation as the p-value is smaller with its introduction.

#answer f
par(mfrow = c(2, 2))
plot(log(auto$horsepower), auto$mpg)
plot(sqrt(auto$weight), auto$mpg)
plot((auto$displacement)^2, auto$mpg)
#log and sqrt term fit the linear model well as compared to squared function