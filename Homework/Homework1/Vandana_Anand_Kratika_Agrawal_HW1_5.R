#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework1 Question5
#--------------------------------------------------------------------------------------

setwd("C:/Users/Arpit/Desktop/Kratika/Semester2/DS502_Statistics/Assignments/Homework1/")
auto=read.table ("Auto.data",header =T,na.strings ="?")
auto=na.omit(auto)
fix(auto)

#answer a
#quantitative variables : mpg, cylinders, displacement, horsepower, weight, accelearation
head(auto[,c(1:6),])
#qualitative variables : year, origin, name
head(auto[,c(7:9),])


#answer b
sapply(auto[,c(1:6),],range)

#answer c
sapply(auto[,c(1:6),],mean)
sapply(auto[,c(1:6),],sd)


auto$year=as.factor(auto$year)
auto$origin=as.factor(auto$origin)
auto$name=as.factor(auto$name)

#answer d
auto_X=auto[-c(10:85),]
fix(auto_X)
sapply(auto_X[,c(1:6),],range)
sapply(auto_X[,c(1:6),],mean)
sapply(auto_X[,c(1:6),],sd)

#answer e
pairs(auto)
#Plot of all predictors provides a clear picture of their relationship with one another. 
plot(auto$cylinders,auto$mpg)
#mpg is inversely related to the number of cylinders.
plot(auto$horsepower,auto$weight)
plot(auto$weight,auto$mpg)
#Displacement, horsepower, weight seem to have proportional relationships with each other, 
#however inverse relation with mpg.

#answer f
#We can see that mpg is inversely related with displacement, horsepower and weight. 
#However with acceleration, year and origin, it increases at first and then becomes independent of these parameters, 
#while it continues to decrease with further increase in number of cylinders.
