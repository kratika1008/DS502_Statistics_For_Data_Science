#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework1 Question4
#--------------------------------------------------------------------------------------


#answer a
setwd("C:/Users/Arpit/Desktop/Kratika/Semester2/DS502_Statistics/Assignments/Homework1/")
college = read.csv("College.csv")

#answer b
fix(college)
rownames (college)=college [,1]
fix(college)
college =college [,-1]
fix(college)

#answer c
#i
summary(college)
#ii
pairs(college[,1:10])
#iii
boxplot(college$Outstate,college$Private)
#iv
Elite =rep ("No",nrow(college ))
Elite [college$Top10perc >50]="Yes"
Elite =as.factor (Elite)
college =data.frame(college ,Elite)
summary(college)
boxplot(college$Outstate,college$Elite)
#v
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$Accept)
hist(college$Enroll)
hist(college$Grad.Rate)

