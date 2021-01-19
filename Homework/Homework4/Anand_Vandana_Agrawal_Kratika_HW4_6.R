#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework4 Question6
#--------------------------------------------------------------------------------------

library(ISLR)

summary(Wage$age)
summary(Wage$maritl)
summary(Wage$jobclass)
summary(Wage$race)
summary(Wage$education)
summary(Wage$health)

par(mfrow = c(2,2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)
plot(Wage$education, Wage$wage)
plot(Wage$age, Wage$wage)

library(gam)

fit.0=gam(wage~lo(year,span=0.8)+s(age,5)+education,data=Wage)
fit.1=gam(wage~lo(year,span=0.8)+s(age,5)+education+jobclass,data=Wage)
fit.2=gam(wage~lo(year,span=0.8)+s(age,5)+education+maritl,data=Wage)
fit.3=gam(wage~lo(year,span=0.8)+s(age,5)+education+jobclass+maritl,data=Wage)

anova(fit.0,fit.1,fit.2,fit.3)

par(mfrow = c(3,2))
par(mar=c(1,1,1,1))
plot(fit.3,se=T,col="red")