#--------------------------------------------------------------------------------------
#Vandana Anand
#Kratika Agrawal
#Homework2 Question7
#--------------------------------------------------------------------------------------


#answer g
x = 1:100000
y = 1 - (1 - 1/x)^x
plot(x, y,main="Prob of jth observation being in bootstrap sample")


#answer h
store=rep (NA , 10000)
for (i in 1:10000) {store[i]=sum(sample (1:100 , rep =TRUE)==4) >0}
mean(store)