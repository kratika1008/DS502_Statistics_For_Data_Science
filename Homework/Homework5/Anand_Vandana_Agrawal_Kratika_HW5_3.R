#HW 5 Question 3

# part a

plot(NA,NA, type='n', xlim=c(-4,2), ylim= c(-1,5), asp = 1, xlab="X1", ylab="X2")
symbols(c(-1), c(2), circles = c(2), add =TRUE, inches = FALSE)

#part b

plot(NA,NA, type='n', xlim=c(-4,2), ylim= c(-1,5), asp = 1, xlab="X1", ylab="X2")
symbols(c(-1), c(2), circles = c(2), add =TRUE, inches = FALSE)
text(c(-1), c(2), '<= 4')
text(c(-4), c(2), '> 4')

#part c

plot(c(0,-1, 2, 3), c(0,1,2,8), col = c("red", "green", "red", "red"),
type='p', asp = 1, xlab="X1", ylab="X2")
symbols(c(-1), c(2), circles = c(2), add =TRUE, inches = FALSE)