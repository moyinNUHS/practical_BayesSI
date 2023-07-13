#Code to create distribution plots (as discussed in meeting 06/07/2023)

#Create data wish to plot
#Just to demonstrate the code for the plots
Vec1<-cbind(c(0.42, 1.2, 1.19), c(0.76, 0.77, 0.95))
colnames(Vec1)<-c("Coefficients", "std.err")
rownames(Vec1)<-c("Treatment 2", "Treatment 3", "Treatment 4")
Vec4<-cbind(c(-1.2, -0.8, 0.01), c(0.95, 0.73, 0.74))
colnames(Vec4)<-c("Coefficients", "std.err")
rownames(Vec4)<-c("Treatment 1", "Treatment 2", "Treatment 3")

#Allocate lower and upper bound of x-axis (possibility of coefficient values)
lcb <- -3
ucb <- 5

#Create x-axis vector
u <- seq(from = lcb,
         to = ucb,
         length.out = 1e+5)
#Create distribution for treatment 2
v2 <- dnorm(x = u,
            mean = Vec1[1, 1],
            sd = Vec1[1, 2])
#Create distribution for treatment 3
v3 <- dnorm(x = u,
            mean = (Vec1[2, 1]),
            sd = (Vec1[2, 2]))
#Create distribution for treatment 4
v4 <- dnorm(x = u,
            mean = (Vec1[3, 1]),
            sd = (Vec1[3, 2]))

#Plot 3 distributions on same figure
matplot(x = u,
        y = cbind(v2, v3, v4),
        type = "l",
        lty = 1,
        col = c("red", "blue", "green"),
        xlab = "Coefficient",
        ylab = "densities",
        main = "Reference: Treatment 1")
#Add vertical line at 0 for reference treatment
abline(v=0, col="gold")
#Add legend
legend(x = "topright",
       legend = paste("Treatment", 1:4),
       col = c("gold","red", "blue", "green"),
       lty = 1)


#Create distribution for treatment 1
v1 <- dnorm(x = u,
            mean = Vec4[1, 1],
            sd = Vec4[1, 2])
#Create distribution for treatment 2
v2 <- dnorm(x = u,
            mean = (Vec4[2, 1]),
            sd = (Vec4[2, 2]))
#Create distribution for treatment 3
v3 <- dnorm(x = u,
            mean = (Vec4[3, 1]),
            sd = (Vec4[3, 2]))

#Plot 3 distributions on same figure
matplot(x = u,
        y = cbind(v1, v2, v3),
        type = "l",
        lty = 1,
        col = c("gold", "red", "blue"),
        xlab = "Coefficient",
        ylab = "densities",
        main = "Reference: Treatment 4")
#Add vertical line at 0 for reference treatment
abline(v=0, col="green")
#Add legend
legend(x = "topright",
       legend = paste("Treatment", 1:4),
       col = c("gold", "red", "blue","green"),
       lty = 1)
      
