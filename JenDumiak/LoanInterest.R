# Jen Dumiak
# November 10, 2016
# Simple Interest

#load in the data file 
loans.file <- read.csv("C:/Users/jdumiak/Desktop/R/loansData_clean.csv", header = TRUE)

#calculate simple interest on loan, A = P(1 + rt)
loans.file$Loan.Simple.Interest <- loans.file$Amount.Funded.By.Investors*(1 + loans.file$Interest.Rate.Clean*loans.file$Loan.Length.Clean)

#partition data 
fivetoten <- subset(loans.file, loans.file$Interest.Rate.Clean >= .05 & loans.file$Interest.Rate.Clean <.10)
tentofifteen <- subset(loans.file, loans.file$Interest.Rate.Clean >= .10 & loans.file$Interest.Rate.Clean <.15)
fifteenplus <- subset(loans.file, loans.file$Interest.Rate.Clean >= .15)

#hist of interest
hist(fivetoten$Loan.Simple.Interest, breaks = 20, main = "Histogram of Interest in 5-10% Range", prob= TRUE, xlab = "Simple Interest on Loans", col = "thistle2")
#overlay exponential distribution
x.est <- fitdistr(fivetoten$Loan.Simple.Interest, "exponential")$estimate
curve(dexp(x, rate = x.est), add = TRUE, col = "red", lwd = 2, lty=2)

hist(tentofifteen$Loan.Simple.Interest, breaks = 20, main = "Histogram of Interest in 10-15% Range", prob= TRUE, xlab = "Simple Interest on Loans", col = "thistle3")


hist(fifteenplus$Loan.Simple.Interest, breaks = 20, main = "Histogram of Interest in +15% Range", prob= TRUE, xlab = "Simple Interest on Loans", col="thistle4")
#overlay exponential distribution
x2.est <- fitdistr(fifteenplus$Loan.Simple.Interest, "exponential")$estimate
curve(dexp(x, rate = x2.est), add = TRUE, col = "red", lwd = 2, lty=2)


