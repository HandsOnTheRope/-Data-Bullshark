# Load data and packages

setwd("C:/Users/elrivas/Documents/Trainings/R Training")
install.packages("ggplot2")
library(ggplot2)
loandata <- read.csv("loandata.csv", header=TRUE)
---------------------------------------------------------
# generate new variable, round to 2 decimal place to reflect dollars
loandata$simple_interest <- round((loandata$Interest.Rate.Clean*
                                     loandata$Amount.Funded.By.Investors),2)
# 3 subsets
# first subset btw 5-10%, then check
interest_5_10 <- subset(loandata, Interest.Rate.Clean > .05 & Interest.Rate.Clean <=.10)
interest_5_10$Interest.Rate.Clean
# second subset btw 10-15%, then check
interest_10_15 <- subset(loandata, Interest.Rate.Clean >.10 & Interest.Rate.Clean <=.15)
interest_10_15$Interest.Rate.Clean
# third subset above 15%, then check
interest_15_plus <- subset(loandata, Interest.Rate.Clean >.15)
interest_15_plus$Interest.Rate.Clean
# check to see no observations were excluded in the 3 subsets
nrow(interest_5_10)+nrow(interest_10_15)+nrow(interest_15_plus) == nrow(loandata)
# returns TRUE

hist(interest_5_10$simple_interest, xlab="Simple Interest", ylab="Frequency", 
     main="Frequency of Simple Interest Paid for Interest Rates Between 5-10%")
hist(interest_10_15$simple_interest, xlab="Simple Interest", ylab="Frequency", 
     main="Frequency of Simple Interest Paid for Interest Rates Between 10-15%")
hist(interest_15_plus$simple_interest, xlab="Simple Interest", ylab="Frequency", 
     main="Frequency of Simple Interest Paid for Interest Rates Above 15%")