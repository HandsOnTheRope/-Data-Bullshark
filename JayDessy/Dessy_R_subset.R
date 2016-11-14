mydata <- read.csv(file="C:\\Users\\jdessy\\Documents\\R\\loansData_clean.csv",
                   sep=",", 
                   dec=",", 
                   stringsAsFactors=FALSE)
mydata$Amount.Funded.By.Investors = as.numeric(mydata$Amount.Funded.By.Investors)
mydata$Interest.Rate.Clean = as.numeric(mydata$Interest.Rate.Clean)

mydata$IRpaid = mydata$Amount.Funded.By.Investors * mydata$Interest.Rate.Clean

IRpaid1 = subset(mydata, mydata$Interest.Rate.Clean <0.1)
IRpaid2 = subset(mydata, mydata$Interest.Rate.Clean >= 0.1 & mydata$Interest.Rate.Clean < 0.15)
IRpaid3 = subset(mydata, mydata$Interest.Rate.Clean >0.15)

hist(IRpaid1$IRpaid, main="Histogram of Interest Paid when Interest Rate is less than 10%", xlab = "Interest Paid ($)")
hist(IRpaid2$IRpaid, main="Histogram of Interest Paid when Interest Rate is between 10% and 15%", xlab = "Interest Paid ($)")
hist(IRpaid3$IRpaid, main="Histogram of Interest Paid when Interest Rate is greater than 15%", xlab = "Interest Paid ($)")


