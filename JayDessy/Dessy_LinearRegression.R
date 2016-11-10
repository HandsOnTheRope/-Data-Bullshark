mydata <- read.csv(file="C:\\Users\\jdessy\\Documents\\R\\loansData_clean.csv",
                  sep=",", 
                  dec=",", 
                  stringsAsFactors=FALSE)

IR = mydata$Interest.Rate.Clean
FICO = mydata$FICO.Score 
Requested = mydata$Amount.Requested
LoanLength = mydata$Loan.Length.Clean

model<- lm( IR ~ FICO + Requested + LoanLength, data = mydata )
coefficients(model)
#plot(IR, residuals(model))
plot(FICO, IR)

myvars <- c("Interest.Rate.Clean", "FICO.Score", "Amount.Requested", "Loan.Length.Clean")
mydata2 <- mydata[myvars]
plot(mydata2)
