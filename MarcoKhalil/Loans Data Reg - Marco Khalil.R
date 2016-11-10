interestRateCleanLinReg = lm(loansData$Interest.Rate.Clean ~ loansData$Monthly.Income + loansData$Home.Ownership
            + loansData$FICO.Score + loansData$Loan.Length.Clean)
summary(interestRateCleanLinReg)

plot(interestRateCleanLinReg)
abline(interestRateCleanLinReg)

#creating Dummy Interest Rate Variable
loansData$Interest.Rate.Dummy = ifelse(loansData$Interest.Rate.Clean >= .12, 1, 0)
summary(loansData$Interest.Rate.Dummy)

interestRateDummyLogReg = glm(loansData$Interest.Rate.Dummy ~ loansData$Monthly.Income + 
                                loansData$Home.Ownership + loansData$FICO.Score + 
                                loansData$Loan.Length.Clean, family = binomial)
summary(interestRateDummyLogReg)
#plot(interestRateDummyLogReg)
