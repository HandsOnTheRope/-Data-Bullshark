loansData$InterestPaid = loansData$Amount.Funded.By.Investors * loansData$Interest.Rate.Clean
summary (loansData$InterestPaid)

SubsetLessThan10 = subset(loansData, loansData$Interest.Rate.Clean < .1)
SubsetBetween10And15 = subset(loansData, loansData$Interest.Rate.Clean >= .1 & loansData$Interest.Rate.Clean < .15)
subsetGreaterThan15 = subset(loansData, loansData$Interest.Rate.Clean >= .15)
hist(SubsetLessThan10$InterestPaid)
hist(SubsetBetween10And15$InterestPaid)
hist(subsetGreaterThan15$InterestPaid)