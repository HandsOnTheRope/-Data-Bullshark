plot(FICO.Score, Interest.Rate.Clean, main="Scatterplot")
abline(lm(Interest.Rate.Clean~FICO.Score))
mod1 <- lm(Interest.Rate.Clean ~ FICO.Score + Amount.Requested + Loan.Length.Clean)
summary(mod1)

loansData_clean$Loan.Purpose <- as.factor(loansData_clean$Loan.Purpose)
mod2 <- lm(Interest.Rate.Clean ~ FICO.Score + Amount.Requested + Loan.Length.Clean + Loan.Purpose)
summary(mod2)
creditcarddata <- subset(loansData_clean, Loan.Purpose=="credit_card")


#logistic regression
loansData_clean$high.interest.rate <- ifelse(loansData_clean$Interest.Rate.Clean >= .12, 1, 0) 
mod3 <- glm(high.interest.rate ~ FICO.Score + Amount.Requested + Loan.Length.Clean, data = loansData_clean)

#create graph
install.packages("ggplot2")
library(ggplot2) 
ggplot(mod3, aes(x=FICO.Score, y=high.interest.rate)) + geom_point()
prob <- predict(mod3)
ggplot(prob, aes(x=FICO.Score, y=high.interest.rate)) + geom_point()
  
)
