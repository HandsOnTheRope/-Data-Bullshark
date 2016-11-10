



------------------------------------------------------------------------------
# LOG REG LOAN EXERCISE
# set working directory, install ggplot2, import csv
setwd("C:/Users/elrivas/Documents/Trainings/R Training")
install.packages("ggplot2")
library(ggplot2)
loandata <- read.csv("loandata.csv", header=TRUE)
# dummy variable, set default value to 0
loandata$interest_yn12 <- 0
# when interest rate > 12%, value = 1
loandata$interest_yn12[loandata$Interest.Rate.Clean>.12] <- 1
# examine relationship
ggplot( loandata, aes(x=FICO.Score, y=interest_yn12)) +
  geom_point() + xlab("FICO Score") + ylab("Interest Rate > 12% Indicator") + 
  ggtitle("FICO Score as Indicator of Low/High Interest") +
  stat_smooth( aes(y = interest_yn12),  method="glm", 
               method.args=list(family="binomial"), se=F) 

ggplot( loandata, aes(x=Monthly.Income, y=interest_yn12)) +
  geom_point() + xlim(c(0,5000)) + xlab("Monthly Income") + 
  ylab("Interest Rate > 12% Indicator") + 
  ggtitle("Monthly Income as Indicator of Low/High Interest") +
  stat_smooth( aes(y = interest_yn12),  method="glm",
               method.args=list(family="binomial"), se=F)

# run regression
logreg_int_fico_cl <- glm(formula= interest_yn12~ FICO.Score + Monthly.Income, 
        family=binomial, data=loandata)
# view results, theyre significant for both FICO Score and Monthly Income
summary(logreg_int_fico_cl)
# save log coefficients
ficomi_coef <- summary(logreg_int_fico_cl)$coefficients[c(2,3),1]
# convert to probability
exp(ficomi_coef)
# for every 1 unit increase in FICO Score, the odds of an interest rate > 12% 
        # DECREASE by a factor of 0.934
# for every $1 increase in Monthly Income, the odds of an interest rate > 12%
        # INCREASE by a factor of 1.000051
----------------------------------------------------------------------------
plot(interest_yn12~funded_ratio, data=loandata)
plot(interest_yn12~Debt.To.Income.Ratio, data=loandata)
plot(interest_yn12~Inquiries.in.the.Last.6.Months, data=loandata)
plot(interest_yn12~Employment.Length, data=loandata)
# code below for employment length cleaning
plot(interest_yn12~Loan.Length.Clean, data=loandata)
plot(interest_yn12~FICO.Score, data=loandata)
plot(interest_yn12~Monthly.Income, data=loandata, xlim=c(0,25000))
plot(interest_yn12~Revolving.CREDIT.Balance, data=loandata, xlim=c(0,100000))
plot(interest_yn12~Amount.Requested, data=loandata)
plot(interest_yn12~Open.CREDIT.Lines, data=loandata)
-----------------------------------------------------------------------------
# LIN REG LOAN EXERCISE
# set working directory
setwd("C:/Users/elrivas/Documents/Trainings/R Training")
# import csv
loandata <- read.csv("loandata.csv", header=TRUE)
# check data type, use interest.rate.clean becacuse is double, not integer
typeof(loandata$Interest.Rate)
typeof(loandata$Interest.Rate.Clean)
# plot data to see relationship
plot(loandata$Interest.Rate.Clean~loandata$FICO.Score, col="lightseagreen", xlab="FICO Score", ylab="Interest Rate", main="Interest Rates by FICO SCore")
# clearly linear, more so than the rltsp with other variables
linreg_int_fico <- lm(Interest.Rate.Clean~FICO.Score, data=loandata)
# run regression
summary(linreg_int_fico)
# summary returns a p-value of nearly zero (significant because < 5%)
# summary returns coefficient consistent with original plot, -0.8457
      # for every 1 unit increase in FICO score, the interest rate decreases by 
abline(linreg_int_fico)
# END LOAN EXERCISE
---------------------------------------------------------------------------
# multiple linear regression
  
linreg_int_fico_six <- lm(loandata$Interest.Rate.Clean~loandata$FICO.Score + loandata$Inquiries.in.the.Last.6.Months)
summary(linreg_int_fico_six)
---------------------------------------------------------------------------
plot(loandata$Interest.Rate.Clean~loandata$Revolving.CREDIT.Balance, xlim=c(0,100000))
plot(loandata$Interest.Rate.Clean~loandata$Open.CREDIT.Lines)
plot(loandata$Interest.Rate.Clean~loandata$Monthly.Income, xlim=c(0,25000))
plot(loandata$Interest.Rate.Clean~loandata$Inquiries.in.the.Last.6.Months)
plot(loandata$Interest.Rate.Clean~loandata$Debt.To.Income.Ratio)
---------------------------------------------------------------------------
linreg_int_six <- lm(loandata$Interest.Rate.Clean~loandata$Inquiries.in.the.Last.6.Months, data=loandata)
summary(linreg_int_six)
abline(linreg_int_six)
---------------------------------------------------------------------------
linreg_int_monthincome <- lm(loandata$Interest.Rate.Clean~loandata$Monthly.Income, data=loandata)
summary(linreg_int_monthincome)
abline(linreg_int_monthincome)
# not significant
---------------------------------------------------------------------------
loandata$funded_ratio <- (loandata$Amount.Funded.By.Investors/loandata$Amount.Requested)
loandata$funded_ratio
plot(loandata$Interest.Rate.Clean~loandata$funded_ratio)
linreg_int_fr <- lm(Interest.Rate.Clean~funded_ratio, data=loandata)
summary(linreg_int_fr)
abline(linreg_int_fr)
# not significant at 5%
---------------------------------------------------------------------------
typeof(loandata$Debt.To.Income.Ratio)
toString(loandata$Debt.To.Income.Ratio)
gsub("[[:punct:]]", "", loandata$Debt.To.Income.Ratio)
loandata$Debt.To.Income.Ratio <- as.double(loandata$Debt.To.Income.Ratio)
loandata$Debt.To.Income.Ratio <- ((loandata$Debt.To.Income.Ratio)/100)
loandata$Debt.To.Income.Ratio
plot(loandata$Interest.Rate.Clean~loandata$Debt.To.Income.Ratio)
linreg_int_dti <- lm(loandata$Interest.Rate.Clean~loandata$Debt.To.Income.Ratio, data=loandata)
summary(linreg_int_dti)
abline(linreg_int_dti)
# LOL very not significant
--------------------------------------------------------------------------
typeof(loandata$Employment.Length)
toString(loandata$Employment.Length)
loandata$Employment.Length <- gsub("[^0-9]", "", loandata$Employment.Length)
loandata$Employment.Length
loandata$Employment.Length <- as.double(loandata$Debt.To.Income.Ratio)