mydata <- read.csv(file="C:\\Users\\jdessy\\Documents\\R\\loansData_clean.csv",
                   sep=",", 
                   dec=",", 
                   stringsAsFactors=FALSE)

#name variables
IR = mydata$Interest.Rate.Clean
FICO = mydata$FICO.Score 
Requested = mydata$Amount.Requested
LoanLength = mydata$Loan.Length.Clean  

# create dummy categorical variable to ge generated when interest rate >= 0.12
mydata$IRdummy <- factor ( with ( mydata, ifelse ( ( IR >=0.12 ), 1 , 0 ) ),level=c(0,1) )

#run a logistic regression of the interest rate dummy 
mylogit <- glm(IRdummy ~ FICO + Requested + LoanLength, data = mydata, family = "binomial")

## Additional model evaluations
#library(pscl)
#pR2(mylogit)
#testlogit <- glm(IRdummy ~ FICO + LoanLength, data = mydata, family = "binomial")
#anova(mylogit,test ="Chisq")

#print(exp(coef(mylogit)))
#summary(mylogit)

mydata$Prob <- predict(mylogit, newdata = mydata, type = "response")
table(round(mydata$Prob,0), mydata$IRdummy)



