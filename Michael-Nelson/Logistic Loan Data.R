#Logistic Loans Exercise

#Import Data

library(readr)
loansData_clean2 <- read_csv("C:/Users/michnelson/Desktop/Analytics Training/R Exercise/loansData_clean2.csv", col_types = cols(`Debt.To.Income.Ratio (%)` = col_number(), Fico.Category = col_factor(levels = c("Exceptional", "Very Good", "Good", "Fair")),Interest.Rate = col_number(), `Loan.Length (mo.)` = col_factor(levels = c("36","60"))))
View(loansData_clean2)

#Create Dummy variable for interest rate --> 1 if r >= 12%, 0 if < 12%

loansData_clean2$r <- loansData_clean2$Interest.Rate
for (i in 1:length(loansData_clean2$r)){
if (loansData_clean2$r[i] >= 12) {loansData_clean2$r[i] <- 1} else {loansData_clean2$r[i] <- 0}}


# Install package "mfx" to run logisitic regressions
# install.packages("mfx") 

# Run logit model at means with marginal effects

library(mfx)
logitmfx(loansData_clean2$r ~ loansData_clean2$Amount.Requested + loansData_clean2$`Loan.Length (mo.)` + loansData_clean2$Monthly.Income + loansData_clean2$FICO.Score, data = loansData_clean2)


