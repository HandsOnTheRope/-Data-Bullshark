# Loan Interest Rates

# Import and Review Data
library(readr)
loansData_clean <- read_csv("C:/Users/michnelson/Desktop/loansData_clean.csv", 
  col_types = cols(`Debt.To.Income.Ratio (%)` = col_number(), 
    Fico.Category = col_factor(levels = c("Exceptional","Very Good", "Good", "Fair")), 
  Interest.Rate = col_number(), `Loan.Length (mo.)` = col_factor(levels = c("36", "60"))))
View(loansData_clean)

summary(loansData_clean)

# Predict Loan Interest Rate with linear regression.
# Explanatory Variables = Amount Requested, Loan Length, FICO Score, FICO Rating, Monthly Income
y <- lm(loansData_clean$Interest.Rate ~ loansData_clean$Amount.Requested + loansData_clean$`Loan.Length (mo.)` + loansData_clean$FICO.Score +loansData_clean$Fico.Category + loansData_clean$Monthly.Income, data = loansData_clean)
summary(y)

plot(y)

# Predict Loan Interest Rate while specifically looking at variable coefficients.
# To avoid multicollinearity, explanatory variables = Amout Requested, Loan Length, FICO Score, Monthly Income
y <- lm(loansData_clean$Interest.Rate ~ loansData_clean$Amount.Requested + loansData_clean$`Loan.Length (mo.)` + loansData_clean$FICO.Score + loansData_clean$Monthly.Income, data = loansData_clean)
summary(y)
