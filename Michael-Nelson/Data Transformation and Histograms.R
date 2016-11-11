# Simple Interest Calculation on LoansData_Clean2

# Import Data

library(readr)
loansData_clean2 <- read_csv("C:/Users/michnelson/Desktop/Analytics Training/R Exercise/loansData_clean2.csv")
View(loansData_clean2)

# Calculate new variable, Simple Interest, as Amount Requested * Interest Rate
Simple.Interest <- loansData_clean2$Amount.Requested*loansData_clean2$Interest.Rate.Clean

# Add Simple Interest to data table
loansData_clean2["Simple.Interest"] <- Simple.Interest

# Create three data subsets for observations with interest rates <10%, 10 - 15%, and >15%.
a <- subset(loansData_clean2, loansData_clean2$Interest.Rate.Clean < 0.1)
b <- subset(loansData_clean2, loansData_clean2$Interest.Rate.Clean >= 0.1 & loansData_clean2$Interest.Rate.Clean <= 0.15)
c <- subset(loansData_clean2, loansData_clean2$Interest.Rate.Clean > 0.15)

# Create an ugly histogram for all three data subsets that measures Simple Interest.
hist(a$Simple.Interest)
hist(b$Simple.Interest)
hist(c$Simple.Interest)

