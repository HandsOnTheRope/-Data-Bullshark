#R-Training
#Cindy
#11/9/16

#Sections:
  # 1.Loading the data
  # 2.Linear regressions
  # 3.Logistic regressions
  # 4.New variable
  # 5.Creating data frames
  # 6.Histograms of data frames

############# LOADING THE DATA ############# 

#Install extra packages if don't have
  #install.packages("broom") #for better output of regressions
  #install.packages("ggplot2")
  #install.packages("car")
  library(ggplot2)
  

#Set working directory
  setwd("~/IRS Compliance/Trainings")
  
#Imports csv file
  #loan.data <- read.csv("loansData_clean.csv", header= TRUE, stringsAsFactors = FALSE)
  loan.data <- read.csv("C:/Users/cizheng/Documents/IRS OCA RAAS/Trainings/loansData_clean.csv", header=TRUE)

#Turn off scientific notation
  options(scipen = 500)
  
############# LINEAR REGRESSIONS ############# 
  
#Summary stats 
  str(loan.data)
  names(loan.data)
  summary(loan.data)
  class(Interest.Rate)
  cor(loan.data[c("FICO.Score","Interest.Rate.Clean", "Loan.Length.Clean")])

#Plots
  #Creating variable names
    Interest.Rate = loan.data$Interest.Rate.Clean       
    FICO = loan.data$FICO.Score          
  
  #Scatter plot of interest rate vs FICO score
    plot(Interest.Rate.Clean~FICO.Score, data=loan.data)
    abline(lm(Interest.Rate~FICO)) 
  
  #Box and whisker plot
    plot(Interest.Rate.Clean~Employment.Length, data=loan.data)
  
#Linear Regressions
  #Interest Rate vs FICO Score  
    linear.model <- lm(Interest.Rate.Clean~FICO.Score, data=loan.data)
    linear.model
    summary(linear.model)

#Multilinear Regressions
  #With all clean variables
    linear.model2 <-lm(Interest.Rate.Clean~
                         Amount.Requested + State + Inquiries.in.the.Last.6.Months +
                         Amount.Funded.By.Investors + Loan.Purpose + Home.Ownership +
                         Open.CREDIT.Lines + Employment.Length + Loan.Length.Clean + 
                         FICO.Score + Debt.To.Income.Ratio + Monthly.Income + Revolving.CREDIT.Balance,
                         data=loan.data)
    linear.model2
    summary(linear.model2)
  
  #With selected varialbes
    linear.model3 <-lm(Interest.Rate.Clean~
                         Amount.Requested + State + Inquiries.in.the.Last.6.Months +
                         Amount.Funded.By.Investors + Home.Ownership +
                         Open.CREDIT.Lines + Loan.Length.Clean + 
                         FICO.Score + Monthly.Income + Revolving.CREDIT.Balance,
                       data=loan.data)
    linear.model3
    summary(linear.model3)
  
  #With selected varialbes, take out state and revolving credit balance
    linear.model4 <-lm(Interest.Rate.Clean~
                         Amount.Requested + Inquiries.in.the.Last.6.Months +
                         Amount.Funded.By.Investors + Home.Ownership +
                         Open.CREDIT.Lines + Loan.Length.Clean + 
                         FICO.Score + Monthly.Income,
                       data=loan.data)
    linear.model4
    summary(linear.model4)
 
  #Variable selection rational:
    #Only selected cleaned data
    #FICO.Range excluded because had FICO.Score
    #Excluded employment length because correlated with income, not as informative
    #Kept homeownership because it's an important asaset used for collateral 
    #Removed debt to income ratio because accounted for with loan amount and income variables, want to avoild multicolinearity
    #Removed loan purpose because it wasn't significant but also didn't seemed to be limited reasons for why the reason for the loan should cause the interest rate to vary too much. Not correlated.
    #Removed revolving credit balance because it is correlated with your FICO score  

############# LOGISTIC REGRESSION ############# 
    
#Create dummy variable for interest rate (0 if i<12%, 1 if i>=12%)
  #Reference source: http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial2.html
    #Option 1:
    #loan.data$high.interest <- as.numeric(loan.data$Interest.Rate.Clean >=.12)
    
    #Option2: With ifelse
    loan.data$high.interest <- ifelse(loan.data$Interest.Rate.Clean >= .12, 1, 0)

#Logistic regressions
    #Reference sources: https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
      #http://www.statmethods.net/advstats/glm.html
    #Simple Logistic Regression
    log.model <- glm(high.interest~FICO, binomial, data=loan.data)
    summary(log.model)
    
    #Look at odds ratio
    
    #Logistic Regression: Adding in All Variables
    log.model2 <- glm(high.interest~FICO +  
                     Amount.Requested + State + Inquiries.in.the.Last.6.Months +
                     Amount.Funded.By.Investors + Home.Ownership +
                     Open.CREDIT.Lines + Loan.Length.Clean + 
                     FICO.Score + Monthly.Income + Revolving.CREDIT.Balance,
                     binomial, data=loan.data)
    summary(log.model2)
    
    #Logistic Regression: Selected Model 
    log.model3 <- glm(high.interest~FICO +  
                        Amount.Requested + Amount.Funded.By.Investors + Loan.Length.Clean +   #Loan Info
                        Inquiries.in.the.Last.6.Months + Monthly.Income,   #Personal Characteristics
                      binomial, data=loan.data)
    summary(log.model3)
    confint(log.model3)       # 95% CI for the coefficients
    exp(coef(log.model3))     # exponentiated coefficients
    exp(confint(log.model3))  # 95% CI for exponentiated coefficients
    predict(log.model3, type="response")    # predicted values
    residuals(log.model3, type="deviance")  # residuals
    
      #Variable Selection Criteria:
        #Need to influence the interest rate without influencing FICO score 
          #Excluded Home.Ownership, Open.Credit.Lines, Fico.Score, Revolving credit balance due to non independence
        #Categorized the remaining into loan info and personal characteristics. 
        #Excluded state because don't believe that different interest rate between states
    
#Results of model 3:
    # Estimate  Std. Error z value             Pr(>|z|)    
    # (Intercept)                    65.44901725  2.81266526  23.269 < 0.0000000000000002 ***
    #   FICO                           -0.10119930  0.00428572 -23.613 < 0.0000000000000002 ***
    #   Amount.Requested                0.00009278  0.00004232   2.192             0.028365 *  
    #   Amount.Funded.By.Investors      0.00007839  0.00004257   1.841             0.065574 .  
    # Loan.Length.Clean               0.11424520  0.01002005  11.402 < 0.0000000000000002 ***
    #   Inquiries.in.the.Last.6.Months  0.45417547  0.06790134   6.689      0.0000000000225 ***
    #   Monthly.Income                 -0.00008547  0.00002401  -3.559             0.000372 ***

#Interpretation:
    #A unit increase in the FICO score decreases the log odds by .101       
    
    #Analysis of deviance
    anova(log.model3, test="Chisq")
      #Results:     
      # Df Deviance Resid. Df Resid. Dev              Pr(>Chi)    
      # NULL                                            2497     3354.8                          
      # FICO                            1  1398.88      2496     1955.9 < 0.00000000000000022 ***
      #   Amount.Requested                1   360.78      2495     1595.1 < 0.00000000000000022 ***
      #   Amount.Funded.By.Investors      1     0.39      2494     1594.7             0.5320231    
      # Loan.Length.Clean               1   180.47      2493     1414.2 < 0.00000000000000022 ***
      #   Inquiries.in.the.Last.6.Months  1    46.84      2492     1367.4     0.000000000007696 ***
      #   Monthly.Income                  1    12.73      2491     1354.7             0.0003589 ***
    
      #Interpreting Results:
        # The difference between the null deviance and the residual deviance shows how our model is doing 
        # against the null model (a model with only the intercept). The wider this gap, the better.
        # Analyzing the table we can see the drop in deviance when adding each variable one at a time. 
        # Again, adding Pclass, Sex and Age significantly reduces the residual deviance. The other 
        # variables seem to improve the model less even though SibSp has a low p-value. A large p-value 
        # here indicates that the model without the variable explains more or less the same amount of 
        # variation. Ultimately what you would like to see is a significant drop in deviance and the AIC.
    
#Calculuating Predicted Probabilities
    #Create new dataframe
    #Reference Source: http://www.ats.ucla.edu/stat/r/dae/logit.htm
    newdata1 <- with(loan.data,
            data.frame(Interest.Rate.Clean=mean(loan.data$Interest.Rate.Clean), Amount.Requested = mean(Amount.Requested), Amount.Funded.By.Investors = mean(Amount.Funded.By.Investors), 
                       Loan.Length.Clean = mean(Loan.Length.Clean), Inquiries.in.the.Last.6.Months = mean(Inquiries.in.the.Last.6.Months,na.rm = TRUE),
                       Monthly.Income=mean(Monthly.Income,na.rm = TRUE), FICO = rep(FICO))) #need to add na.rm = TRUE bc those two vars have missing values.
    newdata1
    
    #Generate Predicted Values
    newdata2 <- cbind(loans.data, predict(log.model3, newdata = newdata1, type="link", se=TRUE))
    newdata2 <- within(newdata2, {
      PredictedProb <- plogis(fit)
      LL <- plogis(fit - (1.96 * se.fit))
      UL <- plogis(fit + (1.96 * se.fit))
    })
    
    ## view first few rows of final dataset
    head(newdata2)
    # Amount.Requested Amount.Funded.By.Investors Loan.Length.Clean Inquiries.in.the.Last.6.Months
    # 1          12406.5                   12001.57           41.2608                      0.9063251
    # 2          12406.5                   12001.57           41.2608                      0.9063251
    # 3          12406.5                   12001.57           41.2608                      0.9063251
    # 4          12406.5                   12001.57           41.2608                      0.9063251
    # 5          12406.5                   12001.57           41.2608                      0.9063251
    # 6          12406.5                   12001.57           41.2608                      0.9063251
    # Monthly.Income FICO        fit     se.fit residual.scale        UL         LL PredictedProb
    # 1       5688.931  735 -2.2013647 0.13331101              1 0.1256396 0.07851808    0.09962801
    # 2       5688.931  715 -0.1773788 0.07714932              1 0.4934589 0.41858339    0.45577121
    # 3       5688.931  690  2.3526036 0.10881423              1 0.9286331 0.89466727    0.91314096
    # 4       5688.931  695  1.8466072 0.09394731              1 0.8839874 0.84056935    0.86372825
    # 5       5688.931  695  1.8466072 0.09394731              1 0.8839874 0.84056935    0.86372825
    # 6       5688.931  670  4.3765896 0.18188451              1 0.9912775 0.98236532    0.98758785
 
    #Plotting Predicted Values
    #Resource for ggplot: http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial2.html
    # ggplot(newdata2, aes(x = Interest.Rate.Clean, y = PredictedProb)) +
    #   geom_ribbon(aes(ymin = LL, ymax = UL, fill = FICO), alpha = .2) +
    #   geom_line(aes(colour = FICO), size=1)
    # DOESN'T WORK
    
    
############# New Variable ############# 
#Create new variable in dataframe for the amount of interest paid on loan (simple interest) 
#Assuming simple interest = Amount.Funded.By.Investors*Interest.Rate.Clean*Loan.Length.Clean
    names(loan.data)  #Seeing all the variable names
    attach(loan.data) #indicate which dataset to reference
    loan.data$Interest.Paid <- Amount.Funded.By.Investors*Interest.Rate.Clean*Loan.Length.Clean
    detach(loan.data)
    
############# Creating Data Frames #############     
#Create 3 new data frames by interest rate: 1) 5%-10%, 2) 10%-15%, 3) >15%
    low.interest.data <- loan.data[loan.data$Interest.Rate.Clean<.1, ]
    med.interest.data <- loan.data[loan.data$Interest.Rate.Clean<.15 & loan.data$Interest.Rate.Clean >= .1, ]
    high.interest.data <- loan.data[loan.data$Interest.Rate.Clean>=.15, ]
    
############# Histograms of Data Frames #############
#Create histograms for amount of interest paid
    hist(low.interest.data$Interest.Paid)
    hist(med.interest.data$Interest.Paid)
    hist(high.interest.data$Interest.Paid)
    
    
    
    