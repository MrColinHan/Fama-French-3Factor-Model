# FIN 9350 Wealth Management
# Final Project: Replicate Fama-French Three-Factor Model in R
# Changze Han

# data source: "ffdata.csv", Fidelity Magellan Fund from 2006 Jan. to 2010 Dec.
# data columns: 
#            Date: in the format of ‘yyyymm’
#            Mkt-RF: market excess returns, market factor
#            SMB: small minus big, size factor
#            HML: high minus low, value factor
#            RF: risk free rate
#            FMAGX: return of Magellan

# Step 1: Load data to R from CSV
ff_data <- read.table("ffdata.csv",header=TRUE,sep=",")

# Step 2: Extract Fama-French Factors and Fund Returns from CSV columns

# 2nd column is the market excess return data, divide by 100 to remove %
rmrf <- ff_data[,2] 
# 3rd column is the size factor data
smb <- ff_data[,3] 
# 4th column is the value factor data
hml <- ff_data[,4] 
# 5th column is the risk free rate data
rf <- ff_data[,5] 
# 6th column is the monthly return of the Magellan fund
fund <- ff_data[,6] 

# Step 3: Calculate Excess Returns for the Magellan fund
fund.xcess <- fund - rf # use the monthly return minus the risk free rate

# Step 4: Use 'lm' function to fit a liner regression model for the Fama-French 3 factor model
# dependent variable: excess return of the Magellan fund
# independent variable: three factors
ffregression <- lm(fund.xcess ~ rmrf + smb + hml)

# Step 5: Print summary of the regression results
print(summary(ffregression))

# Results: 

#     Residuals:
#           Min      1Q  Median      3Q     Max 
#       -4.3944 -0.8162 -0.0333  0.9172  4.2174 
# 
#     Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#     (Intercept) -0.29216    0.19456  -1.502  0.13881    
#     rmrf         1.21064    0.04174  29.002  < 2e-16 ***
#     smb          0.15109    0.08647   1.747  0.08606 .  
#     hml         -0.29893    0.07349  -4.068  0.00015 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#     Residual standard error: 1.491 on 56 degrees of freedom
#     Multiple R-squared:  0.9509,	Adjusted R-squared:  0.9483 
#     F-statistic: 361.7 on 3 and 56 DF,  p-value: < 2.2e-16

