# FIN 9350 Wealth Management
# Final Project: Replicate Fama-French Three-Factor Model in R
# Changze Han

# data source: "ffdata.csv", Fidelity Magellan Fund from 2006 Jan. to 2010 Dec.
# data columns: 
#            Date, 
#            Mkt-RF(market excess returns, market factor),
#            SMB(small minus big, size factor), 
#            HML(high minus low, value factor), 
#            RF(risk free rate), 
#            FMAGX(return of Magellan)

# Step 1: Load data to R from CSV
ff_data <- read.table("ffdata.csv",header=TRUE,sep=",")

# Step 2: Extract Fama-French Factors and Fund Returns from CSV columns
rmrf <- ff_data[,2]/100 # 2nd column is the market excess return data, divide by 100 to remove %
smb <- ff_data[,3]/100 # 3rd column is the size factor data
hml <- ff_data[,4]/100 # 4th column is the value factor data
rf <- ff_data[,5]/100 # 5th column is the risk free rate data
fund <- ff_data[,6]/100 # 6th column is the monthly return of the Magellan fund

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
#           Min        1Q    Median        3Q       Max 
#     -0.043944 -0.008162 -0.000333  0.009172  0.042174 
# 
#     Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#     (Intercept) -0.002922   0.001946  -1.502  0.13881    
#     rmrf         1.210644   0.041743  29.002  < 2e-16 ***
#     smb          0.151090   0.086468   1.747  0.08606 .  
#     hml         -0.298928   0.073488  -4.068  0.00015 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#     Residual standard error: 0.01491 on 56 degrees of freedom
#     Multiple R-squared:  0.9509,	Adjusted R-squared:  0.9483 
#     F-statistic: 361.7 on 3 and 56 DF,  p-value: < 2.2e-16

