## Salary data
## Data extracted from Kaggle

## Set working directory
setwd("~/Documents/UWaterloo_courses/Lan_course_material/data/SLR")
options(digits=10)

## Read data
salarydat = read.csv("salary_dat.csv", header=T)
#View(salarydat) ## View our data in another window
head(salarydat) ## View only the first 6 rows in the Console (useful when data are too big)

## Check data description
summary(salarydat) ## Description of the min, max, mean, median and quantiles
?mean ## Allows us to see what the mean() function does

## Data exploration
plot(salarydat$YearsExperience, salarydat$Salary, xlab = "Experience (in years)", ylab = "Salary (in $)", ylim=c(0,150000), main="A Scatterplot of Salary data") ## Clear linear relationship

## Calculate correlation coefficient in R
r = cor(salarydat$YearsExperience, salarydat$Salary); r

## Calculate r manually
xbar = mean(salarydat$YearsExperience)
ybar = mean(salarydat$Salary)
sd_x = sd(salarydat$YearsExperience)
sd_y = sd(salarydat$Salary)
Sxx = sum( (salarydat$YearsExperience - xbar)^2 ) 
Syy = sum( (salarydat$Salary - ybar)^2 )
Sxy = sum( (salarydat$YearsExperience - xbar) * (salarydat$Salary- ybar) )
r_man = Sxy/sqrt(Sxx*Syy); r_man

## Fit a simple linear regression
myfit = lm(Salary ~ YearsExperience, data = salarydat)
summary(myfit) ## Shows a summary of our fitted model

## Calculate manually
beta_1 = Sxy/Sxx
beta_0 = ybar - beta_1*xbar
beta_0; beta_1

## Interpret our results:
coefficients(myfit) ## Calls the coefficients of our model

plot(salarydat$YearsExperience, salarydat$Salary, xlab = "Experience (in years)", ylab = "Salary (in $)", ylim=c(0,150000), main="A Scatterplot of Salary data") ## Clear linear relationship
abline(lm(salarydat$Salary ~ salarydat$YearsExperience), col = "red") #adds line of best fit to the plot

## T-test

###################################################3
###################################################3

## Manually calculate SE(beta_1_hat):
## first calculate the sigma_sq_hat
n = nrow(salarydat)
sigma_sq_hat = sum(myfit$residuals^2) / (n-2)
SE_beta1 = sqrt( sigma_sq_hat/Sxx )
SE_beta1

## Manually to get the t-test statistic
## beta_1 = Sxy/Sxx from before
beta_1/SE_beta1

## 0.975 quantile t distribution 
qt(0.975 , n-2) # Notice that 97.5 quantile is much smaller than the t-value
## calculate p-value (pt() calculates CDF at 24.95 below)
(1-pt(24.95, n-2))*2 #24.95 is the t-test statistic for this sample


  
