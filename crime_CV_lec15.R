## Crime data example
## Data extracted from Deterrence and Incapacitation (Blumstein, A., Cohen, J., and Nagin, D., Eds.,). Washington, DC: National Academy of Sciences, pp. 270–335, 1978. 

#CrimeRate: # of offenses reported to police per million population
#Age: The number of males of age 14-24 per 1000 population
#Southern: Indicator variable for Southern states (0 = No, 1 = Yes)
#Education: Mean # of years of schooling x 10 for persons of age 25 or older
#Ex0: 1960 per capita expenditure on police by state and local government
#Ex1: 1959 per capita expenditure on police by state and local government
#LF: Labor force participation rate per 1000 civilian urban males age 14-24
#Males: The number of males per 1000 females
#Pop: State population size in hundred thousands
#NW: The number of non-whites per 1000 population
#UE1: Unemployment rate of urban males per 1000 of age 14-24
#UE2: Unemployment rate of urban males per 1000 of age 35-39
#IncIneq: The number of families per 1000 earning below 1/2 the median income


## Read data
crimedat = read.csv("crime.csv", header=T)
head(crimedat) ## View only the first 6 rows in the Console (useful when data are too big)
n = nrow(crimedat) ## number of observations

## Split data into train and validation
set.seed(12345)
train_ID = sample(1:n, size=round(n*0.80), replace=F)
train_dat = crimedat[train_ID,]
valid_dat = crimedat[-train_ID,]

## Calculate MSPE on two models with different variables included: first based on BIC from all subset regression
myfit1 = lm(CrimeRate ~ Age + Education + Ex0 + UE2 + IncIneq, data = train_dat)
pred1 = predict(myfit1, newdata = valid_dat)
sqrt( mean((valid_dat$CrimeRate - pred1)^2) ) #RMSPE
# mean( abs(valid_dat$CrimeRate - pred1) ) #Mean absolute error

# second based on adjusted Rsquared from all subset regression
myfit2 =  lm(CrimeRate ~ Age + Education + Ex0 + Males + UE1 + UE2 + IncIneq, data = train_dat)
pred2 = predict(myfit2, newdata = valid_dat)
sqrt( mean((valid_dat$CrimeRate - pred2)^2) ) #RMSPE
# mean( abs(valid_dat$CrimeRate - pred2) ) #Mean absolute error



## Perform K fold CV
K = 10; set.seed(12345)
folds = sample((1:n)%%K + 1) ## randomly creating folds based on K=10; default in sample() for size is n; default in sample() for replace=F
RMSPE1 = c()
RMSPE2 = c()

for (k in 1:K) {
        validSet <- crimedat[folds==k,] 
        trainSet <- crimedat[folds!=k,]
        m1 <- lm(CrimeRate ~ Age + Education + Ex0 + UE2 + IncIneq, dat=trainSet) 
        pred1 <- predict(m1, newdata = validSet)
        RMSPE1[k] <- sqrt(mean((validSet$CrimeRate - pred1)^2))
        
        m2 <- lm(CrimeRate ~ Age + Education + Ex0 + Males + UE1 + UE2 + IncIneq, dat=trainSet)
        pred2 <- predict(m2, newdata = validSet)
        RMSPE2[k] <- sqrt(mean((validSet$CrimeRate - pred2)^2))
}

RMSPE1
RMSPE2
mean(RMSPE1) 
mean(RMSPE2)



## LOOCV
K = n; set.seed(12345)
folds = 1:n 
RMSPE1 = c()
RMSPE2 = c()

for (k in 1:K) {
  validSet <- crimedat[folds==k,] 
  trainSet <- crimedat[folds!=k,]
  m1 <- lm(CrimeRate ~ Age + Education + Ex0 + UE2 + IncIneq, dat=trainSet) 
  pred1 <- predict(m1, newdata = validSet)
  RMSPE1[k] <- sqrt(mean((validSet$CrimeRate - pred1)^2))
  
  m2 <- lm(CrimeRate ~ Age + Education + Ex0 + Males + UE1 + UE2 + IncIneq, dat=trainSet)
  pred2 <- predict(m2, newdata = validSet)
  RMSPE2[k] <- sqrt(mean((validSet$CrimeRate - pred2)^2))
}

RMSPE1
RMSPE2
mean(RMSPE1) 
mean(RMSPE2)




