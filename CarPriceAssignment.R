# Comment command below before submission 
# setwd("D:/pgdds/Regression/assignment")

#Import libraries here
library(MASS)
library(car)
library(ggplot2)
library(stringr)

##### Reading csv file to a dataframe ##### 
price_cars = read.csv('CarPrice_Assignment.csv')

View(price_cars)
str(price_cars)

# Dataset has 26 variable 
# 1 - Id variable
# 10 - factors
# And rest are numeric or integer
# there are no duplicates 
# there are no missing values 

###### Data Preparation #####
# Check for duplicates

length(unique(price_cars[,1])) == length(price_cars[,1])
nrow(unique(price_cars)) == nrow(price_cars)
# there are no duplicates in the dataset

# Check for NA in price_cars dataframe
sapply(price_cars, function(x)all(is.na(x)))
# Output ::: There are no missing values 
# symboling          CarName         fueltype       aspiration       doornumber          carbody 
# FALSE            FALSE            FALSE            FALSE            FALSE            FALSE 
# drivewheel   enginelocation        wheelbase        carlength         carwidth        carheight 
# FALSE            FALSE            FALSE            FALSE            FALSE            FALSE 
# curbweight       enginetype   cylindernumber       enginesize       fuelsystem        boreratio 
# FALSE            FALSE            FALSE            FALSE            FALSE            FALSE 
# stroke compressionratio       horsepower          peakrpm          citympg       highwaympg 
# FALSE            FALSE            FALSE            FALSE            FALSE            FALSE 
# price      car_company 
# FALSE            FALSE 

# Drop Id variable as that is not going to impact Car prices because that is an Id nominated for each car to identify them uniquely
price_cars <- price_cars[,c(2:26)]

# Create car_company 
price_cars$car_company <- word(price_cars$CarName,1)
price_cars$car_company <- as.factor(toupper(ifelse(price_cars$car_company %in% c('volkswagen', 'vokswagen', 'vw'), 'volkswagen',
       ifelse(price_cars$car_company %in% c('porsche','porcshce'), 'porsche',
              ifelse(price_cars$car_company %in% c('toyota','toyouta'), 'toyota',
                     ifelse(price_cars$car_company %in% c('maxda','mazda'), 'mazda', price_cars$car_company))))))

# drop variable CarName as we got car company now
price_cars <- price_cars[, !(colnames(price_cars) %in% 'CarName')]

##### dealing with factor variable with multiple levels #####
# Finding variables that are factor
factor_Vars <- names(price_cars)[sapply(price_cars, class) == "factor"]
str(price_cars[,factor_Vars])

# Create dummy variables
# first consider factors with 2 level
#fueltype
str(price_cars$fueltype)
summary(price_cars$fueltype)
levels(price_cars$fueltype) <- c(1,0)

price_cars$fueltype <- as.numeric(levels(price_cars$fueltype))[price_cars$fueltype]

#aspiration
str(price_cars$aspiration)
summary(price_cars$aspiration)
levels(price_cars$aspiration) <- c(1,0)

price_cars$aspiration <- as.numeric(levels(price_cars$aspiration))[price_cars$aspiration]

#doornumber
str(price_cars$doornumber)
summary(price_cars$doornumber)
levels(price_cars$doornumber) <- c(1,0)

price_cars$doornumber <- as.numeric(levels(price_cars$doornumber))[price_cars$doornumber]

#enginelocation
str(price_cars$enginelocation)
summary(price_cars$enginelocation)
levels(price_cars$enginelocation) <- c(1,0)

price_cars$enginelocation <- as.numeric(levels(price_cars$enginelocation))[price_cars$enginelocation]

# Lets deal with variable with more levels of factors
#carbody
str(price_cars$carbody)
summary(price_cars$carbody)
dummy_1 <- data.frame(model.matrix( ~carbody, data = price_cars))
dummy_1 <- dummy_1[,-1]

price_cars1 <- cbind(price_cars[,!(colnames(price_cars) %in% 'carbody')], dummy_1)

#drivewheel
str(price_cars1$drivewheel)
summary(price_cars1$drivewheel)
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = price_cars1))
dummy_2 <- dummy_2[,-1]

price_cars2 <- cbind(price_cars1[,!(colnames(price_cars1) %in% 'drivewheel')], dummy_2)

#enginetype
str(price_cars2$enginetype)
summary(price_cars2$enginetype)
dummy_3 <- data.frame(model.matrix( ~enginetype, data = price_cars2))
dummy_3 <- dummy_3[,-1]

price_cars3 <- cbind(price_cars2[,!(colnames(price_cars2) %in% 'enginetype')], dummy_3)

#cylindernumber
str(price_cars3$cylindernumber)
summary(price_cars3$cylindernumber)
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = price_cars3))
dummy_4 <- dummy_4[,-1]

price_cars4 <- cbind(price_cars3[,!(colnames(price_cars3) %in% 'cylindernumber')], dummy_4)

#fuelsystem
str(price_cars4$fuelsystem)
summary(price_cars4$fuelsystem)
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = price_cars4))
dummy_5 <- dummy_5[,-1]

price_cars5 <- cbind(price_cars4[,!(colnames(price_cars4) %in% 'fuelsystem')], dummy_5)

#car_company
str(price_cars5$car_company)
summary(price_cars5$car_company)
dummy_6 <- data.frame(model.matrix( ~car_company, data = price_cars5))
dummy_6 <- dummy_6[,-1]

price_cars6 <- cbind(price_cars5[,!(colnames(price_cars5) %in% 'car_company')], dummy_6)

#let's deal with last categorical variable - symboling
# convert to factor
price_cars6$symboling <- as.factor(price_cars6$symboling)
str(price_cars6$symboling)
summary(price_cars6$symboling)
dummy_7 <- data.frame(model.matrix( ~symboling, data = price_cars6))
dummy_7 <- dummy_7[,-1]

price_cars7 <- cbind(price_cars6[,!(colnames(price_cars6) %in% 'symboling')], dummy_7)

# checking structure o price_cars7 - our modeling ready dataframe
str(price_cars7)

##### Creating train-test datasets #####
set.seed(100)
trainindices= sample(1:nrow(price_cars7), 0.7*nrow(price_cars7))
train = price_cars7[trainindices,]
test = price_cars7[-trainindices,]

# Correlation Matrix - to check the correlation
# A few are very obvious as curbweight and enginesize has very high correlation
# compressionratio & fuel type have almost 0.98 ~ 1
# Price is highly correlated with - enginesize, curbweight, carlength, carwidth & horsepower
cormatt = cor(price_cars7)
cormatt

##### Model Building #####
# create 1st model with all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

# Doing variable selection using stepAIC
# using method - both

step <- stepAIC(model_1, direction="both")
step

# Creating second model which resulted from stepAIC
model_2 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + car_companyBMW + car_companyBUICK + car_companyDODGE + 
                car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                symboling0 + symboling3, data = train)
summary(model_2)

# Checking VIF for multicolinearity

vif(model_2)

# Cols with high VIF - enginesize(very low p-value), curbweight(pvalue-0.0035 , quite low),
# carbodysedan(0.003609, still low), carbodyhatchback(0.001657, low), carbodywagon(0.001875, low),
# enginetypeohcf(p value -vaery low), carwidth(pvalue very low) === all of them have p-value <0.01

# We can see that curbweight and engine size are having high vifs so lets check their correlation
cor(train$enginesize, train$curbweight)
# it comes out to be 0.85, there indeed is colinearity so let's drop curbweight as it has lower pvalue

model_3 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + car_companyBMW + car_companyBUICK + car_companyDODGE + 
                car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                symboling0 + symboling3, data = train)
summary(model_3)

vif(model_3)

# variables - carbodyhatchback & carbodysedan have highest VIFs but there is carbodywagon variable with has VIF close to 10 and pvalue 0.013
cor(train$carbodyhatchback,train$carbodysedan)
cor(train$carbodywagon,train$carbodysedan)
cor(train$carbodyhatchback,train$carbodywagon)

# it is moderately correlated to both these variables let's first drop this - carbodywagon

model_4 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + car_companyBMW + car_companyBUICK + car_companyDODGE + 
                car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                symboling0 + symboling3, data = train)
summary(model_4)

vif(model_4)

# vif for carwidth and enginesize is very high let's check their correlation
cor(train$enginesize, train$carwidth)
# these variables are highly correlated 0.763. there can be a colinearity.
#let's drop carwidth as engine size is more significant due to its pvalue

model_5 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + car_companyBMW + car_companyBUICK + car_companyDODGE + 
                car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                symboling0 + symboling3, data = train)

summary(model_5)

vif(model_5)

# enginetypeohc and enginesize have VIFs 5.72 and 8.87 respactively
cor(train$enginesize, train$enginetypeohc)
# enginetypeohc and enginesize are moderately correlated
# enginetypeohc has high pvalue - 

model_6 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + car_companyBMW + car_companyBUICK + car_companyDODGE + 
                car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                symboling0 + symboling3, data = train)

summary(model_6)

vif(model_6)

# fuelsystem2bbl has VIF higher than 3 ad pvalue is very high
cor(train$fuelsystem2bbl , train$enginesize)
# negatively correlated with enginesize (-0.42), let;s drop fuelsystem2bbl

model_7 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberthree + 
                car_companyBMW + car_companyBUICK + car_companyDODGE + 
                car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                symboling0 + symboling3, data = train)

summary(model_7)

vif(model_7)

# drivewheelrwd has VIF higher than 4 ad pvalue is very high
cor(train$drivewheelrwd , train$enginesize)
# drivewheelrwd is correlated with enginesize (-0.601), lets drop drivewheelrwd

model_8 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan +  
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberthree + 
                car_companyBMW + car_companyBUICK + car_companyDODGE + 
                car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                symboling0 + symboling3, data = train)

summary(model_8)

vif(model_8)

# stroke and enginesize have very high VIFs. stroke has a pvalue of 0.002
cor(train$enginesize,train$stroke)
# moderately correlated 0.27, there is colinearity
# lets drop stroke and see where it goes

model_9 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize +  peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan +  
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberthree + 
                car_companyBMW + car_companyBUICK + car_companyDODGE + 
                car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                symboling0 + symboling3, data = train)

summary(model_9)

vif(model_9)
# r-squared and adjusted r-squared are still good so let's keep going

# carbodyhatchback, car_companyTOYOTA and enginesize have high VIFs. but carbodyhatchback has high p-value 0,1, lets drop carbodyhatchback

model_10 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize +  peakrpm + carbodyhardtop + carbodysedan +  
                enginetypedohcv + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberthree + 
                car_companyBMW + car_companyBUICK + car_companyDODGE + 
                car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                symboling0 + symboling3, data = train)

summary(model_10)

vif(model_10)
# r-squared and adjusted r-squared are still good so let's keep going

# while scanning through VIFs and pvalues, symboling3 comes out as obvious candidate as it have pvalue > 0.9 and VIF > 2

model_11 <- lm(formula = price ~ aspiration + enginelocation +  
                 enginesize +  peakrpm + carbodyhardtop + carbodysedan +  
                 enginetypedohcv + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 car_companyBMW + car_companyBUICK + car_companyDODGE + 
                 car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                 car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                 symboling0 , data = train)
summary(model_11)

vif(model_11)
# r-squared and adjusted r-squared are still good so let's keep going

# while scanning through VIFs and pvalues, enginetypel comes out as a candidate to be dropped as it have pvalue > 0.5 and VIF > 2

model_12 <- lm(formula = price ~ aspiration + enginelocation +  
                 enginesize +  peakrpm + carbodyhardtop + carbodysedan +  
                 enginetypedohcv+ enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 car_companyBMW + car_companyBUICK + car_companyDODGE + 
                 car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                 car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                 symboling0 , data = train)
summary(model_12)

vif(model_12)
# r-squared and adjusted r-squared are still good so let's keep going

# when we check correlation of enginelocation enginetypeohcf
cor(train$enginelocation,train$enginetypeohcf)
# they come to be highly correlated and have vif > 2 and are modrately correlated with enginesize
# let's drop enginetypeohcf as other two vars will explain the variance from enginetypeohcf

model_13 <- lm(formula = price ~ aspiration + enginelocation +  
                 enginesize +  peakrpm + carbodyhardtop + carbodysedan +  
                 enginetypedohcv+ enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 car_companyBMW + car_companyBUICK + car_companyDODGE + 
                 car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                 car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                 symboling0 , data = train)
summary(model_13)

vif(model_13)

# r-squared and adjusted r-squared are still good so let's keep going

# Next let's drop car_CompanyBUIK as among the three left with VIF>2, enginesize and car_CompanyBUICK has high correlation
cor(train$enginesize,train$car_companyBUICK)

model_14 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize +  carbodyhardtop + carbodysedan +  
                 enginetypedohcv+ enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                 car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                 symboling0 , data = train)
summary(model_14)

vif(model_14)

# Now all the variables have VIFs close to 2
# Lets start looking at p-values of the variables

summary(model_14)
# carbodysedan has p-value 0.954, let's remove this

model_15 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize +  carbodyhardtop +  
                 enginetypedohcv+ enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                 car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 + 
                 symboling0 , data = train)
summary(model_15)

# symboling0 has p-value 0.9005, let's remove this

model_16 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize +  carbodyhardtop +  
                 enginetypedohcv+ enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                 car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + car_companySAAB + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 , data = train)
summary(model_16)

# car_companySAAB has p-value 0.664200, lets remove it

model_17 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize +  carbodyhardtop +  
                 enginetypedohcv+ enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyJAGUAR + car_companyMAZDA + 
                 car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 , data = train)
summary(model_17)

# car_companyJAGUAR has p-value 0.734, lets remove it

model_18 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize +  carbodyhardtop +  
                 enginetypedohcv+ enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyMAZDA + 
                 car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 , data = train)
summary(model_18)

# cylindernumberthree has high p-value, lets remove it

model_19 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize +  carbodyhardtop +  
                 enginetypedohcv+ enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyMAZDA + 
                 car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 , data = train)
summary(model_19)

#carbodyhardtop has p-value 0.45 , lets remove it
model_20 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetypedohcv+ enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyMAZDA + 
                 car_companyMERCURY + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 , data = train)
summary(model_20)

#car_companyMERCURY has p-value 0.43 , lets remove it
model_21 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetypedohcv+ enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyMAZDA + 
                 car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN + symboling.1 , data = train)
summary(model_21)

# Lets drop symboling.1 as it comes to be least significant with p-value 0.14236
model_22 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetypedohcv+ enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyMAZDA + 
                 car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + 
                 car_companyTOYOTA + car_companyVOLKSWAGEN , data = train)
summary(model_22)

# car_companyVOLKSWAGEN has pvalue 0.042 > 0.01, lets remove it
model_23 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetypedohcv+ enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyMAZDA + 
                 car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + 
                 car_companyTOYOTA , data = train)
summary(model_23)

# enginetypedohcv has pvalue 0.034 > 0.01, lets remove it
model_24 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyMAZDA + 
                 car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + 
                 car_companyTOYOTA , data = train)
summary(model_24)

# car_companyMAZDA has pvalue 0.024 > 0.01, lets remove it
model_25 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyHONDA + car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + 
                 car_companyTOYOTA , data = train)
summary(model_25)

# car_companyHONDA has pvalue 0.022 > 0.01, lets remove it
model_26 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyRENAULT + 
                 car_companyTOYOTA , data = train)
summary(model_26)

# car_companyRENAULT has pvalue 0.0106 > 0.01, lets remove it
model_27 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyMITSUBISHI + car_companyNISSAN + 
                 car_companyPLYMOUTH + car_companyTOYOTA , data = train)
summary(model_27)

# car_companyNISSAN has pvalue 0.0106 > 0.01, lets remove it
model_28 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyMITSUBISHI + car_companyPLYMOUTH + car_companyTOYOTA , data = train)
summary(model_28)

# car_companyTOYOTA has pvalue 0.022 > 0.01, lets remove it
model_29 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyMITSUBISHI + car_companyPLYMOUTH , data = train)
summary(model_29)

# car_companyPLYMOUTH has pvalue 0.022 > 0.01, lets remove it
model_30 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyDODGE + 
                 car_companyMITSUBISHI , data = train)
summary(model_30)

# car_companyDODGE has pvalue 0.0106 > 0.01, lets remove it
model_31 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetyperotor + cylindernumberfive + 
                 car_companyBMW + car_companyMITSUBISHI , data = train)
summary(model_31)

# car_companyMITSUBISHI has highest pvalue among variables left, lets remove it and see the impact on r-squared
model_32 <- lm(formula = price ~ aspiration + enginelocation +  peakrpm +
                 enginesize + enginetyperotor + cylindernumberfive + 
                 car_companyBMW , data = train)
summary(model_32)
#rsquared and adjusted rsquared don't change much

# peakrpm has highest pvalue among variables left, lets remove it and see the impact on r-squared
model_33 <- lm(formula = price ~ aspiration + enginelocation +  
                 enginesize + enginetyperotor + cylindernumberfive + 
                 car_companyBMW , data = train)
summary(model_33)
#rsquared and adjusted rsquared don't change much

# Now we have a stable model

###### let's use it to predict on test dataset ######
# predicting price in test dataset
Predict_1 <- predict(model_33,test[,-1])
test$test_price <- Predict_1

# checking correlation of actual price and predicted price
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
#For model_33 -> R-squared:  0.8964,	Adjusted R-squared:  0.8918  
#r-squared for prediction - 0.8125807
# Model is doing pretty decent
