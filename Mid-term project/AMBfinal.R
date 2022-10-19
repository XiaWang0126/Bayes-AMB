# Overview dataset ----
WAGE <- read.table("wage.txt", header = TRUE)
library(MASS)
lm.fit <- lm(wage ~.-id, data =WAGE)
summary(lm.fit)

# Diagnostic plots
par(mfrow = c(2,2))
plot(lm.fit)

# Data cleaning (Potential Problem) ----
# 1. Collinearity
library(car)
vif(lm.fit)

# 2. Identify and remove Outliers 
boxplot(WAGE$wage, plot=TRUE)$out
outliers <- boxplot(WAGE$wage, plot=FALSE)$out
WAGE<- WAGE[-which(WAGE$wage %in% outliers),]

# 3. Convert negative value to 0
WAGE$experience[WAGE$experience < 0] <- 0

# Simple linear regression ----
# Model 1 
edu.lm <-lm(log(wage) ~ education, data =WAGE)
summary(edu.lm)

# Model 2
exp.lm <-lm(log(wage) ~ experience, data =WAGE)
summary(exp.lm)

# Multiple linear regression ----
# Model 3
mlt.lm1 <-lm(log(wage) ~ education + experience, data =WAGE)
summary(mlt.lm1)
plot(mlt.lm1)
## Adjusted R-squared:  0.1875 
## F-statistic:  3145

#Model 4
mlt.lm2 <-lm(log(wage) ~ education + experience + parttime , data =WAGE)
summary(mlt.lm2)
par(mfrow = c(2,2))
plot(mlt.lm2)
## Adjusted R-squared:  0.3944 
## F-statistic:  5915

# Model 5 
mlt.lm3 <-lm(log(wage) ~ education + experience + parttime + smsa + ethnicity, data =WAGE)
summary(mlt.lm3)
plot(mlt.lm3)
## Adjusted R-squared:  0.4098 
## F-statistic:  3784

# Anova, compare Model 4 and Model 5 ----
anova(mlt.lm2,mlt.lm3)

# StepAIC ----
intercept.lm <- lm(log(wage) ~ 1, data = WAGE)
stepAIC(intercept.lm, ~ education + experience + parttime + smsa  + ethnicity, data = WAGE)
# According to AIC score, we keep Model 4 in this part

# Consider interations ----
# Model 6 
# Interactions
int.lm <-lm(log(wage) ~ education + experience + parttime + education * experience , data =WAGE)
summary(int.lm)
par(mfrow = c(2,2))
plot(int.lm)
## Adjusted R-squared:  0.3944 
## F-statistic:  4436
# Considering the Adjusted R-squared and F-statistic value, we keep Model 4 at the end.

# -----
