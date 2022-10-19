#Reading data
WINE <- read.table("wine.txt", header = TRUE)

# Data cleaning ----
# Droping the NA prices of the dataset
library(tidyr)
WINE <- WINE%>% drop_na(price)

# Fill the NA values with random forest
#install.packages("missRanger")
library(missRanger)
library(dplyr)
head(wine <- missRanger(WINE, num.trees = 100))

# Analysis of the dataset ----
# Test the correlation matrix among the predictors
cor(wine, method = c("pearson")) 
## There is correlation of Parker with other variables, which we decide to remove
## Parker for the analysis of the model

# Data processing ----
#Remove the high correlated variable: Parker
wine <- wine[-7]

# Check the existence of outliers
boxplot(wine$price, plot=TRUE)$out


# Plots ----
par(mfrow=c(2,3)) 
scat_year <- plot(wine$year, wine$price, main="Price vs Year",  
                  
                  xlab="Year", ylab="Price", pch=19) 
scat_h.rain <- plot(wine$h.rain, wine$price, main="Price vs Rain in the harvest month (mm)",  
                    
                    xlab="Milimeters", ylab="Price", pch=19) 
scat_s.temp <- plot(wine$s.temp, wine$price, main="Price vs Average temperature (C)",  
                    
                    xlab="Temperature (C)", ylab="Price", pch=19) 
scat_w.rain <- plot(wine$w.rain, wine$price, main="Price vs Rain in the winter preceding harvest (mm)",  
                    
                    xlab="Milimeters", ylab="Price", pch=19) 
scat_h.temp <- plot(wine$h.temp, wine$price, main="Price vs Average temperature at harvest (C)",  
                    
                    xlab="Temperature (C)", ylab="Price", pch=19) 
scat_rating <- plot(wine$parker, wine$price, main="Price vs Rating",  
                    
                    xlab="Rating", ylab="Price", pch=19) 

# Models ----
## Linear model prediction ----
price_lm <- lm(price ~., data=wine) 
summary(price_lm) 
par(mfrow=c(2,2))
plot(price_lm)
## In this context it is not reasonable to use the linear model, since the price
## can only be positive. Therefore, it would be better to use GLM for prediction
## The residual plot presents a clear trend, which means that this model is not good.

# Gaussian GLM ----
price_gauss <- glm(price ~ ., data = wine, family = gaussian(link="log"))
summary(price_gauss)
plot(price_gauss)
DE_gauss = (12755.7-1953.3)/12755.7
## The AIC is 271.55
## Almost all the variables are significant, except of h.rain
## The residual plots present trend
## The DE is 0.846

# Poisson GLM ----
price_poisson <- glm(price ~ ., data = wine, family = poisson(link="log"))
summary(price_poisson)
plot(price_poisson)
DE_poisson= (385.730-78.344)/385.730
## The AIC increased compared with the Gaussian one: 277.27
## The deviance explained of the model is 0.796
## There is still some trend in the residuals of the model

# Gamma GLM ----
price_gamma <- glm(price ~ ., data = wine, family = Gamma(link="log"))
summary(price_gamma)
plot(price_gamma)
DE_gamma= (13.5749-3.5104)/13.5749
## There are still small trend in the residual plot, but in general better than previously
## The deviance explained is 0.7414
## The AIC is 265.41
## h.temp seems to be not significant in this case. 

# Gamma GLM (without h.temp) ----
price_gamma1 <- glm(price ~ . -h.temp, data = wine, family = Gamma(link="log"))
summary(price_gamma1)
par(mfrow=c(2,2))
plot(price_gamma1)
DE_gamma1= (13.5749-3.7626)/13.5749
## The AIC is slightly bigger than before: 266.09
## The w.rain seems not to be significant neither
## The DE is 0.72
## There is trend in the residual plot, but much better than previously.
library(car)
vif(price_gamma1)
pchisq(3.7626, 33, lower.tail=F)

