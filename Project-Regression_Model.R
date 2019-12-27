### 1. Open the Longley data set from the datasets package
data <- longley
View(data)


### 2. Plot Employed against each variable and select the 3 variables most correlated to Employed
plot(data$GNP.deflator, data$Employed)
plot(data$GNP, data$Employed)
plot(data$Unemployed, data$Employed)
plot(data$Armed.Forces, data$Employed)
plot(data$Population, data$Employed)
plot(data$Year, data$Employed)

## So #GNP, #Populatuion, #Year these 3 variables are most correlated to Employed.
library(tidyverse)
data.0 <- data %>%
  select(Employed, GNP.deflator, GNP, Population)  
view(data.0)


### 3. For each variable selected in number 2, create a regression model for Employed. Select the best model.
GNP.deflator.model <- lm(Employed ~ GNP.deflator, data = data.0)
summary(GNP.deflator.model)
# GNP.deflator.model Regression equation: Employed = 33.19 + 0.32*GNP.deflator
GNP.model <- lm(Employed ~ GNP, data = data.0)
summary(GNP.model)
# GNP.model Regression equation: Employed = 51.84 + 0.03*GNP
Population.model <- lm(Employed ~ Population, data = data.0)
summary(Population.model)
# Population.model Regression equation: Employed = 8.38 + 0.48*Population
## So coefficient of population model is greater than other models, hence Population.model is the best model.


### 4. For the champion model from number 3, create the model matrices.
y <- as.matrix(data.0$Employed)
view(y)

x <- as.matrix(cbind(rep(1,16), data.0[,4]))
colnames(x) <- c("x0", "x1")
view(x)


### 5. Recalculate the regression parameters and predicted values from the model matrices.
# Estimate model
B <- solve(t(x) %*% x) %*% t(x) %*% y
view(B)
B
# Linear regression equation: y = 8.38 + 0.48*x1
#                                    OR
#                           : Employed = 8.38 + 0.48*Population

## Calculate predicted values
pred <- data.0 %>%
  mutate(PredictedEmployed = 8.38 + 0.48*Population)  
view(pred)
