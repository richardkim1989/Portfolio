wineTrain = read.csv("wine.csv")
wineTest = read.csv("wine_test.csv")

str(wineTrain)
summary(wineTrain)

#Linear Models
model1 = lm(Price ~ AGST, data = wineTrain)
summary(model1)
SSE1 = sum(model1$residuals^2)

model2 = lm(Price ~ AGST + HarvestRain, data = wineTrain)
summary(model2)
SSE2 = sum(model2$residuals^2)

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wineTrain)
summary(model3)
SSE3 = sum(model3$residuals^2)

model3a = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wineTrain) #Best Model
summary(model3a)
SSE3a = sum(model3a$residuals^2)

model3b = lm(Price ~ AGST + HarvestRain + WinterRain, data = wineTrain)
summary(model3b)
SSE3b = sum(model3b$residuals^2)

model4 = lm(Price ~ HarvestRain + WinterRain, data = wineTrain)
summary(model4)
SSE4 = sum(model4$residuals^2)

#Correlations
cor(wineTrain)

plot(wineTrain$Price ~ wineTrain$WinterRain)
cor(wineTrain$WinterRain, wineTrain$Price)

plot(wineTrain$HarvestRain ~ wineTrain$WinterRain)
cor(wineTrain$WinterRain, wineTrain$HarvestRain)

plot(wineTrain$FrancePop ~ wineTrain$Age)
cor(wineTrain$Age, wineTrain$FrancePop)


#Predictions
str(wineTest)
predictTest = predict(model3a, newdata = wineTest)
predictTest

SSE_Test = sum((wineTest$Price - predictTest)^2) #Sum of Squared Errors = (Actual Y - Predicted Y)^2
SST_Test = sum((wineTest$Price - mean(wineTrain$Price))^2) #Total Sum of Squares (Actual Y - Mean of Y or Baseline)^2
RSquared_Test = 1 - (SSE_Test/SST_Test) #R^2 = Sum of Squared Errors / Total Sum of Squares
