options("scipen" = 5, "digits" = 5)
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center, state.division, state.name, state.region)
View(statedata)

str(statedata)

#Problem 1 - Data Exploration
plot(statedata$x, statedata$y)

tapply(statedata$HS.Grad, statedata$state.region, mean)

boxplot(statedata$Murder ~ statedata$state.region)
northeast = subset(statedata, state.region == "Northeast")
northeast$state.name[northeast$Murder == max(northeast$Murder)]


#Problem 2 - Predicting Life Expectancy - An Initial Model
model0 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata) #baseline model
summary(model0) # R-squared = 0.736, Adj R-squared = 0.692

coef(summary(model0))["Income", "Estimate"]
plot(statedata$Income, statedata$Life.Exp)


#Problem 3 - Predicting Life Expectancy - Refining the Model and Analyzing Predictions

model1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata) # Removed Area from model0
summary(model1) # R-squared = 0.736, Adj R-squared = 0.6999

model2 = lm(Life.Exp ~ Population + Illiteracy + Murder + HS.Grad + Frost, data = statedata) # Removed Area and Income from model0
summary(model2) # R-squared = 0.736, Adj R-squared = 0.706

model3 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata) # Removed Area, Income, and Illiteracy from model0
summary(model3) # R-squared = 0.736, Adj R-squared = 0.713 --> Best Model

model4 = lm(Life.Exp ~ Murder + HS.Grad + Frost, data = statedata) # Removed Area, Income, Illiteracy, and Population from model0
summary(model4) # R-squared = 0.713, Adj R-squared = 0.694

model5 = lm(Life.Exp ~ Population + Murder + HS.Grad, data = statedata) # Removed Area, Income, Illiteracy, and Frost from model0
summary(model5) # R-squared = 0.701, Adj R-squared = 0.681

prediction = sort(predict(model3))
prediction # Alabama had the lowest predicted Life Expectancy, and Washington had the highest Life Expectancy using model3.
statedata$state.name[which.min(statedata$Life.Exp)] # But South Carolina had the actual lowest Life Expectancy in the data.
statedata$state.name[which.max(statedata$Life.Exp)] # But Hawaii had the actual highest Life Expectancy in the data.

sort(abs(model3$residuals)) #residuals (absolute error) of model3 per state.

