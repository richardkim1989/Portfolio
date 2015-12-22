library("ggplot2")
library("ggthemes")
elantra = read.csv("elantra.csv")
elantraTrain = subset(elantra, Year <= 2012)
elantraTest = subset(elantra, Year >= 2013)

#Data Exploration
str(elantraTrain)
head(elantraTrain)
ggplot(elantraTrain, aes(x = Month, y = ElantraSales)) + geom_point() + geom_smooth(method = "lm")
ggplot(elantraTrain, aes(x = Year, y = ElantraSales)) + geom_point() + geom_smooth(method = "lm")
ggplot(elantraTrain, aes(x = Month, y = ElantraSales)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~Year)
ggplot(elantraTrain, aes(x = Unemployment, y = ElantraSales)) + geom_point() + geom_smooth(method = "lm")
ggplot(elantraTrain, aes(x = Queries, y = ElantraSales)) + geom_point() + geom_smooth(method = "lm")
ggplot(elantraTrain, aes(x = CPI_energy, y = ElantraSales)) + geom_point() + geom_smooth(method = "lm")
ggplot(elantraTrain, aes(x = CPI_all, y = ElantraSales)) + geom_point() + geom_smooth(method = "lm")

#Linear Regression Models
model0 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(model0)
coef(model0, data = elantraTrain)

model1 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Month, data = elantraTrain)
summary(model1)
coef(model1, data = elantraTrain)

110.69 * (3 - 1)
110.69 * (5 - 1)

elantraTrain$Month2 = as.factor(elantraTrain$Month)
elantraTest$Month2 = as.factor(elantraTest$Month)

model2 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Month2, data = elantraTrain)
summary(model2)
coef(model2, data = elantraTrain)

cor(elantraTrain[c("Unemployment", "Month", "Queries", "CPI_energy", "CPI_all")])

#Refining the Model
model3 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Month2, data = elantraTrain)
summary(model3)

#Predictions
prediction = predict(model3, newdata = elantraTest)
summary(prediction)
SSE = sum((prediction - elantraTest$ElantraSales)^2)
baseline = mean(elantraTrain$ElantraSales)
SST = sum((mean(elantraTrain$ElantraSales) - elantraTest$ElantraSales)^2)
1 - (SSE/SST)
max(abs(prediction - elantraTest$ElantraSales))
elantraTest$Month[which.max(abs(prediction - elantraTest$ElantraSales))]
