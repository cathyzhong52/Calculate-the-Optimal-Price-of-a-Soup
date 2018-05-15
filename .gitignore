#-------------------------------------------Part 1-----------------------------------------------------------
#a)Load the dataset and the forecasting package
setwd("~/Desktop/Winter/MKT Analytics/HW/HW3")
library(readr)
df <- read_csv("~/Desktop/Winter/MKT Analytics/HW/HW3/Homework 3 Student Data.csv")
library(forecast)

#b)
#aggregate units by week
df_agg = aggregate(formula = totalCost~overallWeekNum,
                   data = df,
                   FUN = sum)

#find the first week of the last 52 weeks.
lastweek = max(df$overallWeekNum) - 52
lastweek
#Set aside the last 52 weeks of the dataset as a holdout sample
TrainingData = df_agg[which(df_agg$overallWeekNum< lastweek+1),]
ValidationData = df_agg[which(df_agg$overallWeekNum> lastweek),]
nrow(TrainingData)
nrow(ValidationData)
#Convert the total sales by week into a time series object
TimeSeriesObject = ts(data = TrainingData$totalCost, frequency = 52)
TimeSeriesObject

#c)
#Plot the time series
plot(TimeSeriesObject) 
#plot the autocorrelations
acf(TrainingData$totalCost)

#d)
#estimate an ARMA model
model = Arima(TimeSeriesObject,order = c(3,0,2), include.drift = TRUE)

#e)
#estimate a non-seasonal ARMA model
model1 = auto.arima(TimeSeriesObject,approximation=FALSE, D=0, seasonal=FALSE, stepwise=FALSE) 
model2 = auto.arima(TimeSeriesObject,approximation=FALSE, D=0, seasonal=TRUE, stepwise=FALSE) 
summary(model1)
summary(model2)
#f)
#Calculate the reported AIC of each model
AIC(model1)
AIC(model2)

#Calculate MSE of each model
MSE1 = mean((ValidationData$totalCost - forecast(model2, h=52)$mean)^2)
MSE2 = mean((ValidationData$totalCost - forecast(model3, h=52)$mean)^2)

#Plotting the forecast for one year for each model
plot(forecast(model1, h=52))
plot(forecast(model2, h=52))


#---------------------------------------------Part 2------------------------------------------------
#a)Calculate the Price Per Can
PricePerCan = df$totalCost/df$units

#b)Run a regression 
Model4 = lm(log(df$units)~PricePerCan)

#c)Aggregate units and totalCost to the productNum+overallWeekNum Level
df_agg2 = aggregate(formula = cbind(units,totalCost)~productNum+overallWeekNum,
                   data = df,
                   FUN = mean)
PricePerCan2 = df_agg2$totalCost/df_agg2$units
Model5 = lm(log(df_agg2$units)~PricePerCan2)

#d)Aggregate units and totalCost to the overallWeekNum Level
df_agg3 = aggregate(formula = cbind(units,totalCost)~overallWeekNum,
                    data = df,
                    FUN = mean)
PricePerCan3 = df_agg3$totalCost/df_agg3$units
Model6 = lm(log(df_agg3$units)~PricePerCan3)

summary(Model4)
summary(Model5)
summary(Model6)

confint(Model4)
confint(Model5)
confint(Model6)
#------------------------------------Part 3---------------------------------------------------------
#a)Calculate the Price Per Can
PricePerCan = df$totalCost/df$units

#b)Run a regression
model7 = lm(log(df$units)~PricePerCan)

#c)Control for isFeature
model8 = lm(log(df$units)~PricePerCan+df$isFeature)


#d)Control for isFeature and isDisplay
model9 = lm(log(df$units)~PricePerCan+df$isFeature+df$isDisplay)

#e)Control for isFeature, isDisplay and the storeNum
model10 = lm(log(df$units)~PricePerCan+df$isFeature+df$isDisplay+factor(df$storeNum))

#f)Control for isFeature, isDisplay, the storeNum and productNum
model11 = lm(log(df$units)~PricePerCan+df$isFeature+df$isDisplay+factor(df$storeNum)+factor(df$productNum))

#g)Control for isFeature, isDisplay, the storeNum, productNum and weekInYearNum
model12 = lm(log(df$units)~PricePerCan+df$isFeature+df$isDisplay+factor(df$storeNum)+factor(df$productNum)+factor(df$weekInYearNum))

summary(model7)
summary(model8)
summary(model9)
summary(model10)
summary(model11)
summary(model12)

#-------------------------------------------Part 4-----------------------------------------------------------
#Load the dataset
setwd("~/Desktop/Winter/MKT Analytics/HW/HW3")
library(readr)
df <- read_csv("~/Desktop/Winter/MKT Analytics/HW/HW3/Homework 3 Student Data.csv")

#a)Figure out the most popular productNum
Pop_Prod = aggregate(formula = units~productNum,
                   data = df,
                   FUN = sum)

which.max(Pop_Prod$units)
Pop_Prod$productNum[89]

#b)subset upc
upcFile = df[which(df$productNum == Pop_Prod$productNum[89]), ]

#c)Build Model1
aggUPCFile = aggregate(cbind(totalCost,units)~weekInYearNum+overallWeekNum+storeNum+isFeature+isDisplay,data=upcFile,FUN = sum)
aggUPCFile$pricePerCan = aggUPCFile$totalCost/aggUPCFile$units
model1 = lm(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+factor(storeNum),data=aggUPCFile)

#d)Create a new dataframe
possiblePrices = data.frame(price = seq(0,10,.01))
possiblePrices$demand = NA
newData = data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices$price)

#e)Predict demand for each of the possible price
possiblePrices$demand = predict(model1, newData)

#f) calculate expected profit
possiblePrices$profit = (possiblePrices$price-0.3)*possiblePrices$demand

#g) find the optimal price
which.max(possiblePrices$profit)
possiblePrices$price[which.max(possiblePrices$profit)]
possiblePrices$profit[which.max(possiblePrices$profit)]

#h) Repeat with model2
model2 = lm(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile)

possiblePrices2 = data.frame(price = seq(0,10,.01))
possiblePrices2$demand = NA
newData2 = data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices$price)

possiblePrices2$demand = predict(model2, newData2)

possiblePrices2$profit = (possiblePrices2$price-0.3)*possiblePrices2$demand

which.max(possiblePrices2$profit)
possiblePrices2$price[which.max(possiblePrices2$profit)]
possiblePrices2$profit[which.max(possiblePrices2$profit)]

summary(model1)
summary(model2)
#--------------------------------------------Part 5----------------------------------------------------
#a)Build predict models of nnet1 and nnet2
library('nnet')
set.seed(1)
nnet1 = nnet(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+factor(storeNum),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)
nnet2 = nnet(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)

#b)Predict demand when price is 50 cents
possiblePrices3 = data.frame(price = seq(0,10,.01))
possiblePrices3$demand = NA
possiblePrices3$demand= exp(predict(nnet1, newData))
a = possiblePrices3$demand[possiblePrices3$price == 0.5]

possiblePrices4 = data.frame(price = seq(0,10,.01))
possiblePrices4$demand = NA
possiblePrices4$demand= exp(predict(nnet2, newData))
b = possiblePrices4$demand[possiblePrices3$price == 0.5]

#Predict demand when price is 1 dollar
c = possiblePrices3$demand[possiblePrices3$price == 1]
d = possiblePrices4$demand[possiblePrices3$price == 1]

#How much the demand change
c-a
d-b

#c) calculate optimal price
possiblePrices3$profit = (possiblePrices3$price-0.3)*possiblePrices3$demand
which.max(possiblePrices3$profit)
possiblePrices3$price[which.max(possiblePrices3$profit)]
possiblePrices3$profit[which.max(possiblePrices3$profit)]

possiblePrices4$profit = (possiblePrices4$price-0.3)*possiblePrices4$demand
which.max(possiblePrices4$profit)
possiblePrices4$price[which.max(possiblePrices4$profit)]
possiblePrices4$profit[which.max(possiblePrices4$profit)]

#d)Plot predicted profit versus price
plot(possiblePrices3$price,possiblePrices3$profit)
plot(possiblePrices4$price,possiblePrices4$profit)

#Fix nnet1
max(aggUPCFile$pricePerCan)
min(aggUPCFile$pricePerCan)


possiblePrices5 = data.frame(price = seq(min(aggUPCFile$pricePerCan),max(aggUPCFile$pricePerCan),.01))
possiblePrices5$demand = NA
newData3 = data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices5$price)
possiblePrices5$demand= exp(predict(nnet1, newData3))

possiblePrices5$profit = (possiblePrices5$price-0.3)*possiblePrices5$demand
which.max(possiblePrices5$profit)
possiblePrices5$price[which.max(possiblePrices5$profit)]
possiblePrices5$profit[which.max(possiblePrices5$profit)]

plot(possiblePrices5$price,possiblePrices5$profit)
