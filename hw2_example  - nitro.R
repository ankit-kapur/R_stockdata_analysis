# =====================================================================
# CSE487/587
# Author: Nithin Kumar Reddy Bandaru
# Email: nbandaru@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
# install.packages("forecast")
# install.packages("fpp")
# data path /gpfs/courses/cse587/spring2015/data/hw2/data

library(forecast)
library(fpp)

# need to read the stocklist, and loop all files
### TO DO

# Read all files in the directory

filepath = "/gpfs/courses/cse587/spring2015/data/hw2/data/"
files = list.files(path=filepath, pattern = "*.csv")

#print(length(files))
#listOfMAE = c()

stockMAE_Arima = c()
stockName_Arima = c()

stockMAE_HoltWinters = c()
stockName_HoltWinters = c()

stockMAE_LinearRegression = c()
stockName_LinearRegression = c()


for(n in 1:length(files))
{
  # if file is not empty
  if(file.info(paste(filepath,files[n], sep=""))[1]>0) 
  {  
    # read one csv file into variable (DO NOT EDIT)
    textData=read.csv(file=paste(filepath,files[n], sep=""), header=T)
    #print(paste("nrows", nrow(textData), sep= " "))
    if(nrow(textData) == 754)
    {
      # convert txt data to time-series data, in day unit (DO NOT EDIT)
      tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
      
      # define train data (DO NOT EDIT)
      trainData = window(tsData, end=c(2014,14))
      
      # define test data (DO NOT EDIT)
      testData = window(tsData, start=c(2014,15))
      
      # MAE row vector (DO NOT EDIT)
      MAE_Arima = matrix(NA,1,length(testData))
      MAE_HoltWinters = matrix(NA,1,length(testData))
      MAE_LinearRegression = matrix(NA,1,length(testData))    
      
      
      # applying ARIMA model (DO NOT EDIT)
      fitData_Arima = auto.arima(trainData,seasonal = FALSE, lambda = NULL, approximation = TRUE)
      # applying HOLTWINTERS model (DO NOT EDIT)
      fitData_HoltWinters = HoltWinters(trainData, gamma = FALSE)
      # applying LINEAR REGRESSION model (DO NOT EDIT)
      fitData_LinearRegression = tslm(trainData~trend+season)
      
      # apply forecast(DO NOT EDIT)
      forecastData_Arima = forecast(fitData_Arima, h=length(testData))
      forecastData_HoltWinters = forecast(fitData_HoltWinters, h=length(testData))      
      forecastData_LinearRegression = forecast(fitData_LinearRegression, h=length(testData))
      
      
      # print variable and see what is in the result data set
      #print(forecastData)
      
      # calculate Mean Absolute Error 
      for(i in 1:length(testData))
      {
        MAE_Arima[1,i] = abs(forecastData_Arima$mean[i] - testData[i])
        MAE_HoltWinters[1,i] = abs(forecastData_HoltWinters$mean[i] - testData[i])
        MAE_LinearRegression[1,i] = abs(forecastData_LinearRegression$mean[i] - testData[i])
      }
      
      # this is the result you need for stock AAPL
      #print(sum(MAE_Arima[1,1:10]))
     # print(sum(MAE_HoltWinters[1,1:10]))
     # print(sum(MAE_LinearRegression[1,1:10]))
      
     # listOfMAE[n] = sum(MAE[1,1:10])
      stockMAE_Arima[n] = sum(MAE_Arima[1,1:10])
      stockName_Arima[n] = files[n] 
      
      stockMAE_HoltWinters[n] = sum(MAE_HoltWinters[1,1:10])
      stockName_HoltWinters[n] = files[n]
      
      stockMAE_LinearRegression[n] = sum(MAE_LinearRegression[1,1:10])
      stockName_LinearRegression[n] = files[n]
      
    }
  }
}

print("--- ARIMA MODEL ----")
listOfMAE_Arima = data.frame(stockMAE_Arima , stockName_Arima)
#sorting based on column stock mean error
sortedMAE_Arima = listOfMAE_Arima[order(listOfMAE_Arima$stockMAE_Arima) , ]
#Display top 10 results after sorting 
head(sortedMAE_Arima,10)

print("--- HOLTWINTER MODEL ----")
listOfMAE_HoltWinters = data.frame(stockMAE_HoltWinters , stockName_HoltWinters)
#sorting based on column stock mean error
sortedMAE_HoltWinters = listOfMAE_HoltWinters[order(listOfMAE_HoltWinters$stockMAE_HoltWinters) , ]
#Display top 10 results after sorting 
head(sortedMAE_HoltWinters,10)

print("--- LINEAR REGRESSION MODEL ----")
listOfMAE_LinearRegression = data.frame(stockMAE_LinearRegression , stockName_LinearRegression)
#sorting based on column stock mean error
sortedMAE_LinearRegression = listOfMAE_LinearRegression[order(listOfMAE_LinearRegression$stockMAE_LinearRegression) , ]
#Display top 10 results after sorting 
head(sortedMAE_LinearRegression,10)

# plot the top 10 minimum sum of MAE in 3 models respectively
jpeg('Arima.jpg')
plot(sortedMAE_Arima[1:10,1], col = "blue", main ="Arima Model", xlab = "Stock Index", ylab = "Mean Absolute Error")
lines(sortedMAE_Arima[1:10,1], lw = 2, col = "red")
dev.off()

jpeg('Holtwinter.jpg')
plot(sortedMAE_HoltWinters[1:10,1], col = "blue", main ="HoltWinters Model", xlab = "Stock Index", ylab = "Mean Absolute Error")
lines(sortedMAE_HoltWinters[1:10,1], lw = 2, col = "red")
dev.off()

jpeg('LinearRegression.jpg')
plot(sortedMAE_LinearRegression[1:10,1], col = "blue", main ="Linear Regression Model", xlab = "Stock Index", ylab = "Mean Absolute Error")
lines(sortedMAE_LinearRegression[1:10,1], lw = 2, col = "red")
dev.off()
### TO DO
