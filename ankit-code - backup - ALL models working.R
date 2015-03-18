# =====================================================================
# CSE487/587
# Author: Ankit Kapur
# Email: ankitkap@buffalo.edu
# =====================================================================


#folderpath = "/gpfs/courses/cse587/spring2015/data/hw2/data/"
#folderpath = "/gpfs/user/ankitkap/hw2/verysmall/"
folderpath = "/home/ankitkap/dic/StockVolatility/InputFiles/verysmall/"

# need to install the following two packages in CCR
# install.packages("forecast")
# install.packages("fpp")

library(forecast)
library(fpp)

# Declarations
stockNames = c()

sumOfMAE_arima = c()
sumOfMAE_holtwinters = c()
sumOfMAE_LR = c()

fileset = list.files(path=folderpath, pattern = "*.csv")
cat(sprintf("Folder: %s\n", folderpath))
cat(sprintf("No. of files: %s\n", length(fileset)))

cat(sprintf("Beginning computations..."))
# Iterate through all files in the folder
for(i in 1:length(fileset))
{
  # Get the filename
  filename = fileset[i]
  # Filepath = Folderpath + filename
  filepath = paste(folderpath, filename, sep="")
  
  # If the file is not empty
  if (file.info(filepath)[1]>0) {
    
    # Read this csv file into a DATA-FRAME
    textData=read.csv(file=filepath, header=T)
      
    # Only proceed if there's exactly 754 lines
    if(nrow(textData) == 754)
    {
      # Store this stock's name (same as the filename)
      stockNames[i] = filename
      
      # convert txt data to time-series data, in day unit (DO NOT EDIT)
      tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
      
      # define train data (DO NOT EDIT)
      trainData = window(tsData, end=c(2014,14))
      # define test data (DO NOT EDIT)
      testData = window(tsData, start=c(2014,15))
      
      # MAE row vector (DO NOT EDIT)
      MAE_arima = matrix(NA,1,length(testData))
      MAE_holtwinters = matrix(NA,1,length(testData))
      MAE_LR = matrix(NA,1,length(testData))
        
      # apply ARIMA model (DO NOT EDIT)
      #fitData_arima = auto.arima(trainData)
      fitData_arima = auto.arima(trainData, seasonal = FALSE, lambda = NULL, approximation = TRUE)
      fitData_holtwinters = HoltWinters(trainData, gamma = FALSE)
      fitData_LR = tslm(trainData~trend+season)
      
      # apply forecast (DO NOT EDIT)
      forecastData_arima = forecast(fitData_arima, h=length(testData))
      forecastData_holtwinters = forecast(fitData_holtwinters, h=length(testData))      
      forecastData_LR = forecast(fitData_LR, h=length(testData))
      
      # Calculate MAE (Mean Absolute Error)
      MAE_arima[1, ] = abs(forecastData_arima$mean[ ] - testData[ ])
      MAE_holtwinters[1, ] = abs(forecastData_holtwinters$mean[ ] - testData[ ])
      MAE_LR[1, ] = abs(forecastData_LR$mean[ ] - testData[ ])

      # This is the result you need for this stock
      sumOfMAE_arima[i] = sum(MAE_arima[1,1:10])
      sumOfMAE_holtwinters[i] = sum(MAE_holtwinters[1,1:10])
      sumOfMAE_LR[i] = sum(MAE_LR[1,1:10])
      #cat(sprintf("Sum of MAE for Arima: %s\n", sumOfMAE_arima[i]))
    }
  }
}
cat(sprintf("\n\nCalculations complete."))

# ============ ARIMA
# Make a dataframe using the name and MAE vectors
dataframe_MAE_arima = data.frame(sumOfMAE_arima, stockNames)
# Sort the dataframe according to the MAE sums
dataframe_MAE_arima = dataframe_MAE_arima[order(dataframe_MAE_arima$sumOfMAE_arima) , ]
# Get the top 10 stocks
headVector_arima = head(dataframe_MAE_arima, 10)
# Get names of the top 10 stocks (remove the .csv part)
topStockNames_arima = substr(headVector_arima$stockNames, 0, regexpr(".",headVector_arima$stockNames,fixed=T)-1)
cat(sprintf("\nTop stocks for the ARIMA model: "))
cat(sprintf("%s ", topStockNames_arima))
cat(sprintf("%s ", headVector_arima$sumOfMAE_arima))

# Plot the top 10 minimum MAE - For the ARIMA model
cat(sprintf("\nMaking a plot for the Arima model"))
plot.new()
jpeg('ArimaPlot.jpeg')
plot(dataframe_MAE_arima[1:10,1], xaxt='n', col = "blue", main="Arima model plot", xlab="Stocks", ylab="MAE sums")
axis(1, 1:10, topStockNames_arima)
lines(dataframe_MAE_arima[1:10,1], lw = 4, col = "red")
dev.off()
cat(sprintf("\nPlot created for Arima."))


# ============ HoltWinters
# Make a dataframe using the name and MAE vectors
dataframe_MAE_holtwinters = data.frame(sumOfMAE_holtwinters, stockNames)
# Sort the dataframe according to the MAE sums
dataframe_MAE_holtwinters = dataframe_MAE_holtwinters[order(dataframe_MAE_holtwinters$sumOfMAE_holtwinters) , ]
# Get the top 10 stocks
headVector_holtwinters = head(dataframe_MAE_holtwinters, 10)
# Get names of the top 10 stocks (remove the .csv part)
topStockNames_holtwinters = substr(headVector_holtwinters$stockNames, 0, regexpr(".",headVector_holtwinters$stockNames,fixed=T)-1)
cat(sprintf("\nTop stocks for the HoltWinters model: "))
cat(sprintf("%s ", topStockNames_holtwinters))
cat(sprintf("%s ", headVector_holtwinters$sumOfMAE_holtwinters))

# Plot the top 10 minimum MAE - For HoltWinters
cat(sprintf("\nMaking a plot for the holtwinters model"))
plot.new()
jpeg('HoltWintersPlot.jpeg')
plot(dataframe_MAE_holtwinters[1:10,1], xaxt='n', col = "blue", main="Holt-Winters model plot", xlab="Stocks", ylab="MAE sums")
axis(1, 1:10, topStockNames_holtwinters)
lines(dataframe_MAE_holtwinters[1:10,1], lw = 4, col = "red")
dev.off()
cat(sprintf("\nPlot created for the Holtwinters model."))


# ============ LR
# Make a dataframe using the name and MAE vectors
dataframe_MAE_LR = data.frame(sumOfMAE_LR, stockNames)
# Sort the dataframe according to the MAE sums
dataframe_MAE_LR = dataframe_MAE_LR[order(dataframe_MAE_LR$sumOfMAE_LR) , ]
# Get the top 10 stocks
headVector_LR = head(dataframe_MAE_LR, 10)
# Get names of the top 10 stocks (remove the .csv part)
topStockNames_LR = substr(headVector_LR$stockNames, 0, regexpr(".",headVector_LR$stockNames,fixed=T)-1)
cat(sprintf("\nTop stocks for the LR model: "))
cat(sprintf("%s ", topStockNames_LR))
cat(sprintf("%s ", headVector_LR$sumOfMAE_LR))

# Plot the top 10 minimum MAE - For LR
cat(sprintf("\nMaking a plot for the LR model"))
plot.new()
jpeg('LRPlot.jpeg')
plot(dataframe_MAE_LR[1:10,1], xaxt='n', col = "blue", main="Linear Regression model plot", xlab="Stocks", ylab="MAE sums")
axis(1, 1:10, topStockNames_LR)
lines(dataframe_MAE_LR[1:10,1], lw = 4, col = "red")
dev.off()
cat(sprintf("\nPlot created for the LR model."))

cat(sprintf("\n\nJob complete."))
# }
