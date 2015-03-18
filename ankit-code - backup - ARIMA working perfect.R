# =====================================================================
# CSE487/587
# Author: Ankit Kapur
# Email: ankitkap@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
# install.packages("forecast")
# install.packages("fpp")
# data path /gpfs/courses/cse587/spring2015/data/hw2/data

library(forecast)
library(fpp)

func <- function() {

# Declarations
stockNames_arima = c()
sumOfMAE_arima = c()

folderpath = "/home/ankitkap/dic/StockVolatility/InputFiles/verysmall/"
cat(sprintf("Folder: %s\n", folderpath))

fileset = list.files(path=folderpath, pattern = "*.csv")
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
      stockNames_arima[i] = filename
      
      # convert txt data to time-series data, in day unit (DO NOT EDIT)
      tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
      
      # define train data (DO NOT EDIT)
      trainData = window(tsData, end=c(2014,14))
      # define test data (DO NOT EDIT)
      testData = window(tsData, start=c(2014,15))
      
      # MAE row vector (DO NOT EDIT)
      MAE_arima = matrix(NA,1,length(testData))
        
      # apply ARIMA model (DO NOT EDIT)
      fitData_arima = auto.arima(trainData)
      # apply forecast (DO NOT EDIT)
      forecastData_arima = forecast(fitData_arima, h=length(testData))
      
      # Calculate MAE (Mean Absolute Error)
      MAE_arima[1, ] = abs(forecastData_arima$mean[ ] - testData[ ])

      # This is the result you need for this stock
      sumOfMAE_arima[i] = sum(MAE_arima[1,1:10])
      #cat(sprintf("Sum of MAE for Arima: %s\n", sumOfMAE_arima[i]))
    }
  }
}
cat(sprintf("\n\nCalculations complete."))


# Make a dataframe using the name and MAE vectors
dataframe_MAE_arima = data.frame(sumOfMAE_arima, stockNames_arima)
# Sort the dataframe according to the MAE sums
dataframe_MAE_arima = dataframe_MAE_arima[order(dataframe_MAE_arima$sumOfMAE_arima) , ]
# Get the top 10 stocks
headVector_arima = head(dataframe_MAE_arima, 10)
# Get names of the top 10 stocks (remove the .csv part)
topStockNames_arima = substr(headVector_arima$stockNames_arima, 0, regexpr(".",headVector_arima$stockNames_arima,fixed=T)-1)
cat(sprintf("\nTop stocks for the ARIMA model: "))
cat(sprintf("%s ", topStockNames_arima))

# Plot the top 10 minimum MAE
cat(sprintf("\nMaking a plot for the Arima model"))
plot.new()
jpeg('ArimaPlot.jpeg')
plot(dataframe_MAE_arima[1:10,1], xaxt='n', col = "blue", main="Arima model plot", xlab="Stocks", ylab="MAE sums")
axis(1, 1:10, topStockNames_arima)
lines(dataframe_MAE_arima[1:10,1], lw = 4, col = "red")
dev.off()
cat(sprintf("\nPlot created for Arima."))

cat(sprintf("\n\nJob complete."))
}
