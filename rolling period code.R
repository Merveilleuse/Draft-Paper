#2017 Honours Project: Draft Code           **
#Author: Marvelous Mubenesha                **  
#Student No. MBNMAR005                      **
#*********************************************
#*********************************************


#--------- i) Import, clean and transform data-----------------------------------
library(dplyr)
library(readxl)
top40index <- read_excel("~/Honours_project/Draft-Paper/top40index.xlsx", 
                         col_types = c("date", "numeric"))
index <- top40index%>%distinct(date, .keep_all = TRUE) #Removing duplicate dates


# Outliers
boxplot(c(index[,2]), names = c("price index"), main = "Boxplot of Price Index")  #Comment : looks like there are some outliers eventhough outliers are 'relatively' clustered
log.returns <- CalculateReturns(as.ts(top40index[,2]), method = "log")[-1] 
boxplot(log.returns, names = c("log returns"), main = "Boxplot of Log Returns on the Price Index")

#Formal tests for Outliers in Time series --------------------------

library(tsoutliers)  # conducts multiple tests for outliers #Reference Chen and Liu;s as authors : https://stats.stackexchange.com/questions/104882/detecting-outliers-in-time-series-ls-ao-tc-using-tsoutliers-package-in-r-how
library(ggpubr)
log.returns.outlier.detection <- tsoutliers::tso(as.ts(log.returns),types = c("AO","IO","LS","TC","SLS"), maxit.iloop = 10)
log.return.outlier.detection  #Several outliers detected
#Further investigation shows that these outliers occur when the return is large in abs value, because this is due to natural market activity and the outliers do not reflect in the boxplot of the price index, the outliers have been kept in the data. See : log.returns[c(299,302,348,1144,1156,1540)]


#-------- iii) ARIMA MODEL IDENTIFICATION : BOX-JENKINS METHODOLOGY -----------------------
head(index)

#Partitioning in-sample and out-of-sample data
library(PerformanceAnalytics)
in.sample.data <- top40index %>% filter(between(date, as.POSIXct("2012-07-31"), as.POSIXct("2016-07-31")))
in.samp.log.returns <- CalculateReturns(as.ts(in.sample.data[,2]), method = "log")[-1]  #reduced data set because it was taking too long to run rolling window forecasts

#   Step 1: Formally testing stationarity/Unit root tests
library(tseries)   #for Augmented Dickey-Fuller test
adf.test(in.samp.log.returns) # p < 0.01 therefore we reject the null. Data is stationary 

#    Step 2: ACF and PACF plots for model identification
par(mfrow = c(1,2))
plot1 <- acf(in.samp.log.returns, main = "Autocorrelation Function for Log Returns")  #ma(2)
plot2 <- pacf(in.samp.log.returns, main = "Partial Autocorrelation Function for Log Returns")  #ar(2), ar(30)
par(mfrow = c(1,1))
#ma and ar(30) could be overfitting and since model is for forecasting, I want it to be parsimonious ->
#looks like there could be seasonality but its below the bandwith 

#    Step 3: Fitting possible ARIMA models on log returns (model 1 - model 9)

m1 <- arima(in.samp.log.returns, order = c(0,0,2))
m2 <- arima(in.samp.log.returns, order = c(2,0,0))
m3 <- arima(in.samp.log.returns, order = c(2,0,2))
m4 <- arima(in.samp.log.returns, order = c(0,0,0))


m1$aic; m2$aic; m3$aic; m4$aic

# Model with the lowest AIC is m5 ARIMA(2,0,2)

#Verfiying ARIMA Modelling using forecast package: automatically chooses lowest AIC model
#library(forecast)
#auto.arima(in.samp.log.returns, approximation = F, trace = F) #This also builds the best ARIMA model and it turns out that you got it right. Its the arima(2,0,2) :)


#-------- iv) Rolling Window Forecasting Errors-------------------------------

#Create dataframe compatible with prophet -----------------------
library(prophet)
library(bizdays)

#Sequaence of trading dates
start_date <- as.Date("2012-07-30")
end_date <- as.Date("2017-05-30")   
dates <- bizseq(start_date, end_date, "actual")
prophet_data <- data.frame(ds = dates,y = log.returns[61:1826])   #do this for returns only if it runs smoothly and you have time

#Convert Prophet returns data to xts format for ARIMA rolling period forecasting
library(xts)
toDate <- function(x) as.Date(x, origin = "2017-07-30")
zoo.object <- read.zoo(prophet_data,header=TRUE,FUN=toDate)
returns <- as.xts(zoo.object)  #returns data in xts format but dates include weekends. Prophet has hover matched days correctly so its not a big deal

#Create out of sample period sequence
out_of_sample <- index(returns)[index(returns) >= "2016-05-01"] #all dates after and incl. 1 May 2016


# t+1 rolling period forecasts ------------------------------------
fore_res_1 <- list()  #Initialize list to store results
h=1

#for each element in the out of sample period, calculate h step ahead forecast errors using a rolling window
for(i in 1:(length(out_of_sample) - h)){
  model_1 <- arima(returns[paste0('/', out_of_sample[i])], c(2,0,2),optim.control = list(maxit=1000))  #estimate ARIMA(2,0,2) with data in xts format
  model_2 <-prophet(df=prophet_data[1:4000+i,])      #dataframe format supported by prophet
  
  #Evaluate h step ahead forecast at i for both models
  fore_model_1 <- forecast(model_1, h = h)$mean %>% as.numeric %>% tail(1) 
  fore_model_2 <- predict(model_2, make_future_dataframe(model_2, periods = h), include_history = FALSE)$yhat%>%tail(1)  #This takes >=10 seconds to run
  
  #Actual obs
  actual <- returns[out_of_sample[i + h]] %>% as.numeric
  
  #Store results for each iteration
  fore_res_1[[i]] <- data.frame(date = out_of_sample[i],   
                                actual = actual,
                                fore_model_1, 
                                fore_model_2,
                                error1 = actual - fore_model_1,
                                error2 = actual - fore_model_2)
  
}
fore_res_1 <- do.call(rbind, fore_res_1)

# t+5 rolling period forecasts -----------------------------------------------
fore_res_5 <- list()  #Initialize list to store results
h=5

#for each element in the out of sample period, calculate h step ahead forecast errors using a rolling window
for(i in 1:(length(out_of_sample) - h)){
  model_1 <- arima(returns[paste0('/', out_of_sample[i])], c(2,0,2),optim.control = list(maxit=1000))  #estimate ARIMA(2,0,2) with data in xts format
  model_2 <-prophet(df=prophet_data[1:4000+i,])      #dataframe format supported by prophet
  
  #Evaluate h step ahead forecast at i for both models
  fore_model_1 <- forecast(model_1, h = h)$mean %>% as.numeric %>% tail(1) 
  fore_model_2 <- predict(model_2, make_future_dataframe(model_2, periods = h), include_history = FALSE)$yhat%>%tail(1)  #This takes >=10 seconds to run
  
  #Actual obs
  actual <- returns[out_of_sample[i + h]] %>% as.numeric
  
  #Store results for each iteration
  fore_res_5[[i]] <- data.frame(date = out_of_sample[i],   
                                actual = actual,
                                fore_model_1, 
                                fore_model_2,
                                error1 = actual - fore_model_1,
                                error2 = actual - fore_model_2)
  
}
fore_res_5 <- do.call(rbind, fore_res_5)






# t+20 rolling period forecasts -------------------------------------------------------

fore_res_20 <- list()  #Initialize list to store results
h=20

#for each element in the out of sample period, calculate h step ahead forecast errors using a rolling window
for(i in 1:(length(out_of_sample) - h)){
  model_1 <- arima(returns[paste0('/', out_of_sample[i])], c(2,0,2),optim.control = list(maxit=1000))  #estimate ARIMA(2,0,2) with data in xts format
  model_2 <-prophet(df=prophet_data[1:4000+i,])      #dataframe format supported by prophet
  
  #Evaluate h step ahead forecast at i for both models
  fore_model_1 <- forecast(model_1, h = h)$mean %>% as.numeric %>% tail(1) 
  fore_model_2 <- predict(model_2, make_future_dataframe(model_2, periods = h), include_history = FALSE)$yhat%>%tail(1)  #This takes >=10 seconds to run
  
  #Actual obs
  actual <- returns[out_of_sample[i + h]] %>% as.numeric
  
  #Store results for each iteration
  fore_res_20[[i]] <- data.frame(date = out_of_sample[i],   
                              actual = actual,
                              fore_model_1, 
                              fore_model_2,
                              error1 = actual - fore_model_1,
                              error2 = actual - fore_model_2)
  
}
fore_res_20 <- do.call(rbind, fore_res_20)
write.csv(fore_res_20,file="20 step ahead results")

#Export results to text files (The rolling period approach took hours to run. Therefore, results were stored as a loss mitigation measure)
write.table(fore_res_1,file="1 step ahead results.txt")
write.table(fore_res_5,file="5 step ahead results.txt")
write.table(fore_res_20,file="20 step ahead results.txt")

# Forecast Error Statistics --------------------------------------------
library(forecast)

#Visual analysis

boxplot(fore_res_1$error1, fore_res_1$error2,
        fore_res_5$error1,fore_res_5$error2,
        fore_res_20$error1,fore_res_20$error2,
        main = "Boxplot of Rolling Period Forecast Errors for Arima and Prophet Models",
        names = c("Arima(h = 1)","Prophet(h = 1)","Arima(h = 5)","Prophet(h = 5)","Arima(h = 20)","Prophet(h = 20)"))

#Boxplot of forecast errors with ggplot2----
library(ggplot2)

x.1 <- rep(1, times = 365)
x.2 <- rep(2, times = 365)
x.3 <- rep(3, times = 361)
x.4 <- rep(4, times = 361)
x.5 <- rep(5, times = 346)
x.6 <- rep(6, times = 346)
x.fact <- c(x.1,x.2,x.3,x.4,x.5,x.6)
errors <- c(fore_res_1$error1, fore_res_1$error2,
           fore_res_5$error1,fore_res_5$error2,
           fore_res_20$error1,fore_res_20$error2)

my.result <- cbind(x.fact,errors)
my.result <- as.data.frame(my.result)
p <- ggplot(my.result, aes(x=factor(x.fact), 
                           y=errors)) + geom_boxplot()
p<- p + scale_color_grey() + theme_classic()
print(p)

#______________________________________________________________
# t+1 error statistics
fore_arima_1 <- fore_res_1$fore_model_1 #Arima forecasts
fore_prophet_1 <- fore_res_1$fore_model_2    #Prophet forecasts
actual_1 <- fore_res_1$actual    #Actual returns

#Error statistics for t+1 forecasts
error_statistics_arima_1 <- accuracy(fore_arima_1,actual_1)
error_statistics_prophet_1 <- accuracy(fore_prophet_1,actual_1)

#_______________________________________________________________________
# t+5 error statistics
fore_arima_5 <- fore_res_5$fore_model_1   #Arima forecasts
fore_prophet_5 <- fore_res_5$fore_model_2  #Prophet forecasts
actual_5 <- fore_res_5$actual  #Actual returns

#Error statistics for t+5 forecasts
error_statistics_arima_5 <- accuracy(fore_arima_5,actual_5)
error_statistics_prophet_5 <- accuracy(fore_prophet_5,actual_5)

#________________________________________________________________________
# t+20 error statistics
fore_arima_20 <- fore_res_20$fore_model_1 #Arima forecasts
fore_prophet_20 <- fore_res_20$fore_model_2   #Prophet forecasts
actual_20 <- fore_res_20$actual   #Actual returns

#Error statistics for t+1 forecasts
error_statistics_arima_20 <- accuracy(fore_arima_20,actual_20)
error_statistics_prophet_20 <- accuracy(fore_prophet_20,actual_20)

#_________________________________________________________________________


#_____________________________________________________________________________________



#-------- v) Diebold-Mariano Tests -------------------------------------------

dm.test(fore_res_1$error1, fore_res_1$error2, h = 1)  #t+1 forecasts
dm.test(fore_res_5$error1, fore_res_5$error2, h = 5)  #t+5 forecasts
dm.test(fore_res_20$error1, fore_res_20$error2, h = 20)  #t+20 forecasts

dm.test(fore_res_1$error1, fore_res_1$error2, h = 1, power = 1)  #t+1 forecasts
dm.test(fore_res_5$error1, fore_res_5$error2, h = 5, power = 1)  #t+5 forecasts
dm.test(fore_res_20$error1, fore_res_20$error2, h = 20, power = 1)  #t+20 forecasts

dm.test(fore_res_1$error1, fore_res_1$error2, h = 1)  #t+1 forecasts
dm.test(fore_res_5$error1, fore_res_5$error2, h = 5)  #t+5 forecasts
dm.test(fore_res_20$error1, fore_res_20$error2, h = 20)  #t+20 forecasts



