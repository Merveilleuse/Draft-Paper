#plotting forecasts

library(ggplot2)
library(scales)


results.data.1 <- read.table("1 step ahead results.txt", sep = " ")
head(results.data.1)

arima_gg1 <- ggplot(data=results.data.1, aes(x = as.Date(date), y = actual, group = 1)) 
arima_gg1 <- arima_gg1+geom_line(col = 'brown')
arima_gg1 <- arima_gg1+geom_line(aes(y=fore_model_1),col='blue')
arima_gg1 <- arima_gg1+geom_line(aes(y=fore_model_2),col='green')
arima_gg1
