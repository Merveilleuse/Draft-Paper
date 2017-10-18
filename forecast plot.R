#plotting forecasts

library(ggplot2)
library(scales)


results.data.1 <- read.table("1 step ahead results.txt", sep = " ")
head(results.data.1)

arima_gg1 <- ggplot(data=results.data.1, aes(x = date, y = actual, group = 1)) # y = (Pt+1 /Pt)-1
arima_gg1 <- arima_gg1+geom_line(col = 'red')
arima_gg1 <- arima_gg1+geom_line(aes(y=fore_model_1),col='blue')
arima_gg1 <- arima_gg1+geom_line(aes(y=fore_model_2),col='green')
arima_gg1



#Sample from this link  : https://www.r-bloggers.com/plotting-forecast-objects-in-ggplot-part-2-visualize-observations-fits-and-forecasts/ 

p1a<-ggplot(data=pd,aes(x=date,y=observed)) 
p1a<-p1a+geom_line(col='red')
p1a<-p1a+geom_line(aes(y=fitted),col='blue')
#This ads the grey confidence interval
p1a<-p1a+geom_line(aes(y=forecast))+geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=.25)
p1a<-p1a+scale_x_date(name='',breaks='1 year',minor_breaks='1 month',labels=date_format("%b-%y"),expand=c(0,0))
p1a<-p1a+scale_y_continuous(name='Units of Y')
p1a<-p1a+opts(axis.text.x=theme_text(size=10),title='Arima Fit to Simulated Data\n (black=forecast, blue=fitted, red=data, shadow=95% conf. interval)')
#p1a
arima_gg1

