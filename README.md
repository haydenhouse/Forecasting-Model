# Forecasting-Model
Forecasting Model in R using Air Passenger data.

library(forecast)

# Load Air Passengers dataset
data("AirPassengers")
AirPassengers
summary(AirPassengers)
plot(AirPassengers)
# Through the years Air Passengers have steadily increased

# Decomposition of Data
tsdata<-ts(AirPassengers, frequency = 12)
ddata<-decompose(tsdata, 'multiplicative')
plot(ddata)
# The data looks about how you would expect in the different time series.
# Passengers have steadily increased as seen in observed and trend.
# Passengers fluctuate based on the seasons along with random dips and peaks based on other factors.

# Plotting Passengers with a trend line.
# Must use plot() function first or else abline() will not work. 
plot(AirPassengers)
abline(reg = lm(AirPassengers ~ time(AirPassengers)))

# Boxplot of Air Passengers between 1949-1960.
boxplot(AirPassengers ~ cycle(AirPassengers,xlab = 'Date',ylab = "Passenger Numbers (1000's)",
                              main = "Monthy Air Passengers Boxplot from 1949-1960"))
# More passengers fly in June,July,and August than any other months. This is most likely because of consumers going on vacation. 
# Surprisingly holiday travel was not as popular at this time.

# Auto Arima model
model<-auto.arima(AirPassengers)
model

# Plot Residuals
plot.ts(model$residuals)

# Forecast next 10 years
forecast<-forecast(model,level = c(95),h = 10*12)
plot(forecast)
# Used 10*12 because 12 was number Arima model produced

# Validate model by selecting lag values
Box.test(model$residuals,lag = 5,type = 'Ljung-Box')
Box.test(model$residuals,lag = 10,type = 'Ljung-Box')
Box.test(model$residuals,lag = 15,type = 'Ljung-Box')
# Since we have low p-values our model is pretty accurate. 
# Since our model is accurate we can infer that the (2,1,1) Arima model fits our data the best. 
