library(dplyr)
library(TimeProjection)
library(forecast)

## Data was downloaded from here: 
## https://www.iso-ne.com/isoexpress/web/reports/load-and-demand/-/tree/zone-info

# setting up time series
raw_data <- read.csv("C:\\Users\\Stevens\\Desktop\\energy_load_forcasting.csv", sep = ",")
load_data <- raw_data[,(c("Date", "Hr_End", "System_Load"))]
load_data$Date <- as.Date(load_data$Date, origin = "1900-01-01")
load_data$Date <- seq(from = as.POSIXct("2017-01-03 00:00"), 
                      to = as.POSIXct("2017-07-02 24:00"), by = "hour")
load_data <- load_data[, c('Date', 'System_Load')]

dat <- c(ISOdate(2017, 1, 3), ISOdate(2017, 1, 1))
startTime <- difftime(dat[1], dat[2], units="hours")
train_f <- ts(load_data$System_Load[3000:4296], start=c(2017, as.numeric(startTime)), frequency=24*365)

# TBATS forecasts
orig_data %>%
  tbats %>%
  forecast(h=12) %>%
  plot


plot.new()
plot(aggregate(orig_data,FUN=mean))
plot(forecast(orig_data, h=72), include=240)
abline(reg=lm(orig_data~time(orig_data)))

# simple exponential - models level
fit <- HoltWinters(orig_data, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit2 <- HoltWinters(orig_data, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(orig_data)
# Automated forecasting using an exponential model
fit4 <- ets(orig_data)
# Automated forecasting using an ARIMA model
fit5 <- auto.arima(orig_data)

# predictive accuracy
accuracy(fit)

# predict next three future values
forecast(fit, 12)
plot(forecast(fit, 12))

train_f <- ts(black_df[,-1], start=2001, end = 2005.75, frequency=4)

mean_f <- meanf(train_f, h=4)
naive_f <- naive(train_f, h=4)
snaive_f <- snaive(train_f, h=4)
random_walk <- rwf(train_f, h=4)
simple_smooth <- ses(train_f, h=4)
holt_F <- holt(train_f, h=4)
hot_wint <- hw(train_f, h = 1 * frequency(train_f))
spline_f <- splinef(train_f, h=4)
theta_f <- thetaf(train_f, h=4)
linear <- forecast(tslm(train_f ~ trend + season), h=4)
auto_arima <-  forecast(auto.arima(train_f), h=4)
exp_smooth <- forecast(ets(train_f), h=4)

accuracy(mean_f)
accuracy(naive_f)
accuracy(snaive_f)
accuracy(random_walk)
accuracy(simple_smooth)
accuracy(holt_F)
accuracy(hot_wint)
accuracy(spline_f)
accuracy(theta_f)
accuracy(linear)
accuracy(auto_arima)
accuracy(exp_smooth)
