library(dplyr)
library(TimeProjection)
library(forecast)


## Data was downloaded from here: 
## https://www.iso-ne.com/isoexpress/web/reports/load-and-demand/-/tree/zone-info
raw_data <- read.csv("C:\\Users\\Stevens\\Desktop\\energy_load_forcasting.csv", sep = ",")

load_data <- raw_data[,(c("Date", "Hr_End", "System_Load"))]
load_data$Date <- as.Date(load_data$Date, origin = "1900-01-01")

rand_vect_cont <- function(N, M, sd = 1) {
  vec <- rnorm(N, M/N, sd)
  vec / sum(vec) * M
}

## Build time - related features
dates <- unique(load_data$Date)
dates_data <- cbind(dates, projectDate(as.Date(dates), size = "narrow",
                                  holidays = holidayNYSE(year = unique(year(dates))),
                                  as.numeric = T, drop = T))

## Generate minute-by-minute data
for (i in 1:length(dates)){
  daily_data <- load_data %>% filter(Date == dates[i])
  daily_load <- as.list(daily_data$System_Load)
    for (j in 1:length(daily_data$Hr_End)){
    minute_data <- data.frame(rand_vect_cont(60, mean(daily_data$System_Load[j]), sd(daily_data$System_Load)/60))
    minute_data$date <- dates[i]
    minute_data$hour <- daily_data$Hr_End[j]
    minute_data$minute <- 1:60
    if (!exists('minute_load')){
      minute_load <- minute_data
    } else {
      minute_load <- rbind(minute_load, minute_data)
    }
  }
}
minute_load[minute_load$hour == 24 & minute_load$minute == 60, ]$hour <- 0
minute_load[minute_load$hour == 0 & minute_load$minute == 60, ]$minute <- 0
minute_load <- rename(minute_load, 
               load = rand_vect_cont.60..mean.daily_data.System_Load.j....sd.daily_data.System_Load..60.)

data_set <- left_join(minute_load, dates_data, c("date" = "dates"))

