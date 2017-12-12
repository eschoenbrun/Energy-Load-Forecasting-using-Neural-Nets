library(highcharter)
require(zoo)
library(ggfortify)
suppressPackageStartupMessages(library(forecast))
library(data.table)
library(broom)
library(sweep)

# Arima - hour - with sensor
df  <- sw_sweep(auto_arima, fitted = FALSE, timetk_idx = FALSE) 
df$forecast <- df$value
df$value[1298:1345] <- load_data$System_Load[4297:4344] * .71
df$forecast[1:1297] <- NA

# Arima - hour - without sensor
df  <- sw_sweep(auto_arima, fitted = FALSE, timetk_idx = FALSE) 
df$forecast <- df$value
df$value[1298:1345] <- load_data$System_Load[4297:4344]
df$forecast[1:1297] <- NA

# Arima - minute - with sensor
df  <- sw_sweep(auto_arima, fitted = FALSE, timetk_idx = FALSE) 
df$forecast <- df$value
df$value[77762:80641] <- data_ts[257761:260640] * .71
df$forecast[1:77761] <- NA
# this was to aggregate the minute data, but not sure if needed
# df_agg <- df %>%
#   group_by(indx = gl(ceiling(80641/60), 60, 80641)) %>%
#   summarise_each(funs(sum))
# df_agg <- df_agg[1:1344, ]

# Neural Net - with sensor
df  <- sw_sweep(nnet_rh, fitted = FALSE, timetk_idx = FALSE) 
df$forecast <- df$value
df$value[1298:1345] <- load_data$System_Load[4297:4344] * .71
df$forecast[1:1297] <- NA

# Plot each of the above using this
highchart(type = "chart") %>% 
  hc_add_series(df, "arearange", hcaes(index, low = `lo.95`, high = `hi.95`), color = '#c42525', name = "95 Percentile") %>% 
  hc_add_series(df, "arearange", hcaes(index, low = `lo.80`, high = `hi.80`), color = "#FCFFA4", name = "80 Percentile") %>% 
  hc_add_series(df, "line", hcaes(index, value), name = "Original") %>% 
  hc_add_series(df, "line", hcaes(index, `forecast`), color = '#0d233a', name = "Forecast") 

