library(forecast)
library(xts)
library(ggplot2)
library(urca)
library(lubridate)
library(dplyr)


########## PREPROCESSING ##########

rm(list=ls())
dt <- read.csv('C:/Users/nicco/OneDrive/Desktop/Data Science - MAGISTRALE/Anno 2/Semestre 1/SDMTSA/Progetto/data2022_train.csv') 

# Check NA values
any(is.na(dt))

# Transforming into datetime object
dt$X <- as.POSIXct(dt$X, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extract the hour information from the datetime column
dt$hour <- format(dt$X, "%H")

# Aggregate the data by hour and calculate the mean
agg_dt <- aggregate(dt$y, by = list(date = as.Date(dt$X), hour = dt$hour), mean)

# Rename the columns for better readability
colnames(agg_dt) <- c("date", "hour", "mean")

# Creating a column for each hour
dt_list <- split(agg_dt, agg_dt$hour)
merged_dt <- dt_list[[1]][, c("date", "mean")]
for (i in 2:length(dt_list)) {
  merged_dt <- merge(merged_dt, dt_list[[i]][, c("date", "mean")], by = "date")
}
colnames(merged_dt) <- c("date", paste0("h", 0:(length(dt_list)- 1)))

# Converting into xts objects
xts_list <- list()
for (i in 2:ncol(merged_dt)) {
  xts_list[[i-1]] <- xts(merged_dt[, i], order.by = as.Date(merged_dt[, 1]))
}

# Now I have 24 xts objects, each representing the daily aggregated data for each hour.

# We choose h17 to evaluate stationariety
y <- xts_list[[17]]
weekdays(time(y))[1:10]
# The series starts with sunday

# Dividing into training and validation
y0 <- y[1:304]
xts_list0 <- list()
for (i in 1:24){
  xts_list0[[i]] <- xts_list[[i]][1:304]
}

# Plotting time series graph
plot(as.numeric(y0), type = 'l')

# Variance stationariety
len <- length(y0)
q <- rep(1:floor(len/7)+1, each=7)[1:len]
means <- tapply(y0, q, mean)
sds <- tapply(y0, q, sd)
plot(means, sds, main = 'Variance stationariety', xlab = 'Means', ylab = 'Standard deviations')
abline(lm(sds ~ means, data = mtcars), col = "red")
# The scatterplot seems quite sparse

# BoxCox
BoxCox.lambda(y0, "loglik")

# Dickey-Fuller
summary(ur.df(y0, 'trend', lags = 1, 'AIC')) 

# KPSS
summary(ur.kpss(y0))

# Mean stationariety
plot(y0)
diff(y0, 7) |> plot()

Acf(y0, 50)
Pacf(y0, 50)

Acf(diff(y0, 7), 50)
Pacf(diff(y0, 7), 50)

diff(y0, 7) |> diff(1) |> plot()

Acf(diff(y0, 7) |> diff(1), 50)
Pacf(diff(y0, 7) |> diff(1), 50)


########## MODELING ##########

# Seasonal dummies
holidays <- rep(0, 304)

# Anniversary of the Independence Manifesto
holidays[which(index(y0) == as.Date("2017-01-11"))] <- 1
# Labour Day
holidays[which(index(y0) == as.Date("2017-05-01"))] <- 1
# Eid al-Fitr
holidays[which(index(y0) == as.Date("2017-06-26"))] <- 1
# Feast of the Throne
holidays[which(index(y0) == as.Date("2017-07-30"))] <- 1
# Anniversary of the Recovery Oued Ed-Dahab
holidays[which(index(y0) == as.Date("2017-08-14"))] <- 1
# Anniversary of the Revolution of the King and the People
holidays[which(index(y0) == as.Date("2017-08-20"))] <- 1
# Youth Day
holidays[which(index(y0) == as.Date("2017-08-21"))] <- 1
# Eid al-Adha holidays
holidays[which(index(y0) == as.Date("2017-08-30"))] <- 1
holidays[which(index(y0) == as.Date("2017-09-01"))] <- 1 
holidays[which(index(y0) == as.Date("2017-09-02"))] <- 1  
holidays[which(index(y0) == as.Date("2017-09-03"))] <- 1 
holidays[which(index(y0) == as.Date("2017-09-04"))] <- 1 
# Hijra New Year holidays
holidays[which(index(y0) == as.Date("2017-09-21"))] <- 1
holidays[which(index(y0) == as.Date("2017-09-22"))] <- 1 
holidays[which(index(y0) == as.Date("2017-09-23"))] <- 1 
holidays[which(index(y0) == as.Date("2017-09-24"))] <- 1 

# Residual analysis
holidays[which(index(y0) == as.Date("2017-09-26"))] <- 1

# Modeling time shift: significative only for validation-set!
time_change <- rep(0, 304)
time_change[1:84] <- 0
time_change[85:301] <- 1
time_change[302:304] <- 0

mod <- Arima(y0, c(0,1,1), list(order = c(0,1,1), period = 7), xreg = cbind(time_change, holidays))
mod
Acf(mod$residuals, 20) 
Pacf(mod$residuals, 20)
Acf(mod$residuals, 365) 
Pacf(mod$residuals, 365) 
plot(mod$residuals) 
time(y0)[mod$residuals < -5000] 
time(y0)[mod$residuals > 5000] 
auto.arima(y0)

# H21-H3: (0,1,1)
# H4-H20: (0,1,1)(0,1,1)


########## FORECASTING ##########

# We apply the ARIMA models to each xts object (i.e. each hour)

# H21-H3: (0,1,1)
# H4-H20: (0,1,1)(0,1,1)

holidays_val <- rep(0, 30)
holidays_val[6] <- 1
holidays_val[18] <- 1

time_change_val <- rep(0, 30)

forecast_val <- function(x, i){
  if (between(i, 4, 20)) { 
    mod_val <- Arima(x, c(0,1,1), list(order = c(0,1,1), period = 7), xreg = cbind(holidays, time_change))
  }
  if ((between(i, 1, 3)) | (between(i, 21, 24))){
    mod_val <- Arima(x, c(0,1,1), xreg = cbind(holidays, time_change))
  }
  forecast(mod_val, h = 30, xreg = cbind(holidays_val, time_change_val))
}

lst_forecasts0 = vector("list", length = 24)
for (i in 1:24){
  lst_forecasts0[[i]] <- forecast_val(xts_list0[[i]], i)
}

# Convert to 10-minute intervals
values0 = list()
for (j in 1:30){
  for (i in 1:24){
    values0 <- append(values0, lst_forecasts0[[i]]$mean[j][1])
  }
}
forecasts_10_val = list()
for(i in 1:719){
  forecasts_10_val <- append(forecasts_10_val, seq(from = (unname(unlist(values0)))[i], to = (unname(unlist(values0)))[i+1], length.out = 7)[-7])
}
forecasts_10_val <- c(forecasts_10_val, rep(tail(forecasts_10_val, n = 1), times = 6))
start_date <- as.POSIXct("2017-11-01 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2017-11-30 23:50:00", tz = "UTC")
date_sequence <- seq(from = start_date, to = end_date, by = "10 min")

xts_val <- xts(unname(unlist(forecasts_10_val)), order.by = date_sequence)
colnames(xts_val) <- c("mean")

# Predictions
val = dt$y[43777:48096]

plot(date_sequence, val, type = 'l', main = 'ARIMA', ylab = 'Power consumption', xlab = 'Date sequence')
lines(date_sequence, xts_val$mean, col = 'red')

(val - xts_val$mean) |> abs() |> mean()


########## FINAL ESTIMATION ##########

# H21-H3: (0,1,1)
# H4-H20: (2,1,1)(0,1,1)

y <- xts_list[[17]]

holidays <- rep(0, length(y))
# Anniversary of the Independence Manifesto
holidays[which(index(y) == as.Date("2017-01-11"))] <- 1
# Labour Day
holidays[which(index(y) == as.Date("2017-05-01"))] <- 1
# Eid al-Fitr
holidays[which(index(y) == as.Date("2017-06-26"))] <- 1
# Feast of the Throne
holidays[which(index(y) == as.Date("2017-07-30"))] <- 1
# Anniversary of the Recovery Oued Ed-Dahab
holidays[which(index(y) == as.Date("2017-08-14"))] <- 1
# Anniversary of the Revolution of the King and the People
holidays[which(index(y) == as.Date("2017-08-20"))] <- 1
# Youth Day
holidays[which(index(y) == as.Date("2017-08-21"))] <- 1
# Eid al-Adha holidays
holidays[which(index(y) == as.Date("2017-08-30"))] <- 1
holidays[which(index(y) == as.Date("2017-09-01"))] <- 1 
holidays[which(index(y) == as.Date("2017-09-02"))] <- 1  
holidays[which(index(y) == as.Date("2017-09-03"))] <- 1 
holidays[which(index(y) == as.Date("2017-09-04"))] <- 1 
# Hijra New Year holidays
holidays[which(index(y) == as.Date("2017-09-21"))] <- 1
holidays[which(index(y) == as.Date("2017-09-22"))] <- 1 
holidays[which(index(y) == as.Date("2017-09-23"))] <- 1 
holidays[which(index(y) == as.Date("2017-09-24"))] <- 1 
# Anniversary of the Green March
holidays[which(index(y) == as.Date("2017-11-06"))] <- 1
# Independence Day
holidays[which(index(y) == as.Date("2017-11-18"))] <- 1
# Residual analysis
holidays[which(index(y) == as.Date("2017-09-26"))] <- 1

holidays_test <- rep(0, 30)
# The Prophet Muhammad's Birthday holidays 
holidays_test[1] <- 1
holidays_test[2] <- 1
holidays_test[3] <- 1
# New Year's holiday
holidays_test[30] <- 1

time_change <- rep(0, 334)
time_change[1:84] <- 0
time_change[85:301] <- 1
time_change[302:334] <- 0

# From ACF and PACF for different hours seems like we may add an AR(1).
mod <- Arima(y, c(2,1,1), list(order = c(0,1,1), period = 7), xreg = holidays)
mod
Acf(mod$residuals, 20) 
Pacf(mod$residuals, 20)
Acf(mod$residuals, 365) 
Pacf(mod$residuals, 365) 
plot(mod$residuals) 
time(y)[mod$residuals < -5000] 
time(y)[mod$residuals > 5000] 
auto.arima(y)

forecast_test <- function(x, i){
  if (between(i, 4, 20)) { 
    mod_test <- Arima(x, c(2,1,1), list(order = c(1,1,1), period = 7), xreg = holidays)
  }
  if ((between(i, 1, 3)) | (between(i, 21, 24))){
    mod_test <- Arima(x, c(0,1,1), xreg = holidays)
  }
  forecast(mod_test, h = 30, xreg = holidays_test)
}

lst_forecasts = vector("list", length = 24)
for (i in 1:24){
  lst_forecasts[[i]] <- forecast_test(xts_list[[i]], i)
}

# Convert to 10-minute intervals
values = list()
for (j in 1:30){
  for (i in 1:24){
    values <- append(values, lst_forecasts[[i]]$mean[j][1])
  }
}
forecasts_10 = list()
for(i in 1:719){
  forecasts_10 <- append(forecasts_10, seq(from = (unname(unlist(values)))[i], to = (unname(unlist(values)))[i+1], length.out = 7)[-7])
}
forecasts_10 <- c(forecasts_10, rep(tail(forecasts_10, n = 1), times = 6))
start_date <- as.POSIXct("2017-12-01 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2017-12-30 23:50:00", tz = "UTC")
date_sequence <- seq(from = start_date, to = end_date, by = "10 min")

xts_test <- xts(unname(unlist(forecasts_10)), order.by = date_sequence)
colnames(xts_test) <- c("mean")

write.csv(xts_test$mean, "C:/Users/nicco/OneDrive/Desktop/ARIMA.csv")