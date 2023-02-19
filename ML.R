library(forecast)
library(xts)
library(ggplot2)
library(urca)
library(lubridate)
library(KFAS)
library(xgboost)
library(randomForest)
library(e1071)


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
agg_dt <- aggregate(dt$y, list(hour=cut(as.POSIXct(dt$X), "hour")), mean)

# Rename the columns for better readability
colnames(agg_dt) <- c("date", "mean")

# Convert into xts
xts_dt <- xts(agg_dt[, 2], order.by = as.POSIXct(agg_dt$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
colnames(xts_dt) <- c("y")


########## XGBOOST FORECASTING ##########

set.seed(2023) 

y <- as.numeric(xts_dt$y)
lag = 168
ele_train <- y[1:8016]
y <- ele_train |> diff(lag)
Y <- embed(y, lag+1)
colnames(Y) <- c('y', paste0('y_lag_', 1:lag))

mod1 <- xgboost(data = Y[, -1], label = Y[, 1], nrounds = 1000)
# mod1 <- svm(y ~ ., data = Y)

y_hatr <- numeric(720)
X <- Y[nrow(Y), -lag-1, drop = FALSE]
colnames(X) <- colnames(Y)[-1]

for (h in 1:720){
  y_hatr[h] <- predict(mod1, X)
  X[1, 2:lag] <- X[1, 1:lag-1]
  X[1, 1] <- y_hatr[h]
}

ele_hatr_xg <- filter(y_hatr, c(rep(0, lag-1), 1), 
                      'recursive',
                      init = ele_train[8016:(8016-lag+1)])

forecast_10 = list()
for(i in 1:719){
  forecast_10 <- append(forecast_10, seq(from = (unname(unlist(ele_hatr_xg)))[i], to = (unname(unlist(ele_hatr_xg)))[i+1], length.out = 7)[-7])
}

forecast_10 <- c(forecast_10, rep(tail(forecast_10, n = 1), times = 6))
start_date <- as.POSIXct("2017-11-01 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2017-11-30 23:50:00", tz = "UTC")
date_sequence <- seq(from = start_date, to = end_date, by = "10 min")

xts_test <- xts(unname(unlist(forecast_10)), order.by = date_sequence)
colnames(xts_test) <- c("mean")

val = dt$y[43777:48096]

plot(date_sequence, val, type = 'l', main = 'ML', ylab = 'Power consumption', xlab = 'Date sequence')
lines(date_sequence, xts_test$mean, col = 'red')

(test - xts_test$mean) |> abs() |> mean()

write.csv(xts_test$mean, "C:/Users/nicco/OneDrive/Desktop/ML.csv")