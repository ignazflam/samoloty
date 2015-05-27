extractFeatures <- function(data) {
  features <- c("season",
                "holiday",
                "workingday",
                "weather",
                "temp",
                "atemp",
                "humidity",
                "windspeed",
                "hour",
                "day",
                "year")
  data$hour <- hour(ymd_hms(data$datetime))
  data$day <- wday(ymd_hms(data$datetime), label=TRUE)
  data$year <- year(ymd_hms(data$datetime))
  return(data[,features])
}
extractFeatures <- function(data) {
  features <- c("season",
                "holiday",
                "workingday",
                "weather",
                "temp",
                "atemp",
                "humidity",
                "windspeed",
                "hour",
                "day",
                "year")
  data$season <- as.factor(data$season)
  data$weather <- as.factor(data$weather)
  data$hour <- hour(ymd_hms(data$datetime))
  data$day <- wday(ymd_hms(data$datetime), label=TRUE)
  data$year <- year(ymd_hms(data$datetime))
  return(data[,features])
}

extractFeatures <- function(data) {
  features <- c("season",
                "holiday",
                "workingday",
                "weather",
                "atemp",
                "humidity",
                "windspeed",
                "holiday",
                "hour"
                )
#  data$season <- as.factor(data$season)
#  data$weather <- as.factor(data$weather)
  data$hour <- hour(ymd_hms(data$datetime))
  return(data[,features])
}
train2 <- train
train2$day <- wday(ymd_hms(train2$datetime), label=TRUE)
train2$time <- as.POSIXct(strftime(ymd_hms(train2$datetime), format="%H:%M:%S"), format="%H:%M:%S")
ggplot(train2, aes(x=time, y=count, color=day)) +
  geom_smooth(fill = NA, size = 2) +
  xlab("Time") +
  ylab("Bikes Rented") +
  scale_x_datetime(labels=date_format("%I:%M %p"))

ggplot(rf1fi, aes(x=reorder(Feature, Importance), y=Importance)) +
    geom_bar(stat="identity", fill="#53cfff") + coord_flip()

extractFeatures <- function(data) {
  features <- c("season",
                "workingday",
                "weather",
                "atemp",
                "humidity",
                "windspeed",
                "hour",
                "day",
                "year")
#  data$season <- as.factor(data$season)
#  data$weather <- as.factor(data$weather)
  data$hour <- hour(ymd_hms(data$datetime))
  data$day <- wday(ymd_hms(data$datetime), label=TRUE)
  data$year <- year(ymd_hms(data$datetime))
  return(data[,features])
}
trainFea <- extractFeatures(trainset)
testFea  <- extractFeatures(testset)

rfmodel2 <- randomForest(trainFea,trainset$count, ntrees = 100)

submission <- data.frame(datetime=testset$datetime, count=NA)
submission[, "count"] <- predict(rfmodel2, extractFeatures(testset))

for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs   <- year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    testSubset <- test[testLocs]
    submission[, "count"] <- predict(rfmodel2, extractFeatures(testSubset))
  }
}

write.csv(submission, file = "2_random_forest_submission.csv", row.names=FALSE)
