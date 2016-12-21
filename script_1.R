setwd("~/Desktop/R/weather")

library(rvest)
library(stringr)
library(ggplot2)
library(dplyr)
library(zoo)
library(data.table)

# Метеостанция Москва (ВДНХ), Россия, WMO_ID=27612, выборка с 03.09.2008 по 17.12.2016, все дни
# Кодировка: UTF-8
# Информация предоставлена сайтом "Расписание Погоды", rp5.ru 
# Пожалуйста, при использовании данных, любезно указывайте названный сайт.
# Обозначения метеопараметров см. по адресу http://rp5.ru/archive.php?wmo_id=27612&lang=ru

real <- read.csv("moscow.csv", header = T, sep = ";", comment.char = '#', row.names = NULL)

#names were read a litle bit wrong
names(real) <- names(real[-1])

head(real)

#####
# Store web url
start <- as.Date("2008-09-04")
finish <- as.Date("2016-12-17")
day <- seq(from = start, to = finish, by = "day")
day <- gsub("-", "/", as.character(day))
dat <- sub("^", "http://meteoinfo.ru/archive-forecast/russia/moscow-area/moscow/", day)
head(dat)

forecast <- sapply(dat, function(x) {
                            weather <- html(x)
                            rating <- weather %>% 
                              html_nodes("tr:nth-child(4) .pogodacell") %>%
                              html_text()
                          })
head(forecast)
m <- lapply(forecast, function(x) strsplit(x, "\\s\\s.\\s\\s"))
n <- lapply(m, unlist)
#remove empty lists
n <- n[!sapply(n, is.null)]

official_forecast <- t(as.data.frame(n))
row.names(official_forecast) <- gsub("http...meteoinfo.ru.archive.forecast.russia.moscow.area.moscow.",
                                     "",row.names(official_forecast))
row.names(official_forecast) <- gsub("[[:punct:]]", "-", row.names(official_forecast))


#get the climate average from http://meteoinfo.ru/clim-moscow-daily
climate_average <- read.csv("climate_average.csv", header = F, sep = ";", row.names = NULL)
head(climate_average)




#convert climate average to vector
d <- as.vector(as.matrix(climate_average))
d <- d[!is.na(d)]
avg <- as.numeric(sub(",", ".", d, fixed = TRUE))
head(avg, 10)

off_night <- official_forecast[,c(1,3,5,7,9,11,13)]
off_day <- official_forecast[,c(2,4,6,8,10,12,14)]

train <- real[,c(1,2,3,6,8)]
time_train <- data.frame(matrix(unlist(strsplit(train$Местное.время, " ")), ncol=2, byrow=T))
train <- cbind(train, time_train)
train <- train[,-1]
day_train <- filter(train, X2 == "15:00")
night_train <- filter(train, X2 == "03:00")

row.names(day_train) <- as.Date(day_train$X1, format = "%d.%m.%Y")
day_train[,"date"] <- as.Date(day_train$X1, format = "%d.%m.%Y")

row.names(night_train) <- as.Date(night_train$X1, format = "%d.%m.%Y")
night_train[,"date"] <- as.Date(night_train$X1, format = "%d.%m.%Y")

off_day <- as.data.frame(off_day)
off_day[,"date"] <- as.Date(row.names(off_day))
row.names(off_day) <- as.Date(row.names(off_day))

off_night <- as.data.frame(off_night)
off_night[,"date"] <- as.Date(row.names(off_night))
row.names(off_night) <- as.Date(row.names(off_night))

##########
#fill the gaps
#off_day
day <- as.Date(day, format = "%Y/%m/%d")

dates_missing_off_day <- day[!(day %in% off_day$date)]
missing_data_off_day <- data.frame(date = dates_missing_off_day, V1 = NA, 
                                   V2 = NA, V3 = NA, V4 = NA, V5 = NA, V6 = NA, V7 = NA)
off_day <- rbind(missing_data_off_day, off_day)
off_day <- off_day[order(off_day$date),]

#off_night

dates_missing_off_night <- day[!(day %in% off_night$date)]
missing_data_off_night <- data.frame(date = dates_missing_off_night, V1 = NA, 
                                   V2 = NA, V3 = NA, V4 = NA, V5 = NA, V6 = NA, V7 = NA)
off_night <- rbind(missing_data_off_night, off_night)
off_night <- off_night[order(off_night$date),]

#day_train

dates_missing_day_train <- day[!(day %in% day_train$date)]
missing_data_day_train <- data.frame(date = dates_missing_day_train, `T` = NA, 
                                     Po = NA, U = NA, Ff = NA, X1 = NA, X2 = NA)
day_train <- rbind(missing_data_day_train, day_train)
day_train <- day_train[order(day_train$date),]

#night_train

dates_missing_night_train <- day[!(day %in% night_train$date)]
missing_data_night_train <- data.frame(date = dates_missing_night_train, `T` = NA, 
                                     Po = NA, U = NA, Ff = NA, X1 = NA, X2 = NA)
night_train <- rbind(missing_data_night_train, night_train)
night_train <- night_train[order(night_train$date),]

####
#prepared data - finally

day_train_2 <- as.data.table(day_train[,c(1:5)])

for (i in 1:7) {
  day_train_2[, paste('T_day', i, sep = '_') := shift(day_train_2$`T`, i)]
}

for (i in 1:7) {
  day_train_2[, paste('Po_day', i, sep = '_') := shift(day_train_2$Po, i)]
}

for (i in 1:3) {
  day_train_2[, paste('U_day', i, sep = '_') := shift(day_train_2$U, i)]
}

for (i in 1:3) {
  day_train_2[, paste('Ff_day', i, sep = '_') := shift(day_train_2$Ff, i)]
}

day_train_2 <- day_train_2[-c(1:10),]
day_train_2 <- day_train_2[,-c(3:5)]
head(day_train_2)
#####

night_train_2 <- as.data.table(night_train[,c(1:5)])

for (i in 1:7) {
  night_train_2[, paste('T_day', i, sep = '_') := shift(night_train_2$`T`, i)]
}

for (i in 1:7) {
  night_train_2[, paste('Po_day', i, sep = '_') := shift(night_train_2$Po, i)]
}

for (i in 1:3) {
  night_train_2[, paste('U_day', i, sep = '_') := shift(night_train_2$U, i)]
}

for (i in 1:3) {
  night_train_2[, paste('Ff_day', i, sep = '_') := shift(night_train_2$Ff, i)]
}

night_train_2 <- night_train_2[-c(1:10),]
night_train_2 <- night_train_2[,-c(3:5)]

#####

avg_2 <- c(avg[257:366], avg[-60], avg[-60], avg[-60], avg, avg[-60], avg[-60], avg[-60], avg[1:351])
day_train_2 <- cbind(day_train_2, avg_2)
night_train_2 <- cbind(night_train_2, avg_2)

####
off_day <- as.data.frame(apply(off_day, 2, na.locf))
off_day[,-1] <- apply(off_day[,-1], 2, as.numeric)

off_night <- as.data.frame(apply(off_night, 2, na.locf))
off_night[,-1] <- apply(off_night[,-1], 2, as.numeric)

##########
## predictions for the next day

train_day <- day_train_2[1:2665,]
test_day <- day_train_2[-c(1:2665),]
test_ans_day <- test_day[,1:2]

train_day <- train_day[,-1]
test_day <- test_day[,-c(1:2)]

###

linear_model <- lm(data = train_day, `T`~.)
summary(linear_model)
predictions_lm <- predict(linear_model, test_day)

####

day_train_3 <- cbind(day_train_2, off_day[-c(1:9, nrow(off_day)),])
day_train_3 <- day_train_3[,-24]

train_day_expand <- day_train_3[1:2665,]
test_day_expand <- day_train_3[-c(1:2665),]

train_day_expand <- train_day_expand[,-1]
test_day_expand <- test_day_expand[,-c(1:2)]

linear_model_2 <- lm(data = train_day_expand, `T`~.)
summary(linear_model_2)
predictions_lm_expand <- predict(linear_model_2, test_day_expand)

###

mae <- function(actual, predicted) {
  error <- actual - predicted
  mean(abs(error), na.rm = T)
}

mae_11 <- mae(actual = test_ans_day$T, predicted = predictions_lm)
mae_21 <- mae(actual = test_ans_day$T, predicted = predictions_lm_expand)

off_test <- off_day[-c(1:9, nrow(off_day)), 2]
mae_31 <- mae(actual = test_ans_day$T, predicted = off_test[-c(1:2665)])

results <- cbind(test_ans_day, predictions_lm, predictions_lm_expand, off_test[-c(1:2665)])


######################
### predictions for the next-next day

day_train_4 <- day_train_2
day_train_4[,2] <- c(day_train_2$T[-1], 0)

train_day_2nd_day <- day_train_4[1:2665,]
test_day_2nd_day <- day_train_4[-c(1:2665),]
test_ans_day_2nd_day <- test_day_2nd_day[,1:2]

train_day_2nd_day <- train_day_2nd_day[,-1]
test_day_2nd_day <- test_day_2nd_day[,-c(1:2)]

###

linear_model_2nd_day <- lm(data = train_day_2nd_day, `T`~.)
summary(linear_model_2nd_day)
predictions_lm_2nd_day <- predict(linear_model_2nd_day, test_day_2nd_day)

####

day_train_5 <- cbind(day_train_2, off_day[-c(1:9, nrow(off_day)),])
day_train_5[,2] <- c(day_train_2$T[-1], 0)
day_train_5 <- day_train_5[,-24]

train_day_expand_2nd_day <- day_train_5[1:2665,]
test_day_expand_2nd_day <- day_train_5[-c(1:2665),]

train_day_expand_2nd_day <- train_day_expand_2nd_day[,-1]
test_day_expand_2nd_day <- test_day_expand_2nd_day[,-c(1:2)]

linear_model_2_2nd_day <- lm(data = train_day_expand_2nd_day, `T`~.)
summary(linear_model_2_2nd_day)
predictions_lm_expand_2nd_day <- predict(linear_model_2_2nd_day, test_day_expand_2nd_day)

###

mae_12 <- mae(actual = test_ans_day_2nd_day[-352,]$T, predicted = predictions_lm_2nd_day[-352])
mae_22 <- mae(actual = test_ans_day_2nd_day[-352,]$T, predicted = predictions_lm_expand_2nd_day[-352])

off_test_2nd_day <- off_day[-c(1:8, nrow(off_day),  nrow(off_day) - 1), 3]
mae_32 <- mae(actual = test_ans_day_2nd_day[-352,]$T, predicted = off_test_2nd_day[-c(1:2665, 3017)])

results_2nd_day <- cbind(test_ans_day_2nd_day, predictions_lm_2nd_day, predictions_lm_expand_2nd_day, off_test_2nd_day[-c(1:2665)])



##############
### 3d day forecast

######################
### predictions for the next-next day

day_train_6 <- day_train_2
day_train_6[,2] <- c(day_train_2$T[-c(1:2)], 0, 0)

train_day_3d_day <- day_train_6[1:2665,]
test_day_3d_day <- day_train_6[-c(1:2665),]
test_ans_day_3d_day <- test_day_3d_day[,1:2]

train_day_3d_day <- train_day_3d_day[,-1]
test_day_3d_day <- test_day_3d_day[,-c(1:2)]

###

linear_model_3d_day <- lm(data = train_day_3d_day, `T`~.)
summary(linear_model_3d_day)
predictions_lm_3d_day <- predict(linear_model_3d_day, test_day_3d_day)

####

day_train_7 <- cbind(day_train_2, off_day[-c(1:9, nrow(off_day)),])
day_train_7[,2] <- c(day_train_2$T[-c(1:2)],0, 0)
day_train_7 <- day_train_7[,-24]

train_day_expand_3d_day <- day_train_7[1:2665,]
test_day_expand_3d_day <- day_train_7[-c(1:2665),]

train_day_expand_3d_day <- train_day_expand_3d_day[,-1]
test_day_expand_3d_day <- test_day_expand_3d_day[,-c(1:2)]

linear_model_2_3d_day <- lm(data = train_day_expand_3d_day, `T`~.)
summary(linear_model_2_3d_day)
predictions_lm_expand_3d_day <- predict(linear_model_2_3d_day, test_day_expand_3d_day)

###

mae_13 <- mae(actual = test_ans_day_3d_day[-c(351,352),]$T, predicted = predictions_lm_3d_day[-c(351,352)])
mae_23 <- mae(actual = test_ans_day_3d_day[-c(351,352),]$T, predicted = predictions_lm_expand_3d_day[-c(351,352)])

off_test_3d_day <- off_day[-c(1:7, nrow(off_day),  nrow(off_day) - 1, nrow(off_day) - 2), 4]
mae_33 <- mae(actual = test_ans_day_3d_day[-c(351,352),]$T, predicted = off_test_3d_day[-c(1:2665, 3017, 3016)])

results_3d_day <- cbind(test_ans_day_3d_day, predictions_lm_3d_day, predictions_lm_expand_3d_day, off_test_3d_day[-c(1:2665)])

#####
#final results

mae_total <- round(data.frame(c(mae_11, mae_31, mae_21), c(mae_12, mae_32, mae_22), c(mae_13, mae_33, mae_23),
              row.names = c("linear model", "offical forecast", "lm + official"))
names(mae_total) <- c("1st day", "2nd day", "3d day")


ggplot(results[1:60,], aes(x = date)) +
  geom_line(aes(y = `T`), size = 0.75) + 
  geom_line(aes(y = predictions_lm), colour = "blue") +
  geom_line(aes(y = predictions_lm_expand), colour = "green") +
  geom_line(aes(y = V4), colour = "red") +
  labs(x="Jan-Mar 2016",y="Temperature")
  
