### Airbnb Project on Kaggle

library(caret)
library(lubridate)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

set.seed(12345)

# load data
load(file = 'dataAirbnb.Rda'); load(file = 'dataTest.Rda')
training = dataAirbnb; testing = dataTest
rm(dataAirbnb); rm(dataTest)
training = training[training$country_destination != 'NDF' & training$country_destination != 'US',]

# get labels
classes = training$country_destination
training$country_destination = NULL
id.test = testing$id

## Processing
# combine training and testing data
data.all = rbind(training, testing)
# remove date_first_booking
data.all$date_first_booking = NULL
# ----> deal with NA??

## Feature engineering
# date_account_created
data.all$date_account_created = as.Date(as.character(data.all$date_account_created), 
                                        format = '%Y-%m-%d')
data.all$dac_year = as.factor(year(data.all$date_account_created))
data.all$dac_month = as.factor(month(data.all$date_account_created))
data.all$dac_day = as.factor(day(data.all$date_account_created))
data.all$dac_weekday = as.factor(weekdays(data.all$date_account_created))
data.all$date_account_created = NULL

# timestamp_first_active
data.all$tfa_year = substring(as.character(data.all$timestamp_first_active), 1, 4)
data.all$tfa_year = as.factor(data.all$tfa_year)
data.all$timestamp_first_active = NULL

# age
data.all[is.na(data.all$age), 'age'] = -3
data.all[(data.all$age < 14 & data.all$age != -3) | data.all$age > 100, 'age'] <- -1
data.all$age = cut(data.all$age, breaks = c(-4,-2,0,21,25,30,35,40,45,50,55,60,65,70,80,Inf))

# others
data.all$language = NULL
data.all$first_browser = NULL
data.all$signup_flow = as.factor(data.all$signup_flow)

## ohe
ohe.feats = c('gender', 'signup_method', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type')
dummies <- dummyVars(~ gender + signup_method + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type, data = data.all)
df.ohe <- as.data.frame(predict(dummies, newdata = data.all))
data.all <- cbind(data.all[,-c(which(colnames(data.all) %in% ohe.feats))], df.ohe)

## split data into training and testing
data.train = data.all[(data.all$id %in% training$id),]
data.test = data.all[data.all$id %in% id.test,]
data.train$country_destination = as.factor(as.character(classes))


## train model.rf
set.seed(123)
time.now = proc.time()
model.rf <- randomForest(country_destination ~., data = data.train[,-1],
                          ntree = 200, do.trace = TRUE)
proc.time() - time.now


## prediction
pred <- predict(model.rf, data.test[,-1], type = 'prob')
pred.top3 <- as.vector(apply(pred, 1, function(x) names(sort(x)[10:8])))

ids <- NULL
data.test$id = as.character(data.test$id)
for (i in 1:nrow(data.test)) {
      idx <- data.test$id[i]
      ids <- append(ids, rep(idx,3))
}
rank345 <- NULL
rank345$id <- ids
rank345$country <- pred.top3
rank345 <- as.data.frame(rank345)

rank12 <- read.csv(file = 'submission16.csv')

# i = 1; j = 1
# for (i in (1:nrow(rank123))) {
#       if (i %% 5 == 4) {
#             rank123[i,2] = rank45[j,2]
#             j = j + 1
#       }
#       if (i %% 5 == 0) {
#             rank123[i,2] = rank45[j,2]
#             j = j + 1
#       }
# }
i = 3; j = 1
while (i < nrow(rank12)) {
      rank12[i,2] = rank345[j,2]
      i = i + 1; j = j + 1
      rank12[i,2] = rank345[j,2]
      i = i + 1; j = j + 1
      rank12[i,2] = rank345[j,2]
      i = i + 3; j = j + 1
}


write.csv(rank12, "submission19.csv", quote=FALSE, row.names = FALSE)
