### Airbnb Project on Kaggle

### NOTES: DEC.18
# model.rf is built by a sample of 20,000 obs and 20-trees forest. Test acc = 0.867967, Val acc = 0.867404
# model.rffull is built by all data and 10-trees forest. Test acc = 0.849337, Val acc = 0.850162
# 1.Try more trees first
# 2.NDFs are perfectly predicted. Split it off, then try another model focusing on residuals.
# 3.Prediction on US is well-predicted with accuracy of 0.945797

library(caret)
library(lubridate)

# Check stats
check.c <- read.csv(file = 'countries.csv')
check.agegender = read.csv(file = 'age_gender_bkts.csv')

dataAirbnb <- read.csv(file = 'train_users_2.csv')
set.seed(1111)
inSample = sample(nrow(dataAirbnb), 20000)
dataSample = dataAirbnb[inSample,]
# save(dataSample, file = 'dataSample.Rda')
# load(file = 'dataSample.Rda')

# Function dataClean
dataClean <- function(x){
      x$id = NULL
      x$date_account_created = as.Date(as.character(x$date_account_created), format = '%Y-%m-%d')
      x$date_account_created = paste0(year(x$date_account_created), '-', month(x$date_account_created))
      x$date_account_created = as.factor(x$date_account_created)
      x$date_first_booking = as.Date(as.character(x$date_first_booking), format = '%Y-%m-%d')
      x$date_first_booking = paste0(year(x$date_first_booking), '-', month(x$date_first_booking))
      x$date_first_booking = as.factor(x$date_first_booking)
      x[is.na(x$age), 'age'] = -1
      x$age = cut(x$age, breaks = c(-2,0,20,40,60,80,Inf))
      x$language = NULL
      
      x$timestamp_first_active = NULL
      
      return(x)
}

# Build training set, testing set, and validation set.
set.seed(12345)
inBuild = createDataPartition(y = dataSample$country_destination, p = 0.7, list = FALSE)
validation = dataSample[-inBuild,]
buildData = dataSample[inBuild,]
set.seed(22345)
inTrain = createDataPartition(y = buildData$country_destination, p = 0.7, list = FALSE)
training = buildData[inTrain,]
testing = buildData[-inTrain,]
rm(buildData); rm(inBuild); rm(inTrain)

data.train = dataClean(training)
data.test = dataClean(testing)
data.validation = dataClean(validation)

# Build Model
set.seed(1111)
time.now = proc.time()
model.rf = train(country_destination ~., method = "rf", data = data.train, ntree = 30, do.trace = TRUE)
proc.time() - time.now
# save(model.rf, file = 'model.rf.revised.Rds')
 load(file = 'model.rf.Rds')

# Testing accuracy
data.test = data.test[((data.test$date_first_booking != '2010-2') &
                      (data.test$date_first_booking != '2015-6')), ]
pred.rf = predict(model.rf, data.test)
confusionMatrix(data.test$country_destination, pred.rf)$overall[1]
confusionMatrix(data.test$country_destination, pred.rf)
data.test$result = as.character(pred.rf)

# Group residual data other than NDF and US
data.train = dataClean(training)
data.train = data.train[((data.train$country_destination != 'NDF') & 
                        (data.train$country_destination != 'US')), ]
data.train$country_destination = as.factor(as.character(data.train$country_destination))

# Build model.residual
set.seed(1111)
time.now = proc.time()
model.rf.residual = train(country_destination ~., method = "rf", 
                          data = data.train, ntree = 20, do.trace = TRUE)
proc.time() - time.now
# save(model.rf.residual, file = 'model.rf.residual2.Rds')
# load(file = 'model.rf.residual')

# Test residual accuracy
data.test = dataClean(testing)
data.test = data.test[((data.test$country_destination != 'NDF') &
                        (data.test$country_destination != 'US')), ]
data.test$country_destination = as.factor(as.character(data.test$country_destination))
data.test = data.test[((data.test$date_first_booking != '2010-2') &
                        (data.test$date_first_booking != '2015-6')), ]
pred.rf.residual = predict(model.rf.residual, data.test)
confusionMatrix(data.test$country_destination, pred.rf.residual)$overall[1]  #0.381



### Result test
testing$result = pred.rf
testing.residual = testing[(testing$country_destination != 'NDF'), c(1:14)]
pred.redidual = predict(model.rf.residual, 
                        testing.residual[(testing.residual$date_first_booking != '2010-2' &
                                          testing.residual$date_first_booking != '2015-6'),])
testing[(testing$country_destination != 'NDF'), 15] = 'US'

confusionMatrix(testing$country_destination, testing$result)$overall[1]
confusionMatrix(testing$country_destination, testing$result)

### Short cuts
sapply(data.train, function(x){sum(is.na(x))})

