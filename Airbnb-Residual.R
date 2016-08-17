library(caret)
library(lubridate)
library(ggplot2)

load('data.residual.rda')
data.residual$country_destination = as.factor(as.character(data.residual$country_destination))
set.seed(1111)
inSample = sample(nrow(data.residual), 26532)
dataSample = data.residual[inSample,]

# Function Clean
dataClean <- function(x){
      x$id = NULL
      x$date_account_created = NULL
      x$timestamp_first_active = NULL
      x$date_first_booking = NULL
#      x$gender = NULL
#      x$age = NULL
      x[is.na(x$age), 'age'] = -1
      x$age = cut(x$age, breaks = c(-2,0,5,10,15,21,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf))
#      x$signup_method = NULL
#      x$signup_flow = NULL
#      x$language = NULL
      x$affiliate_channel = NULL
      x$affiliate_provider = NULL
      x$first_affiliate_tracked = NULL
      x$signup_app = NULL
#      x$first_device_type = NULL
#      x$first_browser = NULL
      
      return(x)
}

# Build training set, testing set and validation
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

# Build model
set.seed(1111)
time.now = proc.time()
model.rf.residual = train(country_destination ~., data = data.train, method = "rf",
                           ntree = 10, do.trace = TRUE)
proc.time() - time.now
# save(model.rf.residual, file = 'model.residual(full).Rda')


# Test accuracy
pred.rf.residual = predict(model.rf.residual, data.test)
confusionMatrix(data.test$country_destination, pred.rf.residual)$overall[1]  # 0.380499
confusionMatrix(data.test$country_destination, pred.rf.residual)
table(data.test$country_destination)
table(data.test$language)

