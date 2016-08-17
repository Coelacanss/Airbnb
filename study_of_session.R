library(caret)
library(lubridate)
library(xgboost)
library(readr)
library(stringr)
library(car)

load(file = 'sessions.Rds')
session$user_id = as.character(session$user_id)
users = unique(session$user_id)

load(file = 'data.supply.Rda')

count = 45001

time.now = proc.time()
for (username in users[45001:135484]) {
      
      print(count)
      count = count + 1
      
      user1 = NULL
      user1 <- session[session$user_id == username,]
      newFeatures = NULL
      
      action_detail = NULL
      action_detail = as.data.frame(table(user1$action_detail))
      rownames(action_detail) = action_detail[,1]
      rownames(action_detail)[1] = 'actionNA'
      rownames(action_detail)[2] = 'actionUnknown'
      action_detail[,1] = NULL
      newFeatures$id = user1[1,1]
      newFeatures = as.data.frame(newFeatures)
      newFeatures = cbind(newFeatures, t(action_detail))
      
      secs = NULL
      secs_min = min(user1$secs_elapsed, na.rm = T)
      secs_max = max(user1$secs_elapsed, na.rm = T)
      secs_mean = mean(user1$secs_elapsed, na.rm = T)
      secs = cbind(secs_min, secs_max, secs_mean)
      newFeatures = cbind(newFeatures, secs)
      
      num_actions = nrow(user1)
      newFeatures = cbind(newFeatures, num_actions)
      
      data.supply2 = rbind(data.supply2, newFeatures)
}
proc.time() - time.now

