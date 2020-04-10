require(e1071)
library(caret)

#Read data
data <- read.csv('nba_data_v3.csv', header = TRUE) # already feauture engineered

#Replacing NA values in numerical columns with the median of that column
for(i in 1:ncol(data)){
  if (is.numeric(data[,i]) && sum(is.na(data[,i])) != 0){
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
  }
  #Replacing NA values is non-numerical columns with 'None'
  else if (!is.numeric(data[,i]) && sum(is.na(data[,i])) != 0) {
    levels <- levels(data[ , i])
    if (!("None" %in% levels(train[ ,i]))) {
      levels[length(levels) + 1] <- "None"
      data[ , i] <- factor(data[ , i], levels = levels)
    }
    data[is.na(train[,i]), i] <- "None"
  }
}

for (i in names(data)) {
  if (!is.numeric(data[,i])){
    #Making non numerical values numerical
    data[ ,i] <- as.numeric(as.factor(data[,i]))
  } 
}

data$overunder = factor(data$overunder, levels = c(0, 1))

# Training data is start of season (Game 17) to Game 749 inclusive
train <- data[c(1:639),]
# testing data is Game 750 to 816 inclusive
test <- data[c(640:800),]


# To find the best combination of parameters, there are two methods: Grid Search and Random Search

# Random Search - Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "random")

set.seed(1234)

# Run the model
rf_default <- train(overunder ~ .,
                    data = train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)


set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(150: 160))
rf_mtry <- train(overunder~.,
                 data = train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)

best_mtry <- rf_mtry$bestTune$mtry 
best_mtry


# Searching best max nodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(20: 30)) {
  set.seed(1234)
  rf_maxnode <- train(overunder~.,
                      data = train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)


#The best max node number is 23

# Searching for the best ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(overunder~.,
                       data = train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 10,
                       maxnodes = 23,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

# The best ntrees number is 600

# Training random forest model with parameters
fit_rf <- train(overunder~.,
                train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 15,
                ntree = 600,
                maxnodes = 23)

# Evaluating model

pred <-predict(fit_rf, test)
confusionMatrix(pred, test$overunder)
