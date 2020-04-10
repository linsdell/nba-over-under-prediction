require(e1071) #Contains the SVM 

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

#Making non numerical values numerical
for (i in names(data)) {
  if (!is.numeric(data[,i])){
    data[ ,i] <- as.numeric(as.factor(data[,i]))
  } 
}

#Representing over and under as 0 and 1
data$overunder = factor(data$overunder, levels = c(0, 1))

# Training data is start of season (Game 17) to Game 749 inclusive
train <- data[c(1:639),]
# testing data is Game 750 to 816 inclusive
test <- data[c(640:800),]

# Scale data
train[-249] = scale(train[-249])
test[-249] = scale(test[-249])

# Build classifier
classifier = svm(formula = overunder ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'polynomial',
                 degree = 4)


y_pred = predict(classifier, newdata = test[-249])
y_train_pred = predict(classifier, newdata = train[-249])

cm = table(test[, 249], y_pred)
cm2 = table(train[, 249], y_train_pred )

#Accuracy
print((cm[1]+cm[4])/(cm[1]+cm[2]+cm[3]+cm[4]))