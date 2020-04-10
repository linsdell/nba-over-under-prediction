library(caret)
library(MASS)
library(e1071)
data = read.csv("NBA_data.csv") #read the data

#### Missing values in data ####
# Check for missing values
var.missing <- sapply(data,function(x)sum(is.na(x)))
var.missing <- var.missing[order(var.missing)]
var.missing

data$H.win.pct.close.games <- NULL
data$H.opponent.win.pct.close.games <- NULL
data$V.win.pct.close.games <- NULL
data$V.opponent.win.pct.close.games <- NULL

# Delete the first 16 games since they have no stats for any teams
data <- data[-c(1:16),]

#### Train/test split ####
# training data is start of season (Game 17) to Game 749 inclusive
train <- data[c(1:639),]
# testing data is Game 750 to 816 inclusive
test <- data[c(640:800),]

current_accuracy <- get_accuracy_of_given_data(train,test)
current_accuracy

# Look at the data
skewness(train$Total.Score)
# skewness of 0.2224 so total score is fairly symmetrical
hist(train$Total.Score)
# total score also looks symmetrical
median(train$Total.Score)
mean(train$Total.Score)

skewness(train$Line)
# skewness of 0.195557 so line is fairly symmetrical
hist(train$Line)
# line also looks symmetrical
median(train$Line)
mean(train$Line)

# mean and median for line is less than the total score
# indicates that on average taking the over bet would be the optimal choice 


#### Feature Engineering ####

# Offensive to defensive efficiency
train$H.efficiency.ratio <- train$H.offensive.efficiency/train$H.defensive.efficiency
train$V.efficiency.ratio <- train$V.offensive.efficiency/train$V.defensive.efficiency
test$H.efficiency.ratio <- test$H.offensive.efficiency/test$H.defensive.efficiency
test$V.efficiency.ratio <- test$V.offensive.efficiency/test$V.defensive.efficiency

data$H.efficiency.ratio <- data$H.offensive.efficiency/data$H.defensive.efficiency
data$V.efficiency.ratio <- data$V.offensive.efficiency/data$V.defensive.efficiency

# points per possession
train$H.points.per.possession <- train$H.points.per.game/train$H.possessions.per.game
train$V.points.per.possession <- train$V.points.per.game/train$V.possessions.per.game
test$H.points.per.possession <- test$H.points.per.game/test$H.possessions.per.game
test$V.points.per.possession <- test$V.points.per.game/test$V.possessions.per.game

data$H.points.per.possession <- data$H.points.per.game/data$H.possessions.per.game
data$V.points.per.possession <- data$V.points.per.game/data$V.possessions.per.game

# points per scoring posession
train$H.points.per.scoring.possession <- train$H.points.per.possession/train$H.floor.percentage
train$V.points.per.scoring.possession <- train$V.points.per.possession/train$V.floor.percentage
test$H.points.per.scoring.possession <- test$H.points.per.possession/test$H.floor.percentage
test$V.points.per.scoring.possession <- test$V.points.per.possession/test$V.floor.percentage

data$H.points.per.scoring.possession <- data$H.points.per.possession/data$H.floor.percentage
data$V.points.per.scoring.possession <- data$V.points.per.possession/data$V.floor.percentage

# scoring posessions
train$H.scoring.possessions <- train$H.possessions.per.game*train$H.floor.percentage
train$V.scoring.possessions <- train$V.possessions.per.game*train$V.floor.percentage
test$H.scoring.possessions <- test$H.possessions.per.game*test$H.floor.percentage
test$V.scoring.possessions <- test$V.possessions.per.game*test$V.floor.percentage

data$H.scoring.possessions <- data$H.possessions.per.game*data$H.floor.percentage
data$V.scoring.possessions <- data$V.possessions.per.game*data$V.floor.percentage

# foul capitalization
train$H.foul.capitalization <- train$H.opponent.personal.fouls.per.game*train$H.free.throw.pct
train$V.foul.capitalization <- train$V.opponent.personal.fouls.per.game*train$V.free.throw.pct
test$H.foul.capitalization <- test$H.opponent.personal.fouls.per.game*test$H.free.throw.pct
test$V.foul.capitalization <- test$V.opponent.personal.fouls.per.game*test$V.free.throw.pct

data$H.foul.capitalization <- data$H.opponent.personal.fouls.per.game*data$H.free.throw.pct
data$V.foul.capitalization <- data$V.opponent.personal.fouls.per.game*data$V.free.throw.pct

# point opportunity on foul
train$H.point.opp.on.foul <- train$H.free.throws.attempted.per.game/train$H.opponent.personal.fouls.per.game
train$V.point.opp.on.foul <- train$V.free.throws.attempted.per.game/train$V.opponent.personal.fouls.per.game
test$H.point.opp.on.foul <- test$H.free.throws.attempted.per.game/test$H.opponent.personal.fouls.per.game
test$V.point.opp.on.foul <- test$V.free.throws.attempted.per.game/test$V.opponent.personal.fouls.per.game

data$H.point.opp.on.foul <- data$H.free.throws.attempted.per.game/data$H.opponent.personal.fouls.per.game
data$V.point.opp.on.foul <- data$V.free.throws.attempted.per.game/data$V.opponent.personal.fouls.per.game

# opponent aggressiveness
train$H.opponent.aggressiveness <- train$V.personal.fouls.per.game/train$H.opponent.personal.fouls.per.game
train$V.opponent.aggressiveness <- train$H.personal.fouls.per.game/train$V.opponent.personal.fouls.per.game
test$H.opponent.aggressiveness <- test$V.personal.fouls.per.game/test$H.opponent.personal.fouls.per.game
test$V.opponent.aggressiveness <- test$H.personal.fouls.per.game/test$V.opponent.personal.fouls.per.game

data$H.opponent.aggressiveness <- data$V.personal.fouls.per.game/data$H.opponent.personal.fouls.per.game
data$V.opponent.aggressiveness <- data$H.personal.fouls.per.game/data$V.opponent.personal.fouls.per.game

# matchup free throw contribution 
train$H.matchup.free.throw.contribution <- train$H.percent.of.points.from.free.throws*train$H.opponent.aggressiveness
train$V.matchup.free.throw.contribution <- train$V.percent.of.points.from.free.throws*train$V.opponent.aggressiveness
test$H.matchup.free.throw.contribution <- test$H.percent.of.points.from.free.throws*test$H.opponent.aggressiveness
test$V.matchup.free.throw.contribution <- test$V.percent.of.points.from.free.throws*test$V.opponent.aggressiveness

data$H.matchup.free.throw.contribution <- data$H.percent.of.points.from.free.throws*data$H.opponent.aggressiveness
data$V.matchup.free.throw.contribution <- data$V.percent.of.points.from.free.throws*data$V.opponent.aggressiveness

# rebound potential
train$H.rebound.potential <- train$H.opponent.field.goals.attempted.per.game*train$H.total.rebounding.percentage
train$V.rebound.potential <- train$V.opponent.field.goals.attempted.per.game*train$V.total.rebounding.percentage
test$H.rebound.potential <- test$H.opponent.field.goals.attempted.per.game*test$H.total.rebounding.percentage
test$V.rebound.potential <- test$V.opponent.field.goals.attempted.per.game*test$V.total.rebounding.percentage

data$H.rebound.potential <- data$H.opponent.field.goals.attempted.per.game*data$H.total.rebounding.percentage
data$V.rebound.potential <- data$V.opponent.field.goals.attempted.per.game*data$V.total.rebounding.percentage

# fastbreaks per game
train$H.fastbreaks.per.game <- train$H.fastbreak.points.per.game/train$H.fastbreak.efficiency
train$V.fastbreaks.per.game <- train$V.fastbreak.points.per.game/train$V.fastbreak.efficiency
test$H.fastbreaks.per.game <- test$H.fastbreak.points.per.game/test$H.fastbreak.efficiency
test$V.fastbreaks.per.game <- test$V.fastbreak.points.per.game/test$V.fastbreak.efficiency

data$H.fastbreaks.per.game <- data$H.fastbreak.points.per.game/data$H.fastbreak.efficiency
data$V.fastbreaks.per.game <- data$V.fastbreak.points.per.game/data$V.fastbreak.efficiency



# Check correlation
engineered_cols <- c("Total.Score","H.efficiency.ratio","V.efficiency.ratio","H.points.per.possession","V.points.per.possession",
                     "H.points.per.scoring.possession","V.points.per.scoring.possession","H.scoring.possessions",
                     "V.scoring.possessions","H.foul.capitalization","V.foul.capitalization","H.point.opp.on.foul",
                     "V.point.opp.on.foul","H.opponent.agressiveness","V.opponent.agressiveness",
                     "H.matchup.free.throw.contribution","V.matchup.free.throw.contribution",
                     "H.rebound.potential","V.rebound.potential",
                     "H.fastbreaks.per.game","V.fastbreaks.per.game")
engineered_data <- train[ , (names(train) %in% engineered_cols)]
correlation_matrix <- cor(engineered_data)
totalScoreEngineeredCorrelationCoeffs <- correlation_matrix[,"Total.Score"]
totalScoreEngineeredCorrelationCoeffs <- totalScoreEngineeredCorrelationCoeffs[order(totalScoreEngineeredCorrelationCoeffs, decreasing=TRUE)]
totalScoreEngineeredCorrelationCoeffs

#### Multicollinearity Analysis ####

#### Correlation analysis between predictors and Total Score####

# Get only the numeric attributes
numericValues <- train[,which(sapply(train, is.numeric))]
# Filter out entries which contain NA
numericValues <- numericValues[complete.cases(numericValues),]
# Create the correlation matrix
correlation_matrix <- cor(numericValues)
totalScoreCorrelationCoeffs <- correlation_matrix[,"Total.Score"]
# Order the coefficients in descending order
totalScoreCorrelationCoeffs <- totalScoreCorrelationCoeffs[order(totalScoreCorrelationCoeffs, decreasing=TRUE)]
totalScoreCorrelationCoeffs
high_correlation <- c("Total.Score","Line","H.points.per.game","V.points.per.game","H.field.goals.made.per.game","V.field.goals.made.per.game")

# kill features which have correlation coefficient to total.score less than threshold
threshold = 0.01
drops <- character(0)
for (i in 1:length(totalScoreCorrelationCoeffs)){
  if(abs(totalScoreCorrelationCoeffs[[i]])<threshold){
    drops <- c(drops,names(totalScoreCorrelationCoeffs[i]))
  }
  
}
train <- train[ , !(names(train) %in% drops)]
test <- test[ , !(names(test) %in% drops)]

current_accuracy <- get_accuracy_of_given_data(train,test)
current_accuracy

#### Correlation analysis between predictors #### 
numericValues <- train[,which(sapply(train, is.numeric))]
# Filter out entries which contain NA
numericValues <- numericValues[complete.cases(numericValues),]
# Remove columns which we know are most important
numericValues <- numericValues[,!(names(numericValues) %in% high_correlation)]
# Create the correlation matrix
correlation_matrix <- cor(numericValues)
correlation_matrix[upper.tri(correlation_matrix)] <- 0
diag(correlation_matrix) <- 0
train.new <- train[,!apply(correlation_matrix,2,function(x) any(abs(x) > 0.99))]
strong_columns <- colnames(train.new)
highly_correlated_columns <- colnames(numericValues[ , !(names(numericValues) %in% strong_columns)])

train <- train[ , !(names(train) %in% highly_correlated_columns)]
test <- test[ , !(names(test) %in% highly_correlated_columns)]

current_accuracy <- get_accuracy_of_given_data(train,test)
current_accuracy

#### Assess correlation of highly correlated variable #### 

high_correlation_data <- train[ , (names(train) %in% high_correlation)]
high_correlation_data$Total.Score <- NULL
correlation_matrix <- cor(high_correlation_data)
correlation_matrix

#### Best Subset using model selection ####
regression <- lm(Total.Score~.,train)
step <- stepAIC(regression, direction="both")


#### Evaluate the regression model #### 

#evaluate the regression model to see if feature engineering is helping

get_accuracy_of_given_data <- function(train,test){
regression <- lm(Total.Score~.,train)

pred <- predict(regression, test)

actuals_preds <- data.frame(cbind(actuals=test$Total.Score, predicteds=pred))

betting_strat_data <- data.frame(cbind(betting_line=test$Line,actual_score=test$Total.Score, predicted_score=pred))
for (i in 1:nrow(betting_strat_data)){
  # actual result
  # under = 0, over = 1
  if(betting_strat_data$betting_line[i] > betting_strat_data$actual_score[i]){
    betting_strat_data$actual_ou_result[i] = 0
  }else if (betting_strat_data$betting_line[i] < betting_strat_data$actual_score[i]){
    betting_strat_data$actual_ou_result[i] = 1
  }else{
    betting_strat_data$actual_ou_result[i] = -1
  }
  # predicted result
  if(betting_strat_data$betting_line[i] > betting_strat_data$predicted_score[i]){
    betting_strat_data$predicted_ou_result[i] = 0
  }else if (betting_strat_data$betting_line[i] < betting_strat_data$predicted_score[i]){
    betting_strat_data$predicted_ou_result[i] = 1
  }else{
    betting_strat_data$predicted_ou_result[i] = -1
  }
}

betting_data = data.frame(cbind(actual_result=betting_strat_data$actual_ou_result,predicted_result=betting_strat_data$predicted_ou_result))
# eliminate -1's - this is where the betting line matches the actual game score. In this case no one wins/loses
betting_data<-betting_data[!(betting_data$actual_result==-1 | betting_data$predicted_result==-1),]
betting_data$actual_result <- as.factor(betting_data$actual_result)
betting_data$predicted_result <- as.factor(betting_data$predicted_result)
betting_data

RMSE_error<-sqrt(mean((actuals_preds$actuals-actuals_preds$predicteds)**2))
RMSE_error

CM <- confusionMatrix(betting_data$predicted_result,betting_data$actual_result)
return(CM$overall["Accuracy"])
}

#### Export data ####

selected_columns <- colnames(train)
output_data <- data[ , (names(data) %in% selected_columns)]

write.csv(output_data,'nba_data_v2.csv')

