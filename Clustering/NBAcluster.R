library(caret)
library(MASS)
library(dplyr)
library(factoextra)
library(dbscan)


data1 = read.csv("nba_data_v2.csv", header = TRUE)
set.seed(20)

# create new columns to represent percentage of points scored in the paint for home and away offense and defense
data1$H.paint.Tot <- data1$H.points.in.paint.per.game / (data1$H.points.from.2.pointers + data1$H.points.from.3.pointers + data1$H.free.throws.made.per.game) 
data1$V.opponent.paint.Tot <- data1$V.opponent.points.in.paint.per.game / (data1$V.opponent.points.from.2.pointers + data1$V.opponent.points.from.3.pointers + data1$V.opponent.free.throws.made.per.game) 

data1$V.paint.Tot <- data1$V.points.in.paint.per.game / (data1$V.points.from.2.pointers + data1$V.points.from.3.pointers + data1$V.free.throws.made.per.game) 
data1$H.opponent.paint.Tot <- data1$H.opponent.points.in.paint.per.game / (data1$H.opponent.points.from.2.pointers + data1$H.opponent.points.from.3.pointers + data1$H.opponent.free.throws.made.per.game) 

# Cluster each of the above columns using floor percentage, which accurately represents how efficient a team is at scoring
test11 = data1[,c("H.paint.Tot","H.floor.percentage")]
km11 <- kmeans(test11, 3)
fviz_cluster(km11, test11, frame = FALSE, geom = "point")

test12 = data1[,c("V.opponent.paint.Tot","V.opponent.floor.percentage")]
km12 <- kmeans(test12, 3)
fviz_cluster(km12, test12, frame = FALSE, geom = "point")
 
test21 = data1[,c("V.paint.Tot","V.floor.percentage")]
km21 <- kmeans(test21, 3)
fviz_cluster(km21, test21, frame = FALSE, geom = "point")
 
test22 = data1[,c("H.opponent.paint.Tot","H.opponent.floor.percentage")]
km22 <- kmeans(test22, 3)
fviz_cluster(km22, test22, frame = FALSE, geom = "point")

# Input into dataset for further feature manipulation
data1$H.off.cluster <- km11$cluster
data1$V.off.cluster <- km12$cluster
data1$H.def.cluster <- km21$cluster
data1$V.def.cluster <- km22$cluster

# Determine if a style advantage exists for either the home or away team and sort binaries accordingly
for(i in 1:nrow(data1)) {
  if(abs(data1[i,"H.off.cluster"] - data1[i,"V.def.cluster"]) > 1) {
    data1[i,"H.style.adv"] <- 1
  } else {
    data1[i,"H.style.adv"] <- 0
  }

  if(abs(data1[i,"V.off.cluster"]-data1[i,"H.def.cluster"]) > 1) {
    data1[i,"V.style.adv"] <- 1
  } else {
    data1[i,"V.style.adv"] <- 0
  }
}

str(data1$V.style.adv)
str(data1$H.off.cluster)
str(data1$V.def.cluster)
str(data1$H.style.adv)

# Write new dataset onto a csv
write.csv(data1,'nba_data_v3.csv')
