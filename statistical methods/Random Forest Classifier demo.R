#Exempelimplementation av en Random Forest Classifer
#https://www.stat.berkeley.edu/~breiman/randomforest2001.pdf


library(randomForest)
library(caret)


#St�ll in en random seed f�r reproducerbarhet
set.seed(123456)


#Dela det inbyggda v�xtklassifikationsetet i tr�ning och test
indexes <- sample(1:nrow(subset(iris)), size = 50)
iris.train <- subset(iris)[-indexes,]
iris.test <- subset(iris)[indexes,]


#Odla en Random Forest-modell p� tr�ningssetet. Art �r target, �vriga variabler features.
iris.forest <- randomForest(iris.train[1:4], as.factor(iris.train$Species), proximity=TRUE, importance=TRUE, ntree=2000, keep.forest=TRUE, replace=TRUE)


#Visa Out Of Bag-estimat av modellen (OBS! Finns fallgropar i tolkningen.)
print(iris.forest)
importance(iris.forest)


#Skapa en f�rvirringsmatris och g�r diagnostik p� den
confusionMatrix(predict(iris.forest, iris.test), iris.test$Species)


#Spara skogen och demo av �teranv�ndning
save(iris.rf,file = "iris_rfc.RData")
rm(iris.rf)
load("iris_rfc.RData")