#Exempelimplementation av en Random Forest Classifer
#https://www.stat.berkeley.edu/~breiman/randomforest2001.pdf


library(randomForest)
library(caret)


#Ställ in en random seed för reproducerbarhet
set.seed(123456)


#Dela det inbyggda växtklassifikationsetet i träning och test
indexes <- sample(1:nrow(subset(iris)), size = 50)
iris.train <- subset(iris)[-indexes,]
iris.test <- subset(iris)[indexes,]


#Odla en Random Forest-modell på träningssetet. Art är target, övriga variabler features.
iris.forest <- randomForest(iris.train[1:4], as.factor(iris.train$Species), proximity=TRUE, importance=TRUE, ntree=2000, keep.forest=TRUE, replace=TRUE)


#Visa Out Of Bag-estimat av modellen (OBS! Finns fallgropar i tolkningen.)
print(iris.forest)
importance(iris.forest)


#Skapa en förvirringsmatris och gör diagnostik på den
confusionMatrix(predict(iris.forest, iris.test), iris.test$Species)


#Spara skogen och demo av återanvändning
save(iris.rf,file = "iris_rfc.RData")
rm(iris.rf)
load("iris_rfc.RData")