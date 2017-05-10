#c8w3

data(iris)
str(iris)
summary(iris)
table(iris$Species)

inTrain <- createDataPartition(iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)
library(caret)
library(ggplot2)
library(rpart)

#featurePlot(x=training[,c("Sepal.Length", "Sepal.Width", "Petal.Length","Petal.Width")],
featurePlot(x=training[,c("Sepal.Length", "Sepal.Width")],
                        y=training$Species,
            plot="pairs")
qplot(Petal.Width, Sepal.Width, color=Species, data=training)

modelfit <- train(Species~., data=training, method="rpart")
modelfit
modelfit$finalModel

plot(modelfit$finalModel, uniform=TRUE, main="Classification Tree")
text(modelfit$finalModel, use.n=TRUE, all=TRUE, cex=0.8)

#
library(rattle)
fancyRpartPlot(modelfit$finalModel)

# prediction
prediction <- predict(modelfit, testing)
prediction

confusionMatrix(prediction, testing$Species)

# Bagging: bootstrap aggregating for non-linear models
# often used with trees - like random forest
# ozone data
install.packages("ElemStatLearn")
library(ElemStatLearn)
data(ozone)

ozone <- ozone[order(ozone$ozone),]
head(ozone)

featurePlot(x=ozone[, c("ozone","radiation","wind")],
            y=ozone$temperature,
            plot="pairs")


## rf
data(iris)
str(iris)
summary(iris)
table(iris$Species)

inTrain <- createDataPartition(iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)
library(caret)
library(ggplot2)
library(randomForest)

modelfit <- train(Species~., data=iris, method="rf")
modelfit

# boosting
# starts from simple classifiers in each iterations and combine them

# Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

str(segmentationOriginal)
dim(segmentationOriginal)

inTrain <- createDataPartition(segmentationOriginal$Class, p=0.7, list=FALSE)
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

table(training$Class)

set.seed(125)
modelfit <- train(Class~., data=training, method="rpart")
modelfit$finalModel

fancyRpartPlot(modelfit$finalModel)

predictions <- predict(modelfit, testing)
predictions
testing$Class
confusionMatrix(predictions, testing$Class)


# Q2
# cv
# k fold, k--> big, means more times of sampling with big sample size.
# 5 fold --> 10 fold --> leave one out
# so can be more bias to training set. 
# each CV results have small variance.

# Q3
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]

dim(olive)
str(olive)
olive$Area <- as.factor(olive$Area)
olive$Area <- as.numeric(olive$Area)

modelfit <- train(Area~., data=olive, method="rpart")
modelfit
modelfit$finalModel
fancyRpartPlot(modelfit$finalModel)

colMeans(olive[,-1])
newdata = as.data.frame(t(colMeans(olive[,-1])))

predictions <- predict(modelfit, newdata=newdata)
predictions

# Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
str(trainSA)
?SAheart
set.seed(13234)
modelfit <- glm(chd~age+alcohol+obesity+typea+tobacco+ldl, data=trainSA, family="binomial")
predictions <- predict(modelfit, testSA)
predictions.train <- predict(modelfit, trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd, predictions)
#0.294
missClass(trainSA$chd, predictions.train)
#0.259

modelSA <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                 data = trainSA, method = "glm", family = "binomial")
p.test <- predict(modelSA, testSA)
missClass(testSA$chd, p.test)
# 0.31
p.train <- predict(modelSA, trainSA)
missClass(trainSA$chd, p.train)


# Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

str(vowel.train)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
modelfit <- train(y~., data=vowel.train, method="rf")
varImp(modelfit)
#2 1 5 6 8 ...


#
install.packages("GGally")
library(GGally)
ggpairs(iris)

# use step() to do feature selectiong automatically?? for mtcar c7 project.




















