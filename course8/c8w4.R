#c8w4

library(ElemStatLearn)
data("prostate")

dim(prostate)
str(prostate)

head(prostate)


# 
library(ISLR)
data("Wage")
dim(Wage)
set.seed(1234)
inBuild <- createDataPartition(Wage$wage, p=0.7, list=FALSE)
validation <- Wage[-inBuild,]
buildData <- Wage[inBuild,]
set.seed(2345)
inTrain <- createDataPartition(buildData$wage, p=0.7, list=FALSE)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]

mod1 <- train(wage~., method="glm", data=training)
mod2 <- train(wage~., method="rf", data=training,
              trControl=trainControl(method="cv"),
              number=3)

pred1 <- predict(mod1, testing)
pred1
pred2 <- predict(mod2, testing)
pred2

summary(pred1 - pred2)

qplot(pred1, pred2, col=wage, data=testing)

# build a new model based on the two models
predDF <- data.frame(pred1, pred2, wage=testing$wage)
combmodelfit <- train(wage~., data=predDF, method="gam")

combpred <- predict(combmodelfit, newdata=predDF)

sqrt(sum((pred1 - testing$wage)^2))
sqrt(sum((pred2 - testing$wage)^2))
sqrt(sum((combpred - testing$wage)^2))

# predict on validation dataset
predval1 <- predict(mod1, validation)
predval2 <- predict(mod2, validation)
combdata <- data.frame(pred1=predval1, pred2=predval2)
combpredval <- predict(combmodelfit, newdata=combdata)


# time serise data
install.packages("quantmod")
library(quantmod)

head(GOOG)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
from.dat
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
to.dat
getSymbols("GOOG", src="google", from=from.dat, to=to.dat)
head(GOOG)
tail(GOOG)
str(GOOG)

mGoog <- to.monthly(GOOG)


# Q1
library(ElemStatLearn)

data(vowel.train)

data(vowel.test)
str(vowel.train)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
rf.fit <- train(y~., data=vowel.train, method="rf")
gbm.fit <- train(y~., data=vowel.train, method="gbm", verbos=FALSE)

rf.pred <- predict(rf.fit, vowel.test)
gbm.pred <- predict(gbm.fit, vowel.test)

confusionMatrix(rf.pred, vowel.test$y)
# 0.6147
confusionMatrix(gbm.pred, vowel.test$y)
# 0.5368

# accuracy the two methods agree
confusionMatrix(rf.pred, gbm.pred)
# 0.6797

# Q2
library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]
str(training)
set.seed( 62433)
rf.fit <- train(diagnosis~., data=training, method="rf")
gbm.fit <- train(diagnosis~., data=training, method="gbm")
lda.fit <- train(diagnosis~., data=training, method="lda")

rf.pred <- predict(rf.fit, testing)
gbm.pred <- predict(gbm.fit, testing)
lda.pred <- predict(lda.fit, testing)

combDF <- data.frame(rf.pred, gbm.pred, lda.pred, diagnosis=testing$diagnosis)
combmodel <- train(diagnosis~., data=combDF, method="gam")

comb.pred <- predict(combmodel, testing)

confusionMatrix(rf.pred, testing$diagnosis)$overall[1]
confusionMatrix(gbm.pred, testing$diagnosis)$overall[1]
confusionMatrix(lda.pred, testing$diagnosis)$overall[1]
confusionMatrix(comb.pred, testing$diagnosis)$overall[1]


#Q3
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]
str(training)
set.seed(233)
model <- train(CompressiveStrength~., data=training, method="lasso")
model
model$finalModel
predictions <- predict(model, testing)
plot.enet(model$finalModel, xvar = "penalty", use.color = TRUE)


#Q4
setwd("/home/vagrant")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", 
              destfile="./gaData.csv", method="curl")
list.files("./")
library(lubridate) # For year() function below
getwd()
dat = read.csv("/home/vagrant/gaData.csv")

training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

tstesting = ts()
#install.packages("forecast")
library(forecast)
model <- bats(tstrain)
pred <- forecast(model, h = length(testing$visitsTumblr), level = 95)
sum(pred$lower < testing$visitsTumblr & testing$visitsTumblr < pred$upper)/length(testing$visitsTumblr)


# Q5
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]


set.seed(325)
library(e1071)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
accuracy(pred_svm, testing$CompressiveStrength)
















































