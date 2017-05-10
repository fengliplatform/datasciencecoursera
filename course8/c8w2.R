# c8w2

library(caret)
library(kernlab)
data(spam)
dim(spam)
# 4601 58

str(spam)
set.seed(123)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

dim(training)

set.seed(234)
modelfit <- train(type~., data=training, method="glm")
modelfit
modelfit$finalModel

predictions <- predict(modelfit, newdata=testing)
predictions

confusionMatrix(predictions, testing$type)

# data slicing
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE) # return is training sets
class(folds)
sapply(folds, length)
# 4140 (90% as training set)
folds[[1]][1:10]
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=FALSE) # return is testing sets
class(folds)
sapply(folds, length)
# 460 (10% testing sets)

# use resample
samples <- createResample(y=spam$type, times=10, list=TRUE) # resample with replacement (bootstrap)
sapply(samples, length)
# 4601 (whole population)

# time data
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
class(folds)
names(folds)
folds$train[1]
folds$test[1]
folds$train[2]
folds$test[2]

##
install.packages("ISLR")
library(ISLR)
library(caret)
library(ggplot2)

data(Wage)
summary(Wage)
str(Wage)
head(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot="pairs")

qplot(age, wage, data=training)
qplot(age, wage, colour=jobclass, data=training)

qq <- qplot(age, wage, colour=education, data=training)
qq <- qq + geom_smooth(method="lm", formula=y~x)
qq

library("Hmisc")
cutWage <- cut2(training$wage,g=3)
table(cutWage)
class(cutWage)
str(cutWage)
cutWage

p1 <- qplot(cutWage, age, data=training, fill=cutWage)
p1 <- p1 + geom_boxplot()
p1

p2 <- qplot(cutWage, age, data=training, fill=cutWage,
            geom=c("boxplot","jitter"))
library(gridExtra)
grid.arrange(p1,p2, ncol=2)


#
t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)

# 
qplot(wage, col=education, data=training, geom=c("density"))

#### pre-process
# for model based approaches
# for extremely skewed data column.
library(caret)
library(kernlab)
data(spam)
dim(spam)

inTrain <- createDataPartition(y=spam$type, p=0.7, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# we found that capitalAve is extremely skewed! from boxplot and hist plot
qplot(y=training$capitalAve, x= 1, geom = "boxplot")
qplot(training$capitalAve)
mean(training$capitalAve)
#5 -mean is small
sd(training$capitalAve)
#29 - diviation is big
summary(training$capitalAve)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.000    1.576    2.276    5.112    3.697 1102.000 


# standardise skewed data in training set
capitalAveS <- (training$capitalAve - mean(training$capitalAve)) / sd(training$capitalAve)
mean(capitalAveS)
# 0
sd(capitalAveS)
# 1
qplot(y=capitalAveS, x= 1, geom = "boxplot")
qplot(capitalAveS)
# doesn't seem like normal distribution as there are outliers!!! to use BoxCox!
summary(capitalAveS)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.13810 -0.11880 -0.09526  0.00000 -0.04753 36.86000
# standardize reduced the scale 
qqnorm(capitalAveS)
# no way seems like normal distribution with outliers!! use BoxCox to remove outliers!!

# do the same thing in testing set with the same mean and sd from training set
testCapitalAveS <- (testing$capitalAve - mean(training$capitalAve)) / sd(training$capitalAve)
mean(testCapitalAveS)
# 0.0089
sd(testCapitalAveS)
# 1.2057

# use caret to do preprocessing
preObj <- preProcess(training[,-58], method=c("center", "scale"))
preObj
class(preObj)
trainCapitalAveS <- predict(preObj, training[,-58])$capitalAve
qplot(y=trainCapitalAveS, x= 1, geom = "boxplot")
#qplot(y=log(trainCapitalAveS), x= 1, geom = "boxplot")

#### use caret to preprocessing directly in training model
set.seed(32343)
modelFit <- train(type~., data=training, 
                  preProcess=c("center", "scale"),
                  method="glm")
modelFit


##### preprocessing - boxcox
preObj2 <- preProcess(training[,-58], method=c("BoxCox"))
trainingCapAveB <- predict(preObj2, training[,-58])$capitalAve
hist(trainingCapAveB)
# this more looks like a normal distribution than "standardise".
# from summary, we can see BoxCox removed outliers!
summary(trainingCapAveB)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.3982  0.6491  0.6439  0.9061  1.6420 
par(mar=c(3,3,2,2))
qqnorm(trainingCapAveB)

## impute
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05) == 1
selectNA
training$capAve[selectNA] <- NA

training$capAve == "NA"

as.numeric(training$capAve == "NA")
sum(as.numeric(training$capAve == "NA"))
sum(as.numeric(training$capAve == "NA"))

preObj <- preProcess(training[,-58], method=c("knnImpute"))
capAve <- predict(preObj, training[,-58])$capAve
capAve

capAveTrue <- training$capitalAve
capAveTrue <- (capAveTrue - mean(training$capitalAve)) /sd(training$capitalAve)

quantile(capAve - capAveTrue)

quantile((capAve - capAveTrue)[selectNA])
quantile((capAve - capAveTrue)[!selectNA])


#### covariates
# level 1: raw data to covariates
# text data -> data frame, creating columns

# level 2: transform tidy variates. for regressin, svm, NOT for trees.
spam$capitalAveSq <- spam$capitalAve ^ 2
log10(spam[,-58] + 1)

library(ISLR)
library(caret)
library(ggplot2)

data(Wage)
summary(Wage)
str(Wage)
head(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

# check variation of features
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

#############
library(ISLR)
library(caret)
library(ggplot2)

data(Wage)
summary(Wage)
str(Wage)
head(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

modelfit <- train(wage~age+education+jobclass, data=training, method="lm")
modelfit
finmodel <- modelfit$finalModel
finmodel

#dignostics
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(finmodel)
par(mfrow=c(1,1))

# 
qplot(finmodel$fitted.values, finmodel$residuals, col=training$race)
qplot(finmodel$fitted.values, finmodel$residuals, col=training$age)
qplot(finmodel$fitted.values, finmodel$residuals, col=training$maritl)

#
plot(finmodel$residuals)

trcontrol <- trc
modelfit <- train(wage~age+education+jobclass, data=training, method="lm",trainControl=trcontrol)

###


### predict lm
library(caret)
data(faithful)

head(faithful)

set.seed(334)
inTrain <- createDataPartition(y=faithful$eruptions, p=0.7, list=FALSE)
training <- faithful[inTrain,]
testing <- faithful[-inTrain,]

modelfit <- train(eruptions~waiting, data=faithful, method="lm")
modelfit
predictions <- predict(modelfit, newdata=testing)
myRMSE <- sqrt(sum((predictions - testing$eruptions)^2))
myRMSE


#### pca: principal component analysis
library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.7, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
# corelated predictors
M <- abs(cor(training[,-58]))
M
diag(M) <- 0
which(M>0.8, arr.ind=T)
# we found num415 and num857 two variable are highly corelated.
# we'll use pca to decouple them.
qplot(spam[,34], spam[,32])
smallspam <- spam[, c(34,32)]
prcom <- prcomp(smallspam)
qplot(prcom$x[,1], prcom$x[,2])
# pca: reduce variable, reduce noise
prcom$rotation

# pca
typecolor <- ((spam$type == "spam") * 1 + 1)
prComp <- prcomp(log10(training[,-58] +1))
prComp
# got PC1 to PC57 new variables. each of the new variables are a calculation from original variables.

library(ggplot2)
plot(prComp$x[,1], prComp$x[,2], col=typecolor)
plot(prComp$x[,3], prComp$x[,4], col=typecolor)

# use caret for pca
preProc <- preProcess(log10(spam[,-58] + 1), method = c("pca"))
spamPC <- predict(preProc, log10(spam[,-58] +1))
plot(spamPC[,1], spam[,2], col=typecolor)
plot(spamPC[,5], spam[,6], col=typecolor)

## use caret in one shot
modelfit <- train(type~., data=spam, method="glm", preProcess=c("pca"))
predictions <- predict(modelfit, newdata<-testing)
confusionMatrix(predictions, testing$type)


#Q2
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

dim(training)
str(training)

featurePlot(x=training[,c("Cement","BlastFurnaceSlag","FlyAsh","Water","Superplasticizer", "CoarseAggregate","FineAggregate","Age")],
            y=training$CompressiveStrength,
            plot="pairs")

agecut <- cut2(training$Age, g=5)
# plot by index
par(mar=c(3,3,1,1))
plot(training$CompressiveStrength, col=agecut)

flyashcut <- cut2(training$FlyAsh, g=3)
plot(training$CompressiveStrength, col=flyashcut)


# 
hist(training$Superplasticizer)
summary(training$Superplasticizer)
table(training$Superplasticizer)
length(training$Superplasticizer)

# Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

str(training)
ildata <- training[, c("IL_11","IL_13","IL_16","IL_17E","IL_1alpha","IL_3","IL_4","IL_5","IL_6","IL_6_Receptor","IL_7","IL_8")]
ildata

prcom <- prcomp(ildata)
prcom
prcom$rotation

preObj <- preProcess(ildata, method=c("pca"))
preObj$numComp
ildataPC <- predict(preObj, ildata)
ildataPC


#Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain, c("diagnosis","IL_11","IL_13","IL_16","IL_17E","IL_1alpha","IL_3","IL_4","IL_5","IL_6","IL_6_Receptor","IL_7","IL_8")]
testing = adData[-inTrain,]

str(training)

modelfit.default <- train(diagnosis~., data=training, method="glm")
predictions <- predict(modelfit.default, newdata=testing)
confusionMatrix(predictions, testing$diagnosis)
# 0.65
modelfit.pca <- train(diagnosis~., data=training, method="glm", preProcess=c("pca"))
predictions <- predict(modelfit.pca, newdata=testing)
confusionMatrix(predictions, testing$diagnosis)
#0.71



















































