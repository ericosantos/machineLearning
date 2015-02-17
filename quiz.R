TP<-.99*.001
FP<-.01*.999
TP/(TP+FP)

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)


adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

data(concrete)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

featurePlot(x=mixtures[,-9],
            y = mixtures$CompressiveStrength,
            plot="pairs")
library(Hmisc)
library(ggplot2)

summary(training)
ggplot(training,aes(row(training)[,1],CompressiveStrength,colour = cut(training$Cement,breaks = 4))) + geom_point()
ggplot(training,aes(row(training)[,1],CompressiveStrength,colour = cut(training$BlastFurnaceSlag,breaks = 4))) + geom_point()
ggplot(training,aes(row(training)[,1],CompressiveStrength,colour = cut(training$FlyAsh,breaks = 4))) + geom_point()
ggplot(training,aes(row(training)[,1],CompressiveStrength,colour = cut(training$Water,breaks = 4))) + geom_point()
ggplot(training,aes(row(training)[,1],CompressiveStrength,colour = cut(training$Superplasticizer,breaks = 4))) + geom_point()
ggplot(training,aes(row(training)[,1],CompressiveStrength,colour = cut(training$CoarseAggregate,breaks = 4))) + geom_point()
ggplot(training,aes(row(training)[,1],CompressiveStrength,colour = cut(training$FineAggregate,breaks = 4))) + geom_point()
ggplot(training,aes(row(training)[,1],CompressiveStrength,colour = cut(training$Age,breaks = 4))) + geom_point()

hist(training$Superplasticizer)
hist(log(training$Superplasticizer))

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

names(training)[grepl("^IL",names(training))]
trainIL <- cbind(diagnosis=training$diagnosis,training[,grepl("^IL",names(training))])
testIL <- cbind(diagnosis=testing$diagnosis,testing[,grepl("^IL",names(testing))])
pairs(trainIL)
prep <- preProcess(trainIL[,-1],method = "pca",thresh = .8)
prep

mfit <- train(trainIL$diagnosis~.,method="glm",data=trainIL)
confusionMatrix(testIL$diagnosis,predict(mfit,testIL))

prep <- preProcess(trainIL[,-1],method = "pca",thresh = .8)
mfitpca <- train(trainIL$diagnosis~.,method="glm",data=predict(prep,trainIL[,-1]))
confusionMatrix(testIL$diagnosis,predict(mfitpca,predict(prep,testIL[,-1])))

############ 3 ##########

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

table(segmentationOriginal$Case)

training <- segmentationOriginal[segmentationOriginal$Case=="Train",]
testing <- segmentationOriginal[segmentationOriginal$Case=="Test",]
dim(training); dim(testing)

set.seed(125)
mdl = train(Class ~ .,data=training,method="rpart")
summary(mdl)
plot(mdl$finalModel)
text(mdl$finalModel, use.n=TRUE, all=TRUE, cex=.8)
pred = predict(mdl,testing)

attach(testing)
cv<-apply(testing[,4:119],2,function(x) mean(x,na.rm=T))
 


library(pgmm)
data(olive)
olive = olive[,-1]
mdl = train(Area ~ .,data=olive,method="rpart")
mdl$finalModel
predict(mdl,newdata = as.data.frame(t(colMeans(olive))))
library(tree)
mdl<-tree(Area ~ .,data=olive)
predict(mdl,newdata = as.data.frame(t(colMeans(olive))))


library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
mdl = train(chd ~ age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family = "binomial")
mdl
summary(mdl)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd,predict(mdl,testSA[,c("age","alcohol","obesity","tobacco","typea","ldl")]))
missClass(trainSA$chd,predict(mdl,trainSA[,c("age","alcohol","obesity","tobacco","typea","ldl")]))


library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y<-factor(vowel.train$y)
vowel.test$y<-factor(vowel.test$y)
set.seed(33833)
mdl = train(y ~ .,data=vowel.train,method="rf",prox=T)
mdl2 = train(y ~ .,data=vowel.train,method="rf",prox=F)
summary(mdl)
mdl$finalModel
varImp(mdl)
varImp(mdl2)
