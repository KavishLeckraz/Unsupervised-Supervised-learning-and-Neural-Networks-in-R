seeds = read.csv('/Users/KavishLeckraz 1/Downloads/seeds_dataset.csv', sep=",")
seedsreal = read.csv('/Users/KavishLeckraz 1/Downloads/seeds_real.csv', sep=",")
seedsdata = cbind(seeds,seedsreal)

# seedsvalues = na.omit(seedsvalues) 
# seedsvalues = scale(seedsvalues)
# seedsclass = na.omit(seedsclass) 
# seedsclass = scale(seedsclass)

seeds_rand = seedsdata[sample(209,209),]
seedsclass = seeds_rand[,8]
seedsvalues = seeds_rand[,-8]

# set up a training set 70 %
seedsclassTrain = seedsclass[1:146]
seedsvaluesTrain = seedsvalues[1:146,]

# and testset 30 %
seedsclassTest = seedsclass[146:209]
seedsvaluesTest = seedsvalues[146:209,]

# build a decision tree with the command rpart:
library(rpart)
fit <- rpart(seedsclassTrain~., method="class", data=seedsvaluesTrain)

# plot decision tree
plot(fit, uniform=TRUE, main="Decision Tree for seedsData")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# test the classifier on the test set by calculating the predictions for each testcase in our test set:
treepred <- predict(fit, seedsvaluesTest, type = 'class')

# comparing to the actual test class values to get the accuracy:
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(treepred==seedsclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)
plot(seedsvaluesTest$Area,seedsvaluesTest$Perimeter, col=treepred)

# confusion matrix:
table_mat = table(seedsclassTest, treepred)
print(table_mat)

# pruning 
pfit1 <- prune(fit, cp=0.1)
plot(pfit1, uniform=TRUE, main="Pruned Decision Tree for SeedsData with cp = 0.1")
text(pfit1, use.n=TRUE, all=TRUE, cex=.8)
ptreepred <-predict(pfit1, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(ptreepred==seedsclassTest) #the number of correctly predicted
accuracyP1=ncorrect/n
print(accuracyP1)
plot(seedsvaluesTest$Area,seedsvaluesTest$Perimeter, col=ptreepred)

pfit2<- prune(fit, cp=0.2)
plot(pfit2, uniform=TRUE, main="Pruned Decision Tree for SeedsData with cp = 0.2")
text(pfit2, use.n=TRUE, all=TRUE, cex=.8)
ptreepred2 <-predict(pfit2, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(ptreepred2==seedsclassTest) #the number of correctly predicted
accuracyP2=ncorrect/n
print(accuracyP2)
plot(seedsvaluesTest$Area,seedsvaluesTest$Perimeter, col=ptreepred2)

pfit3<- prune(fit, cp=0.3)
plot(pfit3, uniform=TRUE, main="Pruned Decision Tree for SeedsData with cp = 0.3")
text(pfit3, use.n=TRUE, all=TRUE, cex=.8)
ptreepred3 <-predict(pfit3, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(ptreepred3==seedsclassTest) #the number of correctly predicted
accuracyP3=ncorrect/n
print(accuracyP3)
plot(seedsvaluesTest$Area,seedsvaluesTest$Perimeter, col=ptreepred3)

pfit4<- prune(fit, cp=0.4)
plot(pfit4, uniform=TRUE, main="Pruned Decision Tree for SeedsData with cp = 0.4")
text(pfit4, use.n=TRUE, all=TRUE, cex=.8)
ptreepred4 <-predict(pfit4, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(ptreepred4==seedsclassTest) #the number of correctly predicted
accuracyP4=ncorrect/n
print(accuracyP4)
plot(seedsvaluesTest$Area,seedsvaluesTest$Perimeter, col=ptreepred4)

library(class)
knn1pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=1)
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(knn1pred==seedsclassTest) #the number of correctly predicted
accuracyK1=ncorrect/n
print(accuracyK1)
plot(seedsvaluesTest$Area,seedsvaluesTest$Perimeter, col=knn1pred)

knn2pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=2)
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(knn2pred==seedsclassTest) #the number of correctly predicted
accuracyK2=ncorrect/n
print(accuracyK2)
plot(seedsvaluesTest$Area,seedsvaluesTest$Perimeter, col=knn2pred)

knn3pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=3)
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(knn3pred==seedsclassTest) #the number of correctly predicted
accuracyK3=ncorrect/n
print(accuracyK3)
plot(seedsvaluesTest$Area,seedsvaluesTest$Perimeter, col=knn3pred)

knn4pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=4)
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(knn4pred==seedsclassTest) #the number of correctly predicted
accuracyK4=ncorrect/n
print(accuracyK4)
plot(seedsvaluesTest$Area,seedsvaluesTest$Perimeter, col=knn4pred)

Pruned_accuarcy = c(accuracyP1,accuracyP2,accuracyP3,accuracyP4)
KNN_accuarcy = c(accuracyK1,accuracyK2,accuracyK3,accuracyK4)
Kvalues = c(1,2,3,4)
plot(Kvalues, KNN_accuarcy, type='o',col='red',ylim = c(0.5,1))
lines(Kvalues,Pruned_accuarcy, type="o",col="blue")

#fit <- rpart(seedsclassTrain~., method="class", data=seedsvaluesTrain)
#pfit<- prune(fit, cp=0.2)
#plot(pfit, uniform=TRUE, main="Pruned Decision Tree for SeedsData with cp = 0.2")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#fit <- rpart(seedsclassTrain~., method="class", data=seedsvaluesTrain)
#pfit<- prune(fit, cp=0.3)
#plot(pfit, uniform=TRUE, main="Pruned Decision Tree for SeedsData with cp = 0.3")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#fit <- rpart(seedsclassTrain~., method="class", data=seedsvaluesTrain)
#pfit<- prune(fit, cp=0.4)
#plot(pfit, uniform=TRUE, main="Pruned Decision Tree for SeedsData with cp = 0.4")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)




