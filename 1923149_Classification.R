seedsdata = read.csv('/Users/KavishLeckraz 1/Downloads/seeds_dataset.csv', sep=",")
seedsreal = read.csv('/Users/KavishLeckraz 1/Downloads/seeds_real.csv', sep=",")
seedsdata = seedsdata[c(-209),]
names(seedsreal)[names(seedsreal) == 'X1'] <- 'Class'
seedsdata = cbind(seedsdata,seedsreal)

seeds_rand = seedsdata[sample(208,208),] # Randomly spread data
seedsclass = seeds_rand[,8]
seedsvalues = seeds_rand[,-8]

#set up a training set
seedsclassTrain = seedsclass[1:104]
seedsvaluesTrain = seedsvalues[1:104,]

#and testset
seedsclassTest = seedsclass[105:208]
seedsvaluesTest = seedsvalues[105:208,]

# build a decision tree with the command rpart:
library(rpart)
fit <- rpart(seedsclassTrain~., method="class", data=seedsvaluesTrain)
# plot decision tree
plot(fit, uniform=TRUE, main="Decision Tree for SeedsData")
text(fit, use.n=TRUE, all=TRUE, cex=.7)

# test the classifier on the test set by calculating the predictions
# for each testcase in our test set:
treepred <- predict(fit, seedsvaluesTest, type = 'class')
plot(seedsvaluesTest$Area, seedsvaluesTest$Perimeter, col=treepred,
     main='Scatterplot of Decision Tree output')

# comparing to the actual test class values to get the accuracy:
n = length(seedsclassTest) # the number of test cases
ncorrect = sum(treepred==seedsclassTest) # the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

# Pruning
pfit<- prune(fit, cp=0.1)
plot(pfit, uniform=TRUE, main="Pruned (cp=0.1) Decision Tree for SeedsData")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
treepred <- predict(pfit, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) # the number of test cases
ncorrect = sum(treepred==seedsclassTest) # the number of correctly predicted
accuracyP1=ncorrect/n
print(accuracyP1)
plot(seedsvaluesTest$Area, seedsvaluesTest$Perimeter, col=treepred,
     main='Scatterplot of Decision Tree output with cp = 0.1')

pfit<- prune(fit, cp=0.2)
plot(pfit, uniform=TRUE, main="Pruned (cp=0.2) Decision Tree for SeedsData")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
treepred <- predict(pfit, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) # the number of test cases
ncorrect = sum(treepred==seedsclassTest) # the number of correctly predicted
accuracyP2=ncorrect/n
print(accuracyP2)
plot(seedsvaluesTest$Area, seedsvaluesTest$Perimeter, col=treepred,
     main='Scatterplot of Decision Tree output with cp = 0.2')

pfit<- prune(fit, cp=0.3)
plot(pfit, uniform=TRUE, main="Pruned (cp=0.3) Decision Tree for SeedsData")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
treepred <- predict(pfit, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) # the number of test cases
ncorrect = sum(treepred==seedsclassTest) # the number of correctly predicted
accuracyP3=ncorrect/n
print(accuracyP3)
plot(seedsvaluesTest$Area, seedsvaluesTest$Perimeter, col=treepred,
     main='Scatterplot of Decision Tree output with cp = 0.3')

pfit<- prune(fit, cp=0.4)
plot(pfit, uniform=TRUE, main="Pruned (cp=0.4) Decision Tree for SeedsData")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
treepred <- predict(pfit, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) # the number of test cases
ncorrect = sum(treepred==seedsclassTest) # the number of correctly predicted
accuracyP4=ncorrect/n
print(accuracyP4)
plot(seedsvaluesTest$Area, seedsvaluesTest$Perimeter, col=treepred ,
     main='Scatterplot of Decision Tree output with cp = 0.4')

# KNN
library(class)
knn3pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=1)
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(knn3pred==seedsclassTest) #the number of correctly predicted
accuracyK1=ncorrect/n
print(accuracyK1)

knn3pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=2)
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(knn3pred==seedsclassTest) #the number of correctly predicted
accuracyK2=ncorrect/n
print(accuracyK2)

knn3pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=3)
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(knn3pred==seedsclassTest) #the number of correctly predicted
accuracyK3=ncorrect/n
print(accuracyK3)

knn3pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=4)
n = length(seedsclassTest) #the number of test cases
ncorrect = sum(knn3pred==seedsclassTest) #the number of correctly predicted
accuracyK4=ncorrect/n
print(accuracyK4)

kval = c(1,2,3,4)
accKNN = c(accuracyK1,accuracyK2,accuracyK3,accuracyK4)
accP = c(accuracyP1,accuracyP2,accuracyP3,accuracyP4)
plot(kval, accKNN, type='o',col='red',ylim = c(0.5,1),ylab = "Accuracy (correctly predicted classification)", xlab = "Number of Classification (K Values)",
     main='Comparing the accuracy between KNN (k-values, 1-4) & Pruned Trees (cp, 0.1-0.4) on SeedsData')
lines(kval,accP, type="o",col="blue")
legend(1, 0.6, legend=c("Pruned Trees", "K Nearest Neighbour"),
       col=c("blue","red"), lty = 1:1, cex=0.8)
