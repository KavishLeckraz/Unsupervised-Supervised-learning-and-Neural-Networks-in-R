# Assessed Exercise 1

library(neuralnet)
#XOR gate input data
trainin = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1));
#XOR gate output data
trainout = rbind(0, 1, 1, 0);
#Combined XOR gate data
XORdat=cbind(trainout,trainin)
#train a neural network on the XOR data
set.seed(2)
NN = neuralnet(XORdat[,1]~., XORdat[,-1], hidden = c(3,3) , threshold =
                 0.001, stepmax = 1e+05, linear.output = FALSE)

# Now simulate using the same inputs used to train it:
testin = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1));
testout=rbind(0,1,1,0)
predict_testNN = compute(NN, testin)
predict_testNN$neurons
predict_testNN$net.result
predict_out = as.numeric(predict_testNN$net.result>0.5)
predict_out

# Assessed Exercise 2

winedata = read.csv('/Users/KavishLeckraz 1/Downloads/winedata2.csv', sep=",")
# Architecture 
# Random Sample
wineRand = winedata[sample(130,130),]

# separate the class and values into separate variables:
wineclass = wineRand[c(1)]
winevalues = wineRand[c(2,14)]

# change class values into 0 or 1
for (i in 1:nrow(wineclass)){ 
  if(wineclass$WineClass[c(i)] == 1){
    wineclass$WineClass[c(i)] = 0
  } else {
    wineclass$WineClass[c(i)] = 1
  }
}

# build a training set of the first half of the values and test set of the remaining 
# Set up a training set:
winevaluesTrain = winevalues[1:65, c(1,2)]
wineclassTrain = cbind(wineclass[1:65, c(1)])
winedat = cbind(wineclassTrain,winevaluesTrain)

# Train
set.seed(10)
NN = neuralnet(winedat[,1]~., winedat[,-1], hidden = c(3,3) , threshold =
                0.001, stepmax = 1e+05, linear.output = FALSE)

NN$weights
# and testset:
winevaluesTest = winevalues[66:130, c(1,2)]
wineclassTest = cbind(wineclass[66:130, c(1)])

# Test
predict_testNN = compute(NN, winevaluesTest)
predict_testNN$neurons
predict_testNN$net.result
predict_out = as.numeric(predict_testNN$net.result>0.5)
predict_out

# Accuracy

n = length(wineclassTest) # the number of test cases
ncorrect = sum(predict_out==wineclassTest) # the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)
plot(NN)
