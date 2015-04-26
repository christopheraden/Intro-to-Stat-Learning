generateData = function(n, m, p){
  #Takes in training and testing sample sizes and number of predictors.
  #Generates simple noise data
  yTrain = rnorm(n) #Responses
  xTrain = matrix(rnorm(n*p), ncol=p) #Predictors
  
  yTest = rnorm(m) #Responses
  xTest = matrix(rnorm(m*p), ncol=p) #Predictors
  list(yTrain=yTrain, xTrain=xTrain, yTest=yTest, xTest=xTest)
}

calcRisks = function(trainTest){
  # Takes in training and testing data in a list.
  #Unrolls into matrices, then calculates
  # the training and testing errors (scalars). Returns a 2-vector.
  yTrain = trainTest$yTrain
  xTrain = trainTest$xTrain
  yTest = trainTest$yTest
  xTest = trainTest$xTest

  #Compute training OLS estimate, get residuals/coeffs from it.
  trainLM = lm(yTrain ~ xTrain)
  trainResid = trainLM$resid
  trainCoeffs = trainLM$coefficients

  #Use training OLS estimates to make testing fits. Compute errors.
  testFits = cbind(rep(1,nrow(xTest)), xTest) %*% trainCoeffs
  testResid = yTest - testFits
  
  riskTrain = 1/length(yTrain) * sum(trainResid^2)
  riskTest = 1/length(yTest) * sum(testResid^2)
  c(riskTrain, riskTest)
}

n = 100 #Train sample size
m = 100 #Test sample size
p = 10 #Number of predictors

simErrors = t(sapply(1:100, function(i) calcRisks(generateData(n, m, p))))
par(mfrow=1:2)
hist(simErrors[,1], main='Histogram of Training Risk', xlab='Training Risk')
abline(v = mean(simErrors[,1]), col='red', lwd=4)
abline(v = 1, col='blue', lwd=4)
hist(simErrors[,2], main='Histogram of Testing Risk', xlab='Testing Risk')
abline(v = mean(simErrors[,2]), col='red', lwd=4)
abline(v = 1, col='blue', lwd=4)

#Problem 3
load("hw1prob3.Rdata")
#a 
logisticY1 = glm(y1~x, family='binomial')
plot(x[,1], x[,2], col=c("red","blue")[y1+1], pch = c('o', '+')[y1+1], 
     ylab="X2", xlab="X1", main='Logistic Regression Classifier')
legend('topleft', legend=unique(y1), col=c('blue', 'red'), pch=c('+', 'o'))

#Classification Rule: XB >= 0, classify to Y=1.
logisticY1$coefficients

truePred = cbind(y1, logisticY1$fitted.values >= 0.5) #Obs Y1 and Pred Y1
table(Actual=truePred[,1], Predicted=truePred[,2])
(101+87)/length(y1) #Misclassification Rate = .235

#b
plot(x[,1], x[,2], col=c("red","blue")[y1+1], pch = c('o', '+')[y1+1], 
     ylab="X2", xlab="X1", main='Logistic Regression Classifier')
legend('topleft', legend=unique(y1), col=c('blue', 'red'), pch=c('+', 'o'))

