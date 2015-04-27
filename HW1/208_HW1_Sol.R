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


#Problem 2
# Part A
income = read.csv("http://www-bcf.usc.edu/~gareth/ISL/Income1.csv")[,-1]
incomeModel = lm(Income ~ Education + I(Education^2) + I(Education^3), data=income)
pointwiseCI = predict(incomeModel, interval = "confidence")

multiplier = sqrt(qchisq(.95, 4))
familywiseCI = predict(incomeModel, se.fit=TRUE)
scheffeInterval = data.frame(fit = familywiseCI$fit,
                             lwr = familywiseCI$fit - multiplier * familywiseCI$se.fit,
                             upr = familywiseCI$fit + multiplier * familywiseCI$se.fit)

plot(Income ~ Education, data=income)
legend("topleft", c("Fitted Values", "Pointwise CI", "Familywise CI"), 
       lty = c(1, 2, 4), col = c("red", "blue", "purple")) #Add a legend
#Plot Pointwise CI
lines(income$Education, pointwiseCI[,1], col = "red")
lines(income$Education, pointwiseCI[,2], lty = 2, col = "blue")
lines(income$Education, pointwiseCI[,3], lty = 2, col = "blue")
#Plot Scheffe
lines(income$Education, scheffeInterval[,2], lty = 4, col = "purple")
lines(income$Education, scheffeInterval[,3], lty = 4, col = "purple")

#Problem 3
#a 
load("hw1prob3.Rdata")
logisticY1 = glm(y1~x, family='binomial')
plot(x[,1], x[,2], col=c("red","blue")[y1+1], pch = c('o', '+')[y1+1], 
     ylab="X2", xlab="X1", main='Logistic Regression Classifier')
legend('topleft', legend=unique(y1), col=c('blue', 'red'), pch=c('+', 'o'))
coef(logisticY1) #Classification Rule: b'X >= 0, classify to Y=1.

truePred = cbind(y1, logisticY1$fitted.values >= 0.5) #Obs Y1 and Pred Y1
table(Actual=truePred[,1], Predicted=truePred[,2])
(101+87)/length(y1) #Misclassification Rate = .235

#b
plot(x[,1], x[,2], col=c("red","blue")[y1+1], pch = c('o', '+')[y1+1], 
     ylab="X2", xlab="X1", main='Logistic Regression Classifier')
legend('topleft', legend=unique(y1), col=c('blue', 'red'), pch=c('+', 'o'))
a = -coef(logisticY1)[1] / coef(logisticY1)[3]
b = -coef(logisticY1)[2] / coef(logisticY1)[3]
abline(a, b, col='black', lty=1, lwd=3)

#c, d
logisticY1.X1Square = glm(y1~x + I(x[,1]^2), family='binomial')
plot(x[,1], x[,2], col=c("red","blue")[y1+1], pch = c('o', '+')[y1+1], 
     ylab="X2", xlab="X1", main='Logistic Regression Classifier')
legend('topleft', legend=unique(y1), col=c('blue', 'red'), pch=c('+', 'o'))
a = -coef(logisticY1.X1Square)[1] / coef(logisticY1.X1Square)[3]
b = -coef(logisticY1.X1Square)[2] / coef(logisticY1.X1Square)[3]
c = -coef(logisticY1.X1Square)[4] / coef(logisticY1.X1Square)[3]
curve(a + b*x + c*x^2, from=min(x[,1]), to=max(x[,1]), add = TRUE, lwd=3)
truePred = cbind(y1, logisticY1.X1Square$fitted.values >= 0.5)
table(Actual=truePred[,1], Predicted=truePred[,2])
(0 + 0)/length(y1)

#e
logisticY2 = glm(y2~x, family='binomial')
logisticY2.X1Square = glm(y2~x + I(x[,1]^2), family='binomial')
plot(x[,1], x[,2], col=c("red","blue")[y2+1], pch = c('o', '+')[y2+1], 
     ylab="X2", xlab="X1", main='Logistic Regression Classifiers with y2')
legend('topleft', legend=unique(y1), col=c('blue', 'red'), pch=c('+', 'o'))
a = -coef(logisticY2)[1] / coef(logisticY2)[3]
b = -coef(logisticY2)[2] / coef(logisticY2)[3]
abline(a = logisticCVInt, b = logistCVSlope, col='black', lty=1, lwd=3)
a = -coef(logisticY2.X1Square)[1] / coef(logisticY2.X1Square)[3]
b = -coef(logisticY2.X1Square)[2] / coef(logisticY2.X1Square)[3]
c = -coef(logisticY2.X1Square)[4] / coef(logisticY2.X1Square)[3]
curve(a + b*x + c*x^2, from=min(x[,1]), to=max(x[,1]), add = TRUE, lwd=3, col='orange')

truePredLinear = cbind(y2, logisticY2$fitted.values >= 0.5)
mean(truePredLinear[,1] != truePredLinear[,2])
truePredQuadratic = cbind(y2, logisticY2.X1Square$fitted.values >= 0.5)
mean(truePredQuadratic[,1] != truePredQuadratic[,2])

#f
logisticY2.X1Third = glm(y2~x + I(x[,1]^2) + I(x[,1]^3), family='binomial')
plot(x[,1], x[,2], col=c("red","blue")[y2+1], pch = c('o', '+')[y2+1], 
     ylab="X2", xlab="X1", main='Logistic Regression Classifiers with y2')
legend('topleft', legend=unique(y1), col=c('blue', 'red'), pch=c('+', 'o'))
a = -coef(logisticY2.X1Third)[1] / coef(logisticY2.X1Third)[3]
b = -coef(logisticY2.X1Third)[2] / coef(logisticY2.X1Third)[3]
c = -coef(logisticY2.X1Third)[4] / coef(logisticY2.X1Third)[3]
d = -coef(logisticY2.X1Third)[5] / coef(logisticY2.X1Third)[3]
curve(a + b*x + c*x^2 + d*x^3, from=min(x[,1]), to=max(x[,1]), add = TRUE, lwd=3, col='purple')
truePredCubic = cbind(y2, logisticY2.X1Third$fitted.values >= 0.5)
mean(truePredCubic[,1] != truePredCubic[,2])

