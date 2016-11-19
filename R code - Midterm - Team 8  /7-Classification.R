#70% of the sample size
sample_size <- floor(0.70 * nrow(D1t))

#Set the seed to make your partition reproductible
set.seed(111)
train_ind <- sample(seq_len(nrow(D1t)), size = sample_size)

#knn for base hour rate 
trainingData <- D1t[train_ind, ]
testData <- D1t[-train_ind, ]

train_target<-trainingData[[22]]
test_target<-testData[[22]]


#regression
#Exhaustive Search
regfit.full=regsubsets (Normalised~.,data=trainingData ,really.big = TRUE)
exhaustiveRegSummary =summary(regfit.full)
print(exhaustiveRegSummary)
par(mfrow=c(2,2))
plot(exhaustiveRegSummary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
plot(exhaustiveRegSummary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")


#Forward Search
regfit.fwd=regsubsets(Normalised~.,data=trainingData, method="forward") 
forwardSearchSummary=summary(regfit.fwd)
names(forwardSearchSummary)
forwardSearchSummary$rss
forwardSearchSummary$adjr2
coef(regfit.fwd,5)

par(mfrow=c(2,2))
plot(forwardSearchSummary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
plot(forwardSearchSummary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")


#Linear Regression

lm.fit = lm(Normalised ~., data = trainingData)
summary(lm.fit)
require(forecast)
accuracy(lm.fit)
plot(lm.fit)
L<-predict(lm.fit,testData)
accuracy(L,trainingData$Normalised)



#Regression with the best variable
lm.fit <-lm(BaseHourRate~ TemperatureF+VisibilityMPH+Wind.SpeedMPH+sta_code+day+
              month+BuildingID+meternumb+type+Weekday,data = trainingData)
regfit.fwd1=regsubsets(BaseHourRate~ TemperatureF+ VisibilityMPH+Wind.SpeedMPH +sta_code+day+month+BuildingID+meternumb+type+Weekday,data=trainingData, method="forward") 

#KNN REGRESSION
sqrt(24)
m1<-knn(train = trainingData,test=testData,cl=train_target,k=5)

