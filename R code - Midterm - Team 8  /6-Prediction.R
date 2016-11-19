install.packages("data.table")
install.packages("zoo")
install.packages("FNN")
install.packages("class")
install.packages("neuralnet")
install.packages("leaps")
install.packages("randomForest")
library(randomForest)
library(class)
library(zoo)
library(FNN)
library(data.table)
library(neuralnet)
library(leaps)

Data <- fread("part.csv")
Data[Data=='-']<-NA
Data[Data=='']<-NA
Data[Data=='N/A']<-NA
Data$Wind.SpeedMPH[Data$Wind.SpeedMPH=='-9999.0']<-NA
Data$Wind.SpeedMPH<-ifelse(Data$Wind.SpeedMPH=='Calm','0',Data$Wind.SpeedMPH)

#Replace NA using locf
Data1<-na.locf(Data)
Data1<-na.locf(Data1,fromLast=TRUE)   


#Data1[,.N , by=Events ] # check For NA

#Convert to Numeric data type
DT <-Data1[, Time:=as.numeric(Time)]
DT <-Data1[, TemperatureF:=as.numeric(TemperatureF)]
DT <-Data1[, Dew.PointF:=as.numeric(Dew.PointF)]
DT <-Data1[, Humidity:=as.numeric(Humidity)]
DT <-Data1[, Sea.Level.PressureIn:=as.numeric(Sea.Level.PressureIn)]
DT <-Data1[, VisibilityMPH:=as.numeric(VisibilityMPH)]
DT <-Data1[, Wind.SpeedMPH:=as.numeric( Wind.SpeedMPH)]
DT <-Data1[, Gust.SpeedMPH:=as.numeric(Gust.SpeedMPH)]
DT <-Data1[, WindDirDegrees:=as.numeric( WindDirDegrees)]
DT <-Data1[, day:=as.numeric(day)]
DT <-Data1[, month:=as.numeric(month)]
DT <-Data1[, meternumb:=as.numeric(meternumb)]
DT <-Data1[, Consumption:=as.numeric(Consumption)]
DT <-Data1[, `area_floor _m.sqr`:=as.numeric(`area_floor _m.sqr`)]
DT <-Data1[, DayofWeek:=as.numeric(DayofWeek)]
DT <-Data1[, Weekday:=as.numeric(Weekday)]
DT <-Data1[, Peakhour:=as.numeric(Peakhour)]
DT <-Data1[, mont:=as.numeric(mont)]
DT <-Data1[, BuildingID:=as.numeric(BuildingID)]
DT <-Data1[, type:=as.numeric(as.factor(type))]
DT <-Data1[, sta_code:=as.numeric(as.factor(sta_code))]
DT <-Data1[, Events:=as.numeric(as.factor(Events))]


DT[, V1:=NULL]
DT[, Conditions:=NULL]
DT[, PrecipitationIn:=NULL]
DT[, Sea.Level.Pressurein:=NULL]
DT[, Wind.Direction:=NULL]
DT[, DateUTC:=NULL]
DT[, date:=NULL]
DT[, min:=NULL]
DT[, year:=NULL]
DT[, addresss:=NULL]
DT[, lon:=NULL]
DT[, lat:=NULL]
DT[, building:=NULL]
DT[, VisibilityMi:=NULL]
DT[, '\t address':=NULL]

#Visual Outlier
DT[DT==-9999.0]<-NA
DT_1<-na.locf(DT)
DT_1<-na.locf(DT_1,fromLast=TRUE)   

normalize<- function(x) { return((x - min(x))/(max(x)-min(x))) }
DT_1<-normalize(DT_1)

summary(DT_1$TemperatureF)


'outlier_values <- boxplot.stats(DT_1$TemperatureF)$out 
boxplot(DT_1$TemperatureF, main="TemperatureF", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

name_factors=c('')
DT_2<-DT_1

for (i in 1:22){

  outlier_values <- boxplot.stats(DT_1[[2]])$out 
  
  boxplot(DT_1[[2]], boxwex=0.1)
  a<-mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
  DT_2[DT_2[[i]]== a] <-NA
  DT_3<-na.locf(DT_2)
  DT_3<-na.locf(DT_3,fromLast=TRUE)   
  
}'
#store in new data table
D1t<-DT_1
#We create a new column to divide Consumption / Area Square
D1t$Normalised<-D1t$Consumption/D1t$`area_floor _m.sqr`

#add base hour rate 
D1t$BaseHourRate <- ifelse((D1t$Normalised > mean(D1t$Normalised)),1 ,0)

#New data table D1t removing Consumption and area 
D1t[, Consumption:=NULL]
D1t[, `area_floor _m.sqr`:=NULL]

#70% of the sample size
sample_size <- floor(0.70 * nrow(D1t))

#Set the seed to make your partition reproductible
set.seed(111)
train_ind <- sample(seq_len(nrow(D1t)), size = sample_size)

#knn for base hour rate 
trainingData <- D1t[train_ind, ]
testData <- D1t[-train_ind, ]

train_target<-trainingData[[21]]
test_target<-testData[[21]]


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
trainingData1<-as.matrix(trainingData)
testData1<-as.matrix(testData)
m1<-knn(train = trainingData,test=testData,cl=train_target,k=5)

anova(m1)

table(m1,test_target)
mean(test_target==m1)

summary(m1)

#####################Random forest##########################

install.packages("h2o")
library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(trainingData)
test.h2o <- as.h2o(testData)
y.dep <- 21
x.indep <- c(1:20)

system.time(
  rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = 
                   train.h2o, ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122)
)


#check variable importance
h2o.varimp(rforest.model)

#making predictions on unseen data
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))
sub_rf <- data.frame(Normalised =  predict.rforest$predict)
