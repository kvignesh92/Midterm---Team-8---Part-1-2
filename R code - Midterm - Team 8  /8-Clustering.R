#Kmeans clustering
fx <- trainingData[,data.table(kmeans(cbind(TemperatureF,sta_code,type),centers=2)$centers),by=BuildingID]
fx1 <- trainingData[,data.table(kmeans(cbind(TemperatureF,VisibilityMPH,Wind.SpeedMPH,sta_code,day,
                                            month,meternumb,type,Weekday,Peakhour),
                                       centers=2)$centers),by=BuildingID]

plot(fx1)
plot(fx)

#Hierarchial clustering
install.packages("fastcluster")
library('fastcluster')
df<-as.data.frame(trainingData)
dissim <- dist(df[1:100,1:22])
hr <- hclust(dissim)
plot(hr)

clusterCut <- cutree(hr, 4)
table(clusterCut, df$BuildingID[1:50])

# to check how buildingID influences the other two parameters
plot(x=df$TemperatureF[1:50],y=df$Events[1:50],color=df$BuildingID[1:50])