# add PART 1.1 and PART 1.2

#building consumption data
E3<-as.data.table(E2)
names(E3$V5)<-"sta_code";

#weather data
weather_m<-weathers
weather_m$Time<-substr(strptime(weather_m$Time, "%I:%M %p" ),11,19)
Time<-read.table(text = weather_m$Time, sep = ":", colClasses = "character")
weather_m$Time<-Time$V1
weather_m$min<-Time$V2
weather_m<-as.data.table(weather_m)
setkeyv(weather_m,c("Time","date","sta_code"))
weather_mm<-subset(unique(weather_m))

weather_mmm<- weather_mm[, Time:=as.numeric(Time)]
weather_done<- weather_mmm[, date:=as.Date(date)]

setkey(E3,V5,Date,hour)
setkey(weather_done,sta_code,date,Time)
E4<-weather_done[E3]
write.csv(E4,file="part.csv")
utils::View(E4)

E4[,.N,by=BuildingID]