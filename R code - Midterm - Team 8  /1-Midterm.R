install.packages("zoo")
install.packages("translate")
library(translate)
library(data.table)
library(reshape)
library("dplyr")
library("tidyr")
library("zoo")
library(rvest)
library(XML)
library(stringr)


# Read the data set as data table and atomic variable for large dataset
mydata<-fread("Finland_addresses_area.csv")
mydata1<-fread("Finland_masked.csv")

#convert it to dplyr data frame
dplyr::tbl_df(mydata)
dplyr::tbl_df(mydata1)

# transpose meter number to columns
setkey(mydata1,type)
abc1<-mydata1[c("elect","Dist_Heating")]
#abc1[,.N,by=type]

#############################################################################utils::View(abc2)
#abc<-dcast(mydata1,BuildingID+vac+date+hour+meternumb~type,value.var = "Consumption",sum)
# melt it the meter no. back to rows
#abc1<- abc[,.(BuildingID,vac,date,hour,meternumb,Dist_Heating,elect)]
##################################################################################################
#assign uilding 9 and 27
abc1[, vac := ifelse(BuildingID==81909, "Building 27", vac)]
abc1[, vac := ifelse(BuildingID==82254|BuildingID==83427|BuildingID==84681, "Building 9", vac)]
#remove blank 
abc2<-abc1[!(vac=="                              ")] 
names(abc2)[2]<-"building"

#merge the address and area data with your data
setkey(abc2,building)
setkey(mydata,building)
Data_merge <- merge(abc2,mydata, all.x=TRUE)

#convert date column from int to date format
D <- transform(Data_merge, date = as.Date(as.character(date), "%Y%m%d"))
##...........................................................................utils::View(Data_merge)
#add year month and day column
E<-tidyr::separate(D, date, c("year","month","day"))
#get date column
E$Date <- as.Date(with(E, paste(year, month, day,sep="/")), "%Y/%m/%d")#add Date column
# get Day of week
E$DayofWeek<-as.POSIXlt(E$Date)$wday
#check for weekday
E$Weekday<-ifelse(E$DayofWeek > 0 & E$DayofWeek < 6, "1", "0")
#peek hour column
E1 <- E %>% mutate(hour = as.numeric(hour))
E1$Peakhour<-ifelse(E1$hour > 6 & E1$hour < 20, "1", "0")
# convert day and month to nymeric column
E1$month<-as.numeric(E1$month);E1$day<-as.numeric(E1$day)

E0<-as.data.table(E1)
E0[,.N,by=BuildingID]
