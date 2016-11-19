#look for weather station
# Refere : http://stackoverflow.com/questions/39909142/r-weather-underground-how-to-use-longitude-and-latitude-to-get-the-closest-a
library(XML)
library(ggmap)
library(rwunderground)
mydata<-read.csv("Finland_addresses_area.csv")

g<-geocode(as.character(mydata$X..address))
mydata <- data.frame(mydata[,1:2],g)


lt<-mydata$lat
ln<-mydata$lon
url<-paste0("http://api.wunderground.com/auto/wui/geo/GeoLookupXML/index.xml?query=",lt,",",ln,"")
my<-mydata
url[1]
for (i in 1:33) 
  {
    file<-xmlTreeParse(url[i],useInternalNodes=TRUE)
    airport<-xpathApply(file,"//icao",xmlValue)
    my[i,5]<-airport[2]
  }

distinct_airport = my %>% distinct(V5)  
names(my)[2]<-"addresss"
my$building<-as.character(my$building);my$addresss<-as.character(my$addresss)
# add station code to our data PART 1.1
###############################################E2<-E2 %>%  ungroup() %>%select(-sta_code)
E2<- merge(E2,my,by=c("building"), all.x=TRUE,all.y = FALSE)

View(E2)
