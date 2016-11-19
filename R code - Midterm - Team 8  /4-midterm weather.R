#get the date range
date <-  as.Date('20130101',format = "%Y%m%d")
date2 <-  as.Date('20131208',format = "%Y%m%d")
# create a sequence of every day in this year
s <- seq(date,to = date2, by='days') 
#modify the date format
datereplace<-str_replace_all(s,"-","/")
#get the dist number of airport
n=nrow(distinct_airport)
#scrape the html from URL
weathers<-data.frame(Time=as.character())
for(j in 1:n)
{
  code<-distinct_airport[j,1]
  dat<-paste0("https://www.wunderground.com/history/airport/",code,"/2013/1/1/DailyHistory.html?req_city=Mariehamn&req_statename=Finland&format=1")
  htmlpage<-read_html(dat)
  #get the text out of HTML
  weathe<-html_text(htmlpage)
  #make it a table
  weath<-read.table(text = weathe, sep = ",", colClasses = "character")
  #remove extra row with heading
  colnames(weath)=weath[1,]
  weath = weath[-1, ]
  #name 1st column as TIME
  names(weath)[1]<-"Time"
  #convert it to data frame
  weather<-data.frame(weath)
  #add a column ad date
  weather<- weather %>% rowwise() %>% mutate(date = "2013/1/1")
  #get the dist number of airport
  n=nrow(distinct_airport)
  #for loop to scrape the weather data of a paticular airport code frm Jan 1 2013 to Dec 8 2013
  weather<- weather %>% rowwise() %>% mutate(sta_code = code)
  
  for (i in 2:342)
  {  
    data<-paste0("https://www.wunderground.com/history/airport/",code,"/",datereplace[i],"/DailyHistory.html?req_city=Mariehamn&req_statename=Finland&format=1")
    htmlpage<-read_html(data)
    weathe<-html_text(htmlpage)
    weath<-read.table(text = weathe, sep = ",", colClasses = "character")
    colnames(weath)=weath[1,]
    weath = weath[-1, ]  
    names(weath)[1]<-"Time"
    weather1<-data.frame(weath)
    dat = datereplace[i]
    weather1<-weather1 %>% rowwise() %>% mutate(date=dat)
    weather1<-weather1 %>% rowwise() %>% mutate(sta_code = code)
    
    
    dplyr::tbl_df(weather)
    dplyr::tbl_df(weather1)
    weather<-dplyr::bind_rows(weather, weather1)
  } 
  weathers<-dplyr::bind_rows(weathers, weather)
  
}
utils::View(weathers)
write.csv(weather,file="weather.csv")