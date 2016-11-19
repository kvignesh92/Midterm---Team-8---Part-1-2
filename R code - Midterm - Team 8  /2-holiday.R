#get holidays from webpage
htmlpage<-read_html("http://www.timeanddate.com/calendar/?year=2013&country=24")
#scrape CSS
holiday<-html_nodes(htmlpage,"#ch1 .lpad td:nth-child(1) , #ch1 .co1")
#get the text out of node
holiday1<-html_text(holiday)
#convert text to table
holiday2<-read.table(text = holiday1, sep = ".", colClasses = "character")
#name the column
names(holiday2)[1]<-"day"
names(holiday2)[2]<-"mont"
#translate finnish to numeric month
fin_month = matrix(c(" tam"," maa"," huh"," tou"," kes"," mar"," jou",1,3,4,5,6,11,12),nrow=7,ncol=2) 
fin_month<-as.data.frame(as.matrix(fin_month))
#name the columns
names(fin_month)[1]<-"mont"
names(fin_month)[2]<-"month"
fin_month$mont<-as.character(fin_month$mont)
fin_month$month<-as.character(fin_month$month)
#convert month to number
holidays<-merge(holiday2,fin_month,by=c("mont"))
holidays$mont<-1
holidays<-as.data.frame(holidays)
#male the day and month column numeric
holidays$month<-as.numeric(holidays$month);holidays$day<-as.numeric(holidays$day)
holidays<-holidays[!duplicated(holidays[,c('day','month')]),]
#merge the holdays data and find which days are a holiday
E2<-E1
E2<- merge(E2,holidays,by=c("day","month"), all.x=TRUE,all.y = FALSE)
E2[is.na(E2)] <- 0
#final data of PART 1.1
View(E2)
