getwd()

setwd("/Users/Monit/Documents/UNH/DATA 801/R Programming")

unh = read.csv("NH_cleaned.csv", header = TRUE)

View(unh)

unhdf <- data.frame(lapply(unh, as.character), stringsAsFactors=FALSE) ## Converting Factors to Characters  


library(dplyr)

library(lubridate)

yy[order(as.Date(yy$monthn, format="%m")),]

unhdf$month = as.numeric(format(unhdf$stop_date,"%M"))

unhdf$stop_date<- week(as.Date(unh$stop_date))# Converting dates to months


## Filtering out outcome of tickets per month
dataunh<-unhdf%>%
  group_by(stop_date)%>%
  filter(stop_outcome=="Ticket")%>%
  select(stop_outcome)

unhfinal <- data.frame(table(dataunh$stop_date)) ## Table counts of tickets

countsdf <- table(marcount$stop_outcome)



dectemp<-unh%>% ## Filtering tickets out by week
  group_by(ymd(stop_date) >= as.Date("2014-12-25") & ymd(stop_date) <= as.Date("2014-12-31"))%>%
  filter(stop_outcome=="Ticket")%>%
  select(stop_outcome)
  

xtemp<-table(marcount$stop_outcome)

unhfinal$outcome<-c("529","1105","708","847","637","789","697","555","1094","865","797","655") ##Adding last week totals


> ggplot(unhfinal,aes(x=monthn, y = totalm))+ geom_bar(stat = "identity")
library(reshape2)

yy$monthn = factor(yy$monthn, levels = month.abb)

yy <- melt(unhfinal, c=("monthn"))


library(ggplot2)
ggplot(yy, aes(x=monthn, y = value, fill = variable))+geom_bar(stat="identity")

p<-ggplot(yy, aes(x=monthn, y = value, fill = variable))+geom_bar(stat="identity", position = "dodge")
p<-p + scale_fill_manual(values = c("red","purple"),labs("Time Period"), labels=c("Monthly","Last Week"))
p<- p + ggtitle("Monthly Tickets VS Last Week of Month") + xlab("Months")+ylab("Ticket Counts")
p



library(lubridate)

yyx<-yyx[order(as.)]

library(zoo)    
yyx<--yyx[order(as.yearmon(yyx$monthn,format="%m/%Y")),]
yyx<-yyx[order(as.yearmon(df_prod$date,format="%m/%Y")),]
