library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(scales)

#reading of data
apr_data <- read.csv(file.choose())
may_data <- read.csv(file.choose())
jun_data <- read.csv(file.choose())
jul_data <- read.csv(file.choose())
aug_data <- read.csv(file.choose())
sep_data <- read.csv(file.choose())

#data formating
data<-rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)
head(data)
summary(data)
str(data)
data$Date.Time <- as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data$Date.Time <- ymd_hms(data$Date.Time)
data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label = TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label = TRUE))
data$hour <- factor(hour(hms(data$Time)))
data$minute <- factor(minute(hms(data$Time)))
data$second <- factor(second(hms(data$Time)))

#for analysis
hour_data<-data%>%
           group_by(hour)%>%
           summarise(total=n())
datatable(hour_data)

#plot1
ggplot(hour_data,aes(x=hour,y=total,fill=total))+
   geom_bar(stat="identity",fill="blue",colour="red")+
 ggtitle("Trips per hour")+
  theme(plot.title = element_text(colour="dark red",size=30))+
 scale_y_continuous(labels=comma)

#subdata creation for analysis
hour_month<-data%>%
  group_by(month,hour)%>%
  summarise(total=n())
head(hour_month)

#plot2
ggplot(hour_month,aes(hour,total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("trips pre hour and month")+
  scale_y_continuous(labels=comma)

#table
day<-data%>%
  group_by(day)%>%
  summarise(total=n())

head(day)

#plot3
ggplot(day,aes(day,total))+
  geom_bar(stat="identity",fill="lightblue",colour="red")+
  ggtitle("trips on each day of month")+
  scale_y_continuous(labels=comma)

#table
day_month<-data%>%
  group_by(month,dayofweek)%>%
  summarise(total=n())

head(day_month)

#plot4
ggplot(day_month,aes(month,total,fill=dayofweek))+
  geom_bar(stat="identity",position="dodge")+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=c(1:7))+
  ggtitle("trips by day and month")


#plot5
ggplot(data, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

#for heatmap
day_and_hour <- data %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
#heatmap 
ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")


