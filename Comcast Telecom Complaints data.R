library(stringi)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)

setwd("C:/Users/Rahul Kumar/Desktop/Comcast Telecom Consumer Complaints")
getwd()
comcast_data <- read.csv("Comcast Telecom Complaints data.csv", header = TRUE)
View(comcast_data)
names(comcast_data)<- stri_replace_all(regex =  "\\.",replacement = "",str =names(comcast_data))
head(comcast_data)
str(comcast_data)
na_data <- is.na(comcast_data)
length(na_data[na_data==T])
comcast_data$Date <- dmy(comcast_data$Date)
monthly_count <- summarise(group_by(comcast_data, Month=as.integer(month(Date))), Count=n())
daily_count <- summarise(group_by(comcast_data, Date), count = n())
monthly_count <- arrange(monthly_count, Month)
ggplot(monthly_count, aes(Month, Count, label=Count))+
         geom_line()+ geom_point(size = 1.0)+
         geom_text()+ scale_x_continuous(breaks = monthly_count$Month)+
         labs(title = "Monthly Ticket Count", x= "Months", y="No. of Tickets")+
  theme(plot.title = element_text(hjust = 0.5))
p <- ggplot(monthly_count, aes(x=Month, y=Count))+
  geom_bar(stat = "identity", color = "black", fill="blue")+
  geom_text(aes(label=Count), vjust=1.2, size=3.0, color="white")+
  theme_minimal()
ggplot(daily_count, aes(as.POSIXct(Date), count))+
         geom_line()+
         geom_point(size = 1)+
         scale_x_datetime(breaks = "1 weeks", date_labels = "%d/%m")+
         labs(title = "Daily Ticket Count", x="Days", y="No. of Tickets")+
         theme(axis.text.x = element_text(angle = 75),
               plot.title = element_text(hjust = 0.5))
network_tickets<-contains(comcast_data$CustomerComplaint,match = 'network', ignore.case = T)
internet_tickets<-contains(comcast_data$CustomerComplaint, match = 'internet', ignore.case = T)
billing_tickets<-contains(comcast_data$CustomerComplaint, match = 'bill',ignore.case = T)
emailing_tickets<-contains(comcast_data$CustomerComplaint,match = 'email',ignore.case = T)
charges_tickets<-contains(comcast_data$CustomerComplaint,match = 'charge',ignore.case = T)
cable_tickets<-contains(comcast_data$CustomerComplaint,match = 'cable', ignore.case = T)
comcast_data$CustomerComplaint[network_tickets]<-"Network"
comcast_data$CustomerComplaint[internet_tickets]<-"Internet"
comcast_data$CustomerComplaint[billing_tickets]<-"Billing"
comcast_data$CustomerComplaint[emailing_tickets]<-"Email"
comcast_data$CustomerComplaint[charges_tickets]<-"Charge"
comcast_data$CustomerComplaint[cable_tickets]<-"Cable"
comcast_data$CustomerComplaint[-c(network_tickets,internet_tickets,billing_tickets,
                                  emailing_tickets,charges_tickets,cable_tickets)]<-"Others"
table(comcast_data$CustomerComplaint)
open_complaints<-(comcast_data$Status=="Open"|comcast_data$Status=="Pending")
closed_complaints<-(comcast_data$Status=="Closed"|comcast_data$Status=="Solved")
comcast_data$Status[open_complaints]<-"Open"
comcast_data$Status[closed_complaints]<-"Closed"
comcast_data<-group_by(comcast_data, State, Status)
bar_data<-summarise(comcast_data, count=n())
ggplot(as.data.frame(bar_data), mapping = aes(State, count))+
  geom_col(aes(fill=Status), width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16, colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Ticket Status Stacked Bar Chart by States", x="States", 
       y="No. of Tickets", fill="Status")
bar_data%>%
  filter(Status=="Open")->open_complaints
open_complaints[open_complaints$count==max(open_complaints$count),c(1,3)]
resolved_data<-group_by(comcast_data, Status)        
total_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))) 
resolved_data <- group_by(comcast_data,ReceivedVia,Status)
Category_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data)))
par(mfrow=c(1,2))
total<-ggplot(total_resloved,aes(x="",y=percentage,fill=Status))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start = 0)+
  geom_text(aes(label = paste0(round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
total
category_wise<-ggplot(Category_resloved,
                 aes(x= "",y =percentage,fill = Status))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(ReceivedVia,"-",round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
ggarrange(total,category,nrow = 1, ncol = 2)
category_wise