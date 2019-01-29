#Data Cleaning
#Load the Uber Request data
uber<-read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)
str(uber)
View(uber)
#Checking for any duplicate entry in the data given
uber[which(duplicated(uber$Request.id)),]

#Making Date and Time pattern uniform in Request.timestamp,Drop.timestamp column in uber
library(lubridate)
uber$Request.timestamp<- parse_date_time(uber$Request.timestamp,orders=c("%d/%m/%Y %H:%M","%d-%m-%Y %H:%M:%S"),locale = "eng")
uber$Drop.timestamp<- parse_date_time(uber$Drop.timestamp,orders=c("%d/%m/%Y %H:%M","%d-%m-%Y %H:%M:%S"),locale = "eng")

#Derived metrices
#Splitting the date and time in Request.timestamp
uber$Req.date<-as.Date(uber$Request.timestamp)
#Deriving only absolute hours from the time for analysis
uber$Req.hours<-format(uber$Request.timestamp,"%H")


#As we are only interested in Cancelled and No Cars available status,Filtering the data
library(dplyr)
uber_filtered_city<-filter(uber,Pickup.point=="City",Status!="Trip Completed")
uber_filtered_airport<-filter(uber,Pickup.point=="Airport",Status!="Trip Completed")

#Plotting the Status of pickup point for entire uber data 
library(ggplot2)
ggplot(uber,aes(x=Status,fill=Pickup.point))+geom_bar()
#Plotting the per hour request 
ggplot(uber,aes(x=Req.hours,fill=Status))+geom_bar()
#plotting the frequency of requests that get cancelled or show 'no cars available'
# 1.For Airport
ggplot(uber_filtered_airport,aes(x=factor(Pickup.point),fill=factor(Status)))+geom_bar()+facet_wrap(~Req.date)
#2.For City
ggplot(uber_filtered_city,aes(x=factor(Pickup.point),fill=factor(Status)))+geom_bar()+facet_wrap(~Req.date)

#Plotting the Status of request w.r.t time
#1.For Airport
ggplot(uber_filtered_airport,aes(x=factor(Req.hours),fill=factor(Status)))+geom_bar()+facet_wrap(~Pickup.point)
#2.For City
ggplot(uber_filtered_city,aes(x=factor(Req.hours),fill=factor(Status)))+geom_bar()+facet_wrap(~Pickup.point)

#Supply-Demand Analysis
#Let us assume that for supply =1,Status is trip is completed 
#and for supply=0,Status is trip is cancelled and no cars available
uber_supply_demand<-uber
uber_supply_demand$supply[uber_supply_demand$Status=="Trip Completed"]<-1
uber_supply_demand$supply[uber_supply_demand$Status=="Cancelled"|uber_supply_demand$Status=="No Cars Available"]<-0

#plotting the frequency of Supply based on Req.time for both pickup points

ggplot(uber_supply_demand,aes(x=factor(Req.hours),fill=factor(supply)))+geom_bar()+facet_wrap(~Pickup.point)

#From Graph,Peak Demand for uber from Airport to city is from 17 to 22 hours
#Converting time in hours to numeric

uber_supply_demand$Req.hours<-as.numeric(uber_supply_demand$Req.hours)

#filtering data for peak hours for airport
uber_supply_demand_filtered_airport<-filter(uber_supply_demand,Pickup.point=="Airport",between(Req.hours,17,22))

#Aggregating data w.r.t Status
x<-aggregate(supply~Status,uber_supply_demand_filtered_airport,length)
#Percentage of cancelled Status during peak hours
x$supply[1]/sum(x$supply)*100
#5.50%
#Percentage of No Cars Available Status during peak hours
x$supply[2]/sum(x$supply)*100
#71.66%
#Percentage of Trip Completed Status during peak hours
x$supply[3]/sum(x$supply)*100
#22.84%
#Calculating Supply Demand Gap percentage
Airport_SupplyDemandGap<-nrow(uber_supply_demand_filtered_airport)-length(which(uber_supply_demand_filtered_airport$supply==1))
Airport_SupplyDemandGap/sum(x$supply)*100
#77.16%

#From Graph,Peak Demand for uber from City to Airport is from 5 to 9 hours

#filtering data for peak hours for City
uber_supply_demand_filtered_city<-filter(uber_supply_demand,Pickup.point=="City",between(Req.hours,5,9))
#Aggregating data w.r.t Status
y<-aggregate(supply~Status,uber_supply_demand_filtered_city,length)
#Percentage of cancelled Status during peak hours
y$supply[1]/sum(y$supply)*100
#48.89%
#Percentage of No Cars Available Status during peak hours
y$supply[2]/sum(y$supply)*100
#22.957%
#Percentage of Trip Completed Status during peak hours
y$supply[3]/sum(y$supply)*100
#28.145%
#Calculating Supply Demand Gap percentage
City_SupplyDemandGap<-nrow(uber_supply_demand_filtered_city)-length(which(uber_supply_demand_filtered_city$supply==1))
City_SupplyDemandGap/sum(y$supply)*100
#71.854%
    
#..............................END......................................


