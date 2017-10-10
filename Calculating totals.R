##Script for calculating daily and monthly totals from event based rainfall data
#Abri de Buys & Sedzani Mahuluhulu

#Packages required
install.packages("dplyr")
install.packages("plyr")
library(plyr)
library(dplyr)


#Set your working directory, for example:
setwd("C:/Users/ABRI/Desktop")

#Read in the raw .csv file as it has been exported from the ObsDB for example:
dat<-read.csv("5B_Bosboukloof3Mar2017.txt", header=T, sep="\t", dec=".", skip=1)

#Set the column names
colnames(dat)<- c("Record","Timestamp","Temp", "Rain", "Battery")

#Specify a format for the date
dat$Date <- strptime(dat$Timestamp, "%Y/%m/%d %H:%M")
dat$Date <- strptime(dat$Date, "%Y-%m-%d")

#Create a column called "Months" that includes only the year and month if you want monthly totals)
dat$Months <- as.factor(format(dat$Date,'%Y%m'))   #monthly

##For MONTHLY totals
#Select the columns you want to use and create a new dataframe "m" from them
m<-subset(dat, select = c(Months, Rain))

#Change cumulative rainfall to 0.2mm increments so it can be summed
m$Rain <- ifelse((m$Rain> 0),0.2,0)

#Make NA zero so that values can be summed
m$Rain[is.na(m$Rain)] <- 0

#Create a dataframe x with "Rain" summed by "Months"
x<-ddply(m,.(Months),summarise, Rain = sum(Rain))


##For DAILY totals
#Select the columns you want to use and create a new dataframe "d" from them
d<-subset(dat, select = c(Date,Rain))

#Change cumulative rainfall to 0.2mm increments so it can be summed
d$Rain <- ifelse((d$Rain> 0),0.2,0)

#Make NA zero so that values can be summed
d$Rain[is.na(d$Rain)] <- 0

#Create a dataframe y with "Rain" summed by "Date"
y<-ddply(m,.(d$Date),summarise, Rain = sum(Rain))
