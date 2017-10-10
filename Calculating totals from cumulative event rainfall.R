#####Calculating totals from cumulative event rainfall
#####Abri de Buys - December 2016

##The purpose of this script is to process cumulative event based rainfall data as recorded by a Hobo Pendant Event logger,
##into daily or monthly summaries and graph it.

#Data prep: 
#This script uses as input the .txt output obtained by "Export table data" (File -> Export table data) in HOBOware Pro.
------------------------------------------------------------------------------------------------------------------------------------------------------------

#The following packages are required:
install.packages("dplyr")  
install.packages("ggplot2")

#Run the libraries 
library(plyr)
library(ggplot2)

#Set your working directory
setwd("H:/SCIENCE/SAEON_ABSTRACTS/Data Management/Projects/Mountain catchment streamflow monitoring/Data/Jonkershoek_March2009 to current/Jonkershoek_Tipping bucket gauges/2016/28 October 2016")

#Read in the raw .txt file as it has been exported from the HOBOware Pro, for example:
dat<-read.table("5B_Bosboukloof28Oct2016.txt", header=T, sep="\t", dec=".", skip=1)

#Rename the 4th column as "Rain"
colnames(dat)[4]<-"Rain"

#Specify a format for the date
dat$Date <- strptime(dat$Date.Time..GMT.02.00, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
dat$Date <- strptime(dat$Date, "%Y-%m-%d", tz="Africa/Johannesburg")

#Create a column called "Dates" that includes only the month and day if you want to calculate daily totals (or the year and month if you want monthly totals)
dat$Dates <- as.factor(format(dat$Date,'%m%d'))   #for daily totals run this line only
dat$Dates <- as.factor(format(dat$Date,'%Y%m'))   #for monthly totals run this line only

#Select the columns you want to use and delete the rest from you dataframe
dat<-subset(dat, select = c(Dates, Rain ))

#Make NA zero so that values can be summed
dat$Rain[is.na(dat$Rain)] <- 0

#Make cumulative rainfall values all = 0.2 so they can be summed
dat$Rain[which(dat$Rain > 0)] <- as.numeric(0.2)

#Create a dataframe x with "DataValue" summed by "Dates"
x<-ddply(dat,.(Dates),summarise, Rain = sum(Rain))


#Plot your dataframe (x) 
ggplot(x, aes(x=Dates, y=Rain)) + geom_bar(stat="identity", colour="black") + ylim(c(0, 70)) + ggtitle("5B_Bosboukloof daily")

