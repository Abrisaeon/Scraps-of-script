##Script for calculating daily and monthly totals from event based rainfall data
#Abri de Buys & Sedzani Mahuluhulu

#Packages required
install.packages("plyr")
library(plyr)
library(dplyr)

#Set your working directory, for example:
setwd("C:/Users/ABRI/Desktop")

#Read in the raw .csv file as it has been exported from the ObsDB for example:
dat<-read.csv("try2.txt", header=F, sep="\t", skip=1, fileEncoding="UTF-16LE") ##Note the .csv encoding had to be specified

#Set the column names
colnames(dat)<- c("Record","Timestamp", "Rain")

#Specify a format for the date
dat$Dates <- strptime(dat$Timestamp, "%m/%d/%Y")

##For DAILY totals
#Select the columns you want to use and create a new dataframe "d" from them
d<-subset(dat, select = c(Dates,Rain))

#Change cumulative rainfall to 0.2mm increments so it can be summed
d$Rain <- ifelse((d$Rain> 0),0.2,0)

#Make NA zero so that values can be summed
d$Rain[is.na(d$Rain)] <- 0

#Change the clas of the "Dates" column to character
d$Dates<- as.character(d$Dates)

#Create a dataframe y with "Rain" summed by "Dates"
y<-ddply(d,.(d$Dates),summarise, Rain = sum(Rain))

#Set the column names
colnames(y)<- c("Dates", "Rain")

write.csv(y, "C:/Users/ABRI/Desktop/new.csv", quote=F, row.names=T, na = "NA")

