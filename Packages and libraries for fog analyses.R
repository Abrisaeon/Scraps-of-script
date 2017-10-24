#Fog project - Abri de Buys, 2015-2017

#Install the following packages

#install.packages(c("zoo", "latticeExtra", "polynom", "car", "Hmisc","reshape"))
#install.packages("sp")
#install.packages("hydromad", repos="http://hydromad.catchment.org")
#install.packages("C:\\Users\\ABRI\\Downloads\\hydromad_0.9-16.tar.gz", repos = NULL, type="source")
#install.packages("chron")
#install.packages("playwith")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("devtools")
#install.packages("gridExtra") ##this is to create multiple plots in the same window
#install.packages("arules") ##this allows categorization/division into classes
#install.packages("reshape2")
#install.packages("lubridate")
#install.packages("RColorBrewer")
#install.packages("climdex.pcic")
#install.packages("seas")
#install.packages("xts")
#install.packages("dygraphs")
#install.packages("agricolae")
#install.packages("multcompView")

install_github('metvurst', 'tim-salabim')
install_github("easyGgplot2", "kassambara")

#Install libraries
library(hydromad)
library(chron)
library(playwith)
library(plyr)
library(ggplot2)
library(gridExtra)
library(devtools)
library(arules)
library(reshape2)
library(lubridate)
library(RColorBrewer)
library(dplyr)
library(scales) 
library(seas)
library(xts)
library(dygraphs)
library(agricolae)
library(multcompView)


##Data cleaning
#***************************************************************************************************
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis")
dat<-read.csv("Langrivier stream level.csv",skip=0) ##edit your .csv file to only include one row of column headers before reading it in

#Identify and remove duplicate rows
duplicated(dat) ##Identify
dat[duplicated(dat), ] ##View duplicates
dat[!duplicated(dat), ] ##View dataframe without duplicates
duplicateless<- dat[!duplicated(dat), ] ##Create a dataframe of the "duplicateless" data
write.csv(duplicateless,"C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Raw data/F8duplicateless.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")


#Find gaps and fill them with NA
ts <- seq.POSIXt(as.POSIXct("2011/08/24 15:00",'%m/%d/%y %H:%M'), as.POSIXct("2017/10/01 00:00",'%m/%d/%y %H:%M'), by="hour")  ##Insert the start and end TIMESTAMPS of your raw data. This creates a TIMESTAMP column without any missing hours
ts <- format.POSIXct(ts,'%Y/%m/%d %H:%M')  ##Ensure the TIMESTAMP format in the ts is the same as in your raw data
df <- data.frame(timestamp=ts) ##Change your ts into a dataframe before you can join it to your raw data
dat<-read.csv("Langrivier stream level.csv",skip=0) ##Read in whatever raw data file you want to work on
colnames(df) <- c("TIMESTAMP") ##Change your timeseries dataframe's column heading to TIMESTAMP to match the raw data column heading
data_with_missing_times <- left_join(df,dat, by = "TIMESTAMP") ##Join the the time series and your raw data by the TIMESTAMP column
write.csv(data_with_missing_times, "C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis/Langriver_clean.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")

colnames(dat) <- c("TIMESTAMP", "level")

#Edit your TIMESTAMP to get it in the right format yyyymmddhh (if necessary)
dat<-read.csv("Fog800.csv",skip=0)
dat$TIMESTAMP <- strptime(dat$TIMESTAMP, "%Y/%m/%d %H:%M")
dat$TIMESTAMP <- as.factor(format(dat$TIMESTAMP,'%Y%m%d%H'))
write.csv(dat, "C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Raw data/Fog800_clean.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")


##Stats about the datasets:

dwa<-read.csv("Dwarsberg.csv",skip=0)
dwa<-read.csv("Constantiaberg.csv",skip=0)
dwa<-read.csv("Fog500.csv",skip=0)
dwa<-read.csv("Fog600.csv",skip=0)
dwa<-read.csv("Fog700.csv",skip=0)
dwa<-read.csv("Fog800.csv",skip=0)

#number of NAs in a column
sum(is.na(dwa$Fog.corrected.mm))

#remove all rows where the fog column has an NA
fogna<-dwa[!is.na(dwa$Fog.corrected.mm),]

#remove rows where fog = 0 (this gives you all the rows where fog > 0)
dp<-fogna[!(fogna$Fog.corrected.mm=="0"),]

#remove rows where rain > 0 (this gives you rows with fog only)
dm<-dp[!(dp$Rain_Tot.mm>"0"),]




