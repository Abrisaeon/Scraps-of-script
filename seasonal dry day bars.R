#Set working directory that contains the relevant data
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis")
#For short time series to compare identical periods
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Short time series")
#For overlapping temp & RH time series 
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Temp time series")

##Data files need to be read in one by one and processed as below
#Read in your data file
dat<-read.csv("Dwarsberg.csv",skip=0)
dat<-read.csv("Constantiaberg.csv",skip=0)
dat<-read.csv("Fog800.csv",skip=0)
#Subset the columns that you're going to use in the analyses
dat<-subset(dat, select = c(TIMESTAMP,Rain_Tot.mm, Fog.corrected.mm))
#d$es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + d$AirTC_Avg)))
#d$vpd <- ((100 - d$RH) / 100) * d$es
#con$es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + con$AirTC_Avg)))
#con$vpd <- ((100 - con$RH) / 100) * con$es
#f8$es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + f8$AirTC_Avg)))
#f8$vpd <- ((100 - f8$RH) / 100) * f8$es

#SUM OF DRY DAYS BY MONTH FOR FOG VS RAIN:
#------------------------------------------
 

rainday<-group_by(dat,day=as.Date(dat$TIMESTAMP)) %>% summarise(total=sum(Rain_Tot.mm))			#Calculate the total rainfall in mm per day
fogday<-group_by(dat,day=as.Date(dat$TIMESTAMP)) %>% summarise(total=sum(Fog.corrected.mm))		#Calculate the total mixed precip in mm per day
dailyprec=merge(x = fogday, y = rainday, by = "day", all=TRUE)						#Merge the two daily total dataframes
colnames(dailyprec) <- c("date", "mixed", "rain")							            #Rename the headings

dailyprec$rcount <- ifelse((dailyprec$rain < 0.254),1,0) #Identify days with rain less than one tip of the gauge (in other words zero)
dailyprec$fcount <- ifelse((dailyprec$mixed < 0.04	),1,0)

dailyprec$yearmon<- as.factor(format(dailyprec$date,'%Y%m'))
#*******************************UP TO HERE SHOULD BE ADEQUATE TO CALCULATE CDD*************************************

d<-dailyprec

rmcount <- aggregate(dailyprec[c("rcount")], by=list(dailyprec$yearmon), FUN=sum, na.rm=TRUE)		#Count days with rain as identified above
fmcount <- aggregate(dailyprec[c("fcount")], by=list(dailyprec$yearmon), FUN=sum, na.rm=TRUE)		#Count days with mixed as identified above
drycount=merge(x = fmcount, y = rmcount, by = "Group.1", all=TRUE)					#Merge the count data by month

#drycount$yearmon<- as.Date(format(drycount$Group.1,'%Y%m'))
drycount$months<-substr(drycount$Group.1,5,6)

d<-drycount
colnames(d) <- c("date", "fog", "rain", "month")	
d$month <- as.numeric(d$month)
#Create a column called season and classify records into seasons based on month
d$season <- ifelse((d$month > 4)&(d$month < 11),"winter","summer")

ds<-subset(d, select = c(season, fog, rain))
colnames(ds) <- c("season", "fog+rain", "rain")
df <- melt(ds, id.var = "season")

dwa<- ggplot(df, aes(x=season, y=value, fill=variable)) + geom_boxplot()+ ggtitle("Jonkershoek 1214") + theme_bw() + ylim(c(0, 31)) + ylab("Monthly average dry days") + xlab("") + scale_fill_grey(start = 0, end = .9) + theme(legend.position = "none") + theme(axis.text=element_text(size=12))
l8f<- ggplot(df, aes(x=season, y=value, fill=variable)) + geom_boxplot()+ ggtitle("Jonkershoek 800")  + ylim(c(0, 31)) + ylab("Monthly average dry days") + xlab("") + scale_fill_grey(start = 0, end = .9) + theme(axis.text=element_text(size=12))
con<- ggplot(df, aes(x=season, y=value, fill=variable)) + geom_boxplot()+ ggtitle("Constantiaberg 890") + theme_bw()  + ylim(c(0, 31)) + ylab("") + xlab("")  + scale_fill_grey(start = 0, end = .9) + theme(axis.text=element_text(size=12))




grid.arrange(dwa,con, ncol=2, nrow=1)


#ggplot(d, aes(x=season, y=fog)) + geom_boxplot(aes(fill=variable), stat="identity", colour="black", position=position_dodge()) + scale_fill_grey(start = 0, end = .9) + xlab("Season") + ylab("Dry day count") + ggtitle("Jonkershoek 1214 dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#boxplot(fog~season, data=d, main="dry days counting fog", xlab = "season", ylab = "ave # days" )
#boxplot(rain~season, data=d, main="dry days counting rain", ylab = "ave # days" )


d<-na.omit(d)


#*******************************UP TO HERE SHOULD BE ADEQUATE TO CALCULATE CDD BY SEASON*************************************


rmc <- aggregate(d$rcount, by=list(d$month), FUN=mean, na.rm=TRUE)		#Count days with rain as identified above
fmc <- aggregate(d$fcount, by=list(d$month), FUN=mean, na.rm=TRUE)
drycount=merge(x = fmc, y = rmc, by = "Group.1", all=TRUE)					#Merge the count data by month
colnames(drycount) <- c("month", "ddf", "ddr")

rs <- aggregate(d$rcount, by=list(d$season), FUN=mean, na.rm=TRUE)		#Count days with rain as identified above
fs <- aggregate(d$fcount, by=list(d$season), FUN=mean, na.rm=TRUE)
seascount=merge(x = fs, y = rs, by = "Group.1", all=TRUE)	
colnames(seascount) <- c("season", "ddf", "ddr")


boxplot(ddr~month, data=drycount, main="dry days counting fog", xlab = "season", ylab = "proportion of days dry" )


sr <- aggregate(summer[c("rain")], by=list(summer$season), FUN=sum, na.rm=TRUE)		#Count days with rain as identified above
sf <- aggregate(summer[c("fog")], by=list(summer$season), FUN=sum, na.rm=TRUE)
fmcount <- aggregate(dailyprec[c("fcount")], by=list(dailyprec$month), FUN=sum, na.rm=TRUE)		#Count days with mixed as identified above

fr=merge(x = sf, y = sr, by = "Group.1", all=TRUE)					#Merge the count data by month

df <- melt(drycount ,  id.vars = 'Group.1', variable.name = 'series')					#Melt the dataframe into the long form in order to plot it


#Create an object that contains all the summer data
summer<- d[!(d$season=="winter"),] 


#Skip the three lines below when generating the summer object
#Create an object that contains all the winter data
winter<- d[!(d$season=="summer"),] 


#Identify days with mixed precip less than one tip of the gauge (in other words zero)

rmcount <- aggregate(dailyprec[c("rcount")], by=list(dailyprec$month), FUN=sum, na.rm=TRUE)		#Count days with rain as identified above

fmcount <- aggregate(dailyprec[c("fcount")], by=list(dailyprec$month), FUN=sum, na.rm=TRUE)		#Count days with mixed as identified above
drycount=merge(x = fmcount, y = rmcount, by = "Group.1", all=TRUE)					#Merge the count data by month

df <- melt(drycount ,  id.vars = 'Group.1', variable.name = 'series')					#Melt the dataframe into the long form in order to plot it

#

#ggplot(df, aes(x=Group.1, y=value)) + geom_bar(aes(fill=variable), stat="identity", colour="black", position=position_dodge()) + scale_fill_grey(start = 0, end = .9) + theme_bw() + annotate("text", x = c("201306","201406","201505","201506", "201507"), y = 24, label = c("*")) + xlab("Month") + ylab("Dry day count") + ggtitle("Jonkershoek 1214 dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Dwarsberg
ggplot(df, aes(x=Group.1, y=value)) + geom_bar(aes(fill=variable), stat="identity", colour="black", position=position_dodge()) + scale_fill_grey(start = 0, end = .9) + theme_bw() + annotate("text", x = c("201303","201306","201406","201505","201506", "201507"), y = 25, label = c("*")) + xlab("Month") + ylab("Dry day count") + ggtitle("Jonkershoek 1214 dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Constantiaberg
ggplot(df, aes(x=Group.1, y=value)) + geom_bar(aes(fill=variable), stat="identity", colour="black", position=position_dodge()) + scale_fill_grey(start = 0, end = .9) + theme_bw() + annotate("text", x = c("201502","201503"), y = 25, label = c("*")) + xlab("Month") + ylab("Dry day count") + ggtitle("Constantiaberg 890 dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


