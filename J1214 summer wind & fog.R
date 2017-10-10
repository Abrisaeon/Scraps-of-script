
#Set working directory that contains the relevant data
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis")
#For short time series to compare identical periods
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Short time series")

#Read in your data file
dwa<-read.csv("Dwarsberg.csv",skip=0)
dwa<-read.csv("Constantiaberg.csv",skip=0)
dwa<-read.csv("Langrivier Fog 800.csv",skip=0)
#Edit the timestamp format
dwa$TIMESTAMP <- strptime(dwa$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
#Subset the columns that you're going to use in the analyses
d<-subset(dwa, select = c(TIMESTAMP,AirTC_Avg,WindDir_D1_WVT, Rain_Tot.mm, Fog.corrected.mm))
#Create a column containing just the month of observation for each record
d$month <- as.factor(format(d$TIMESTAMP,'%m'))
d$month <- as.numeric(d$month)
#Create a column called season and classify records into seasons based on month
d$season <- ifelse((d$month > 4)&(d$month < 11),"winter","summer")

#Create an object that contains all the summer data
summer<- d[!(d$season=="winter"),] 
d<-summer

#Skip the three lines below when generating the summer object
#Create an object that contains all the winter data
winter<- d[!(d$season=="summer"),] 
d<-winter

#Classify all records into win direction categories
d$ne <- ifelse((d$WindDir_D1_WVT > 22.51)&(d$WindDir_D1_WVT < 67.5),1,0)
d$e <- ifelse((d$WindDir_D1_WVT > 67.51)&(d$WindDir_D1_WVT < 112.5),1,0)
d$se <- ifelse((d$WindDir_D1_WVT > 112.5)&(d$WindDir_D1_WVT < 157.5),1,0)
d$s <- ifelse((d$WindDir_D1_WVT > 157.51)&(d$WindDir_D1_WVT < 202.5),1,0)
d$sw <- ifelse((d$WindDir_D1_WVT > 202.51)&(d$WindDir_D1_WVT < 247.5),1,0)
d$w <- ifelse((d$WindDir_D1_WVT > 247.51)&(d$WindDir_D1_WVT < 292.5),1,0)
d$nw <- ifelse((d$WindDir_D1_WVT > 292.51)&(d$WindDir_D1_WVT < 337.5),1,0)
d$dummy1<- ifelse((d$WindDir_D1_WVT > 337.51)&(d$WindDir_D1_WVT < 360),1,0)
d$dummy <- ifelse((d$WindDir_D1_WVT > 0)&(d$WindDir_D1_WVT < 22.5),1,0)
d$dummy1<-as.numeric(d$dummy1)
d$dummy<-as.numeric(d$dummy)
d$n<-d$dummy1+d$dummy

#Select only records which have a fog reading greater than zero
prec<- d[!(d$Fog.corrected.mm=="0"),] 
#Of those readings, select only the ones for which rainfall is less than the minimum detectable amount (i.e. no rain)
fog<-na.omit(prec[!(prec$Rain_Tot.mm>0.253),] )

#Count all the hours per direction category
secount <- aggregate(fog[c("se")], by=list(fog$month), FUN=sum, na.rm=TRUE)		#Count days with rain as identified above
scount <- aggregate(fog[c("s")], by=list(fog$month), FUN=sum, na.rm=TRUE)
swcount <- aggregate(fog[c("sw")], by=list(fog$month), FUN=sum, na.rm=TRUE)
wcount <- aggregate(fog[c("w")], by=list(fog$month), FUN=sum, na.rm=TRUE)
nwcount <- aggregate(fog[c("nw")], by=list(fog$month), FUN=sum, na.rm=TRUE)
ncount <- aggregate(fog[c("n")], by=list(fog$month), FUN=sum, na.rm=TRUE)
necount <- aggregate(fog[c("ne")], by=list(fog$month), FUN=sum, na.rm=TRUE)
ecount <- aggregate(fog[c("e")], by=list(fog$month), FUN=sum, na.rm=TRUE)

#Bind all the count data together and select relevant columns for the rest of the analysis
fogwind<-cbind(secount, scount, swcount, wcount, nwcount, ncount, necount, ecount )
fw<-fogwind[ c(1,2,4,6,8,10,12,14,16) ] 

#Put the data in the long form for plotting and set column names
df <- melt(fw ,  id.vars = 'Group.1', variable.name = 'series')					#Melt the dataframe into the long form in order to plot it
colnames(df) <- c("month", "dir", "count")	

#Create an object that contains a graph
dwaw<-ggplot(df, aes(x=dir, y=count)) + geom_bar(aes(fill=dir), stat="identity", position="stack") + scale_fill_grey(start = .3, end = .7) + coord_polar(theta="x", direction = 10, start = 1.97)  + theme_bw() + ylab("Fog hour count") + xlab("") + scale_y_continuous(limits = c(-100, 1000))+ ggtitle("Jonkershoek 1214 - winter (May-October)") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
dwas<-ggplot(df, aes(x=dir, y=count)) + geom_bar(aes(fill=dir), stat="identity", position="stack") + scale_fill_grey(start = .3, end = .7) + coord_polar(theta="x", direction = 10, start = 1.97)  + theme_bw() + ylab("Fog hour count") + xlab("") + scale_y_continuous(limits = c(-100, 1000))+ ggtitle("Jonkershoek 1214 - summer (November-April)") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

conw<-ggplot(df, aes(x=dir, y=count)) + geom_bar(aes(fill=dir), stat="identity", position="stack") + scale_fill_grey(start = .3, end = .7) + coord_polar(theta="x", direction = 10, start = 1.97)  + theme_bw() + ylab("Fog hour count") + xlab("") + scale_y_continuous(limits = c(-100, 1000))+ ggtitle("Constantiaberg 890 - winter (May-October)") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
cons<-ggplot(df, aes(x=dir, y=count)) + geom_bar(aes(fill=dir), stat="identity", position="stack") + scale_fill_grey(start = .3, end = .7) + coord_polar(theta="x", direction = 10, start = 1.97)  + theme_bw() + ylab("Fog hour count") + xlab("") + scale_y_continuous(limits = c(-100, 1000))+ ggtitle("Constantiaberg 890 - summer (November-April)") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

f8w<-ggplot(df, aes(x=dir, y=count)) + geom_bar(aes(fill=dir), stat="identity", position="stack") + scale_fill_grey(start = .3, end = .7) + coord_polar(theta="x", direction = 10, start = 1.97)  + theme_bw() + ylab("Fog hour count") + xlab("") + scale_y_continuous(limits = c(-100, 1000))+ ggtitle("Jonkershoek 800 - winter (May-October)") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1))
f8s<-ggplot(df, aes(x=dir, y=count)) + geom_bar(aes(fill=dir), stat="identity", position="stack") + scale_fill_grey(start = .3, end = .7) + coord_polar(theta="x", direction = 10, start = 1.97)  + theme_bw() + ylab("Fog hour count") + xlab("") + scale_y_continuous(limits = c(-100, 1000))+ ggtitle("Jonkershoek 800 - summer (November-April)") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1))


grid.arrange(dwas, dwaw, f8s, f8w, cons, conw, ncol=2, nrow=3)
