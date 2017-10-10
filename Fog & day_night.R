
slr<-read.csv("slrd.csv",skip=0)
slrd<-na.omit(slr)
slrd<-subset(slr, select = c(Date,DataValue)
write.csv(slrd, "C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/slrd.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")

#Identify and remove duplicate rows
slr[duplicated(slr), ] ##View duplicates


#Find gaps and fill them with NA
ts <- seq.POSIXt(as.POSIXct("2013/03/23 12:00",'%m/%d/%y %H:%M'), as.POSIXct("2016/10/01 00:00",'%m/%d/%y %H:%M'), by="hour")  ##Insert the start and end TIMESTAMPS of your raw data. This creates a TIMESTAMP column without any missing hours
ts <- format.POSIXct(ts,'%Y/%m/%d %H:%M')  ##Ensure the TIMESTAMP format in the ts is the same as in your raw data
df <- data.frame(timestamp=ts) ##Change your ts into a dataframe before you can join it to your raw data
dat<-read.csv("slrd.csv",skip=0) ##Read in whatever raw data file you want to work on
colnames(df) <- c("TIMESTAMP") ##Change your timeseries dataframe's column heading to TIMESTAMP to match the raw data column heading
colnames(dat) <- c("TIMESTAMP", "solrad")
data_with_missing_times <- left_join(df,dat, by = "TIMESTAMP") ##Join the the time series and your raw data by the TIMESTAMP column
write.csv(data_with_missing_times, "C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/slrd_clean.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")

#Do the following one by one
#Read in the solar radiation data file as well as the fog data file
slr<-read.csv("slrd_clean.csv",skip=0)
dwa<-read.csv("Dwarsberg.csv",skip=0)            #Dwarsberg
dwa<-read.csv("Constantiaberg.csv",skip=0)       #Constantiaberg
dwa<-read.csv("Langrivier fog 500.csv",skip=0)
dwa<-read.csv("Langrivier fog 600.csv",skip=0)
dwa<-read.csv("Langrivier fog 700.csv",skip=0)
dwa<-read.csv("Langrivier fog 800.csv",skip=0)


#Merge the two files by TIMESTAMP and subset the columns needed into a new dataframe
db=merge(x = dwa, y = slr, by = "TIMESTAMP", all=TRUE)
dwarsb<-subset(db, select = c(TIMESTAMP,Rain_Tot.mm, Fog.corrected.mm, solrad))

#Identify hours that contain precipitation (identify where fog = 0 and exclude those rows) and from this hours that contain fog only (rain = 0)
Dwaprec<-dwarsb[!(dwarsb$Fog.corrected.mm=="0"),]
Dwafogonly<-na.omit(Dwaprec[(Dwaprec$Rain_Tot.mm=="0"),])

#Identify night vs day hours 
night<-Dwafogonly[(Dwafogonly$solrad=="0"),]
day<-Dwafogonly[!(Dwafogonly$solrad=="0"),]

#If you want to run an ANOVA, do the following:
n<-subset(night, select = c(Fog.corrected.mm))
n$id<-"night"
d<-subset(day, select = c(Fog.corrected.mm))
d$id<-"day"
#Bind all the stations together in one dataframe and omit NAs
total <- rbind(d, n)
t<-na.omit(total)

ggplot(t, aes(x = id, y = Fog.corrected.mm)) + geom_boxplot(fill = "grey", colour = "black") +   scale_x_discrete() + xlab("Site") +   ylab("Fog.corrected.mm") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


aov1<-aov(Fog.corrected.mm~ id, data=t)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

a=HSD.test(aov1, "id", group=TRUE)
head(a)

graph_summary<-ddply(t, c("id"), summarize, AVERAGE=mean(Fog.corrected.mm), SE=sqrt(var(Fog.corrected.mm)/length(Fog.corrected.mm)))

ggplot(graph_summary)+ aes(x=id, y=AVERAGE, colour=id)+ geom_point()+  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+  scale_x_discrete("Site") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



##############Otherwise, for a Ttest########################




#calculate the amount of fog per day/night and make a dataframe of all days/nights with fog
nightfog<-group_by(night,day=as.Date(night$TIMESTAMP)) %>% summarise(total=sum(Fog.corrected.mm))
dayfog<-group_by(day,day=as.Date(day$TIMESTAMP)) %>% summarise(total=sum(Fog.corrected.mm))


ggplot(dayfog, aes(x=day, y=total)) + geom_bar(stat="identity", colour="red") + geom_bar(data=nightfog, aes(x=day, y=total), stat="identity", colour="blue", position = "stack") + ylab("Fog (mm)") + xlab("Date") + ggtitle("Dwarsberg") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(dayfog, aes(x=day, y=total)) + geom_bar(stat="identity", colour="grey") + ylab("Fog (mm)") + xlab("Date") + ggtitle("Dwarsberg day") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(nightfog, aes(x=day, y=total)) + geom_bar(stat="identity", colour="blue") + ylab("Fog (mm)") + xlab("Date") + ggtitle("Dwarsberg night") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


t.test(nightfog$total,dayfog$total)

hist(nightfog$total, breaks = 100)
hist(dayfog$total, breaks = 100)

mean(na.omit(nightfog$total))
mean(na.omit(dayfog$total))

sum(na.omit(nightfog$total))
sum(na.omit(dayfog$total))

#Change the TIMESTAMP format  to YmdH
Dwafogonly$TIMESTAMP <- strptime(Dwafogonly$TIMESTAMP, "%Y/%m/%d %H:%M")
Dwafogonly$TIMESTAMP <- as.factor(format(Dwafogonly$TIMESTAMP,'%Y%m%d%H'))

