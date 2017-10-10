dat<-read.csv("Dwarsberg.csv",skip=0)
dat$TS<-as.POSIXct(dat$TIMESTAMP,format = "%Y/%m/%d %H:%M",  tz = "Africa/Johannesburg") 
dat$TIMESTAMP <- strptime(dat$TIMESTAMP, "%Y/%m/%d %H:%M")
dat$TIMESTAMP <- as.integer(format(dat$TIMESTAMP,'%Y%m%d%H'))
m<- with(dat, dat[hour(dat$TS) >= 00 & hour(dat$TS) < 7, ] ) #this required the lubridate library
d<- with(dat, dat[hour(dat$TS) >= 7 & hour(dat$TS) < 19, ] )
e<- with(dat, dat[hour(dat$TS) >= 19 & hour(dat$TS) <= 23, ] )

ma<- with(m, m[m$Fog.corrected.mm >0.03 & m$Rain_Tot.mm < 0.253, ])
da<- with(d, d[d$Fog.corrected.mm >0.03 & d$Rain_Tot.mm < 0.253, ])
ea<- with(e, e[e$Fog.corrected.mm >0.03 & e$Rain_Tot.mm < 0.253, ])
na<-with(n, n[n$Fog.corrected.mm >0.03 & n$Rain_Tot.mm < 0.253, ])

n<-rbind(e,m,)


c<- with(dat, dat[hour(dat$TS) >= 7 & hour(dat$TS) < 19, ] ) #this required the lubridate library
n<- with(c, c[c$Fog.corrected.mm >0.03 & c$Rain_Tot.mm < 0.253, ])


b$TSA<-as.POSIXct(b$TS,format = "%Y/%m/%d %H:%M",  tz = "Africa/Johannesburg") 
a<- with(dat , dat[dat$TS ) >= 7 & hour( dat$TS ) < 19 , ] ) #this required the lubridate library
b<- with(a, a[a$Fog.corrected.mm >0.03 & a$Rain_Tot.mm < 0.253, ])
 

a$TIMESTAMP <- strptime(a$TIMESTAMP, "%Y/%m/%d %H:%M")
a$TIMESTAMP <- as.factor(format(a$TIMESTAMP,'%Y%m%d%H'))
a$X <- sapply(a$TIMESTAMP, substring, 0, 6)
test<-zooreg(a[,3:26], order.by=a$TIMESTAMP)
evvpd <- eventseq(test$vpd, thresh==0)

duplicated(dat) ##Identify
dat[duplicated(dat), ] ##View duplicates
dat[!duplicated(dat), ] ##View dataframe without duplicates
duplicateless<- dat[!duplicated(dat), ] ##Create a dataframe of the "duplicateless" data
write.csv(duplicateless,"C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Fog500duplicateless.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")


6) Deal with data gaps caused by instrument downtime by creating their TIMESTAMPS and filling their cells with NA

#Find gaps and fill them with NA
ts <- seq.POSIXt(as.POSIXct("2013/03/23 12:00",'%m/%d/%y %H:%M'), as.POSIXct("2016-10-01 00:00",'%m/%d/%y %H:%M'), by="hour")  ##Insert the start and end TIMESTAMPS of your raw data. This creates a TIMESTAMP column without any missing hours
ts <- format.POSIXct(ts,'%Y/%m/%d %H:%M')  ##Ensure the TIMESTAMP format in the ts is the same as in your raw data
df <- data.frame(timestamp=ts) ##Change your ts into a dataframe before you can join it to your raw data
dat<-read.csv("Fog500duplicateless.csv",skip=0) ##Read in whatever raw data file you want to work on
colnames(df) <- c("TIMESTAMP") ##Change your timeseries dataframe's column heading to TIMESTAMP to match the raw data column heading
data_with_missing_times <- left_join(df,dat, by = "TIMESTAMP") ##Join the the time series and your raw data by the TIMESTAMP column
write.csv(data_with_missing_times, "C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Dwarsberg.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")


