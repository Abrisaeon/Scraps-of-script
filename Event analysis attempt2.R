setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses")
dat<-read.csv("Constantiaberg_ready.csv",skip=0) ##edit your .csv file to only include one row of column headers before reading it in

#sapply(dat, class) # check out the class of data in each column

dat$Hour.ID=1:12934
datetime=matrix(unlist(strsplit(as.character(dat$TIMESTAMP), split=" ")), length(dat$TIMESTAMP), 2, byrow=T)
dat=cbind(datetime, dat)
colnames(dat)[1:2]=c("Date", "Time")
dat$Date=as.Date(dat$Date)


#basic line graph
plot(dat$Fog.corrected.mm, type="o", col="blue", ylim=c(0,35),ylab="Fog (mm)")

install.packages("RGtk2")
install.packages(c("zoo", "latticeExtra", "polynom", "car", "Hmisc","reshape"))
#install.packages("sp")
install.packages("hydromad")
install.packages("hydromad", repos="http://hydromad.catchment.org")
install.packages("C:\\Users\\ABRI\\Documents\\R\\R-3.1.0\\library\\hydromad_0.9-26.tar.gz", repos = NULL, type="source")
install.packages("chron")
install.packages("playwith")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("devtools")
install.packages("gridExtra") ##this is to create multiple plots in the same window
install.packages("arules")
install.packages("lattice")
install.packages("plotrix")
install.packages("reshape2")
install.packages("lubridate")
install.packages("RColorBrewer")
install.packages("climdex.pcic")
install.packages("seas")
install.packages("xts")
install.packages("agricolae")
install.packages("dygraphs")

install_github('metvurst', 'tim-salabim')

 

install_github("easyGgplot2", "kassambara")

#Identify and remove duplicate rows
duplicated(dat) ##Identify
dat[duplicated(dat), ] ##View duplicates
dat[!duplicated(dat), ] ##View dataframe without duplicates
duplicateless<- dat[!duplicated(dat), ] ##Create a dataframe of the "duplicateless" data

#Find gaps and fill them with NA
ts <- seq.POSIXt(as.POSIXct("2013/10/28 12:00",'%m/%d/%y %H:%M'), as.POSIXct("2016/01/01 00:00",'%m/%d/%y %H:%M'), by="hour")  ##Insert the start and end TIMESTAMPS of your raw data. This creates a TIMESTAMP column without any missing hours
ts <- format.POSIXct(ts,'%Y/%m/%d %H:%M')  ##Ensure the TIMESTAMP format in the ts is the same as in your raw data
df <- data.frame(timestamp=ts) ##Change your ts into a dataframe before you can join it to your raw data
dat<-read.csv("Constantiaberg event based - cleanedtest.csv",skip=0) ##Read in whatever raw data file you want to work on
colnames(df) <- c("TIMESTAMP") ##Change your timeseries dataframe's column heading to TIMESTAMP to match the raw data column heading
data_with_missing_times <- left_join(df,dat, by = "TIMESTAMP") ##Join the the time series and your raw data by the TIMESTAMP column
write.csv(data_with_missing_times, "H://SCIENCE/SAEON_ABSTRACTS//Data Management//Projects//Mountain catchment streamflow monitoring//Data//Constantiaberg cloud precipitation//Analyses & summaries//Constantiaberg_ready.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")

tsamdwa <- xts(Dwadat$Fog.corrected.mm, as.Date(Dwadat$TIMESTAMP, "%Y/%m/%d %H:%M"))
tsam_dwa = apply.daily(tsamdwa, sum)
TSDamdwa<-data.frame(date=index(tsam_dwa), coredata(tsam_dwa)) 
colnames(TSDamdwa) <- c("date", "amount")
ggplot(TSDamdwa, aes(x=date, y=amount)) + geom_bar(stat="identity", colour="black") + ylim(c(0, 150))



#Prepare data as a zoo object for time series analysis
#test<-read.zoo(dat, FUN=as.chron)
test<-zooreg(dat[,5:17], order.by=dat$Hour.ID) #Date
test<-zooreg(dat[,2:15], order.by=dat$TIMESTAMP) #Make sure the raw data are clean with no duplicate rows before running this

# Do the time series analysis/event identification
evfog <- eventseq(test$Fog.corrected.mm, thresh=0.1, mindur=4, inthresh = 0.09, indur = 4, continue = TRUE)
evrain <- eventseq(test$Rain_Tot.mm, thresh=0.253, below = FALSE)
#head(events)
str(evfog)
nlevels(evfog)
table(coredata(evfog))#to view the number of hours per event as well as the start time

#basic analyses of events
# to calculate the number of millimeters of fog per event
eventapply(test$Fog.corrected.mm, evfog, FUN = mean)# to calculate the average hourly deposition of fog per event
eventapply(test$Fog.corrected.mm, evfog, FUN = range)
table(test$Fog.corrected.mm)# to calculate the number of samples for each deposition value
summary(evfog)
test[test$Fog.corrected.mm>10, ]# select data according to certain thresholds ie. in this case fog>10mm
bigrain <- (test[test$Rain_Tot.mm>10, ]) # to make a table of records that meet certain thresholds ie. table of rain events >10mm
test[test$Rain_Tot.mm>10 & test$Fog.corrected.mm>20, ] # to select data according to thresholds in multiple columns

# to identify fog only events and then sum the fog per "event"
evfogonly <- eventseq(test$Fog.corrected.mm > 0.03 & test$Rain_Tot.mm<=0) # to identify fog only events. This gives the start timestamp and number of samples in the event
eventapply(test$Fog.corrected.mm, evfogonly, FUN = sum) #sum per "event"

## To do the fancy Hydromad events plot
#xyplot(test[,c(5,6,11)]) + layer_(panel.xblocks(events, col = c("grey90", "grey80"), border = "grey80"))  + layer(panel.xblocks(events, block.y = 0, vjust = 1, col = 1))
xyplot(test[,c(10)]) + layer_(panel.xblocks(evfog, col = c("grey90", "grey80"), border = "grey80"))  + layer(panel.xblocks(evfog, block.y = 0, vjust = 1, col = 1))



######################ALL EVENTS############################################

#to turn the "coredata table of evfog" into a table that can be exported or plotted
evtable <- table(coredata(evfog))
as.data.frame(evtable) #to turn the table into two columns with column headers
plotevtable <- (as.data.frame(evtable)) # to turn the dataframe into a dataset that can be plotted
# to plot a frequency distribution of event duration with an x scale showing all values, with assorted axes labels and title
ggplot(plotseastab, aes(Freq)) + geom_histogram(binwidth = 5)

#####################FOGONLY#############################################


fogonlytable <- eventapply(test$Fog.corrected.mm, evfogonly, FUN = sum)

#to write out a csv table
write.csv(fogonlytable, "H://SCIENCE/SAEON_ABSTRACTS//Data Management//Projects//Mountain catchment streamflow monitoring//Data//Constantiaberg cloud precipitation//Analyses & summaries//fogonly.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")
write.csv(foghours, "H://SCIENCE/SAEON_ABSTRACTS//Data Management//Projects//Mountain catchment streamflow monitoring//Data//Constantiaberg cloud precipitation//Analyses & summaries//fogonly.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")

#########
##Plotting

xyplot(x) + layer_(panel.xblocks(evp, col = c("grey90", "grey80"), border = "grey80")) + 
  layer(panel.xblocks(evq, block.y = 0, vjust = 1, col = 1))

#plot the time series
xyplot(test$Fog.corrected.mm, ylim=c(0,40))
xyplot(test$RH, ylim=c(0,120))
xyplot(test$Rain_Tot.mm, ylim=c(0,30))

#or using qplot
qplot(Var1, Freq, data=evraintab, geom = "bar", stat = "identity", colour = I("red"))

#to change the column names in a dataframe
colnames(evfogtab) <- c("Timestamp", "Duration")

#to plot a histogram of all fog event durations
evfogtab<-as.data.frame(table(coredata(evfog)))
qplot(Duration, data=evfogtab, geom = "histogram", stat = "bin", colour = I("orange"), binwidth = 1)

##########################SELECTING SPECIFIC TIME PERIODS###################################################

#this asks eventseq to run just on a specific time subset
evfog <- eventseq(seasontest[seasontest$Month==11]$Fog.corrected.mm, thresh=0.1) 

#OR

evfog <- eventseq(seasontest$Fog.corrected.mm, thresh=0.1) #and then....

# this uses substring to select specific digits in a value as identification (BEWARE THIS SELECTS MONTHS IRRESPECTIVE OF YEAR)
novtab=table(coredata(evfog)) #must be a table first, not a zoo series
novtab[which(substr(names(novtab),5,6)=="11")] 


#######################MONTHLY#############################################################################
##EVENT DURATION PER MONTH##

test<-zooreg(dat[,5:17], order.by=dat$TIMESTAMP)
evfog <- eventseq(test$Fog.corrected.mm, thresh=0.03)
nlevels(evfog)
monthtab=table(coredata(evfog))
jantab <- monthtab[which(substr(names(monthtab),5,6)=="01")]# swap "01" for any month
febtab <- monthtab[which(substr(names(monthtab),5,6)=="02")]
martab <- monthtab[which(substr(names(monthtab),5,6)=="03")]
aprtab <- monthtab[which(substr(names(monthtab),5,6)=="04")]
maytab <- monthtab[which(substr(names(monthtab),5,6)=="05")]
juntab <- monthtab[which(substr(names(monthtab),5,6)=="06")]
jultab <- monthtab[which(substr(names(monthtab),5,6)=="07")]
augtab <- monthtab[which(substr(names(monthtab),5,6)=="08")]
septab <- monthtab[which(substr(names(monthtab),5,6)=="09")]
octtab <- monthtab[which(substr(names(monthtab),5,6)=="10")]
novtab <- monthtab[which(substr(names(monthtab),5,6)=="11")]
dectab <- monthtab[which(substr(names(monthtab),5,6)=="12")]

plotjantab <- (as.data.frame(jantab))
plotfebtab <- (as.data.frame(febtab))
plotmartab <- (as.data.frame(martab))
plotaprtab <- (as.data.frame(aprtab))
plotdectab <- (as.data.frame(dectab))
plotdectab <- (as.data.frame(dectab))
plotdectab <- (as.data.frame(dectab))
plotdectab <- (as.data.frame(dectab))

head(plotdectab)##to identify the column header

##to plot the data
ggplot(plotjantab, aes(jantab)) + geom_histogram(binwidth = 1) + scale_x_discrete() ##to plot the frequency distribution
ggplot(plotjantab, aes(jantab)) + geom_histogram(binwidth = 1) + scale_x_discrete(name ="eventdur") + scale_y_discrete(name ="count") ##set axes "names"

##to remove gridlines
+ theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
##to set axes limits
+ expand_limits(y=c(0,90))

#######################MONTHLY#############################################################################
##EVENT MAGNITUDE IN mm PER MONTH##

setwd("H://SCIENCE/SAEON_ABSTRACTS//Data Management//Projects//Mountain catchment streamflow monitoring//Data//Constantiaberg cloud precipitation//Analyses & summaries")
dat<-read.csv("Constantiaberg event based errorsremoved.csv",skip=0) 		#Ensure errors are converted to NA before reading in
test<-zooreg(dat[,2:15], order.by=dat$TIMESTAMP)
evfog <- eventseq(test$Fog.corrected.mm, thresh=0.03)				#Any fog above 0.03mm (in other words any fog) 
evrain <- eventseq(test$Rain_Tot.mm, thresh=0.253, below = FALSE)		#Any rain above 0.253mm (in other words any rain) 
evfogonly <- eventseq(test$Fog.corrected.mm > 0.03 & test$Rain_Tot.mm<0.254)	#Any fog above 0.03mm which occurs when rain < 0.254 (in other fog when there's no rain)
evmm<-eventapply(test$Fog.corrected.mm, evfogonly, FUN = sum)			#Sum the mm by event 
rainev<-eventapply(test$Rain_Tot.mm, evrain, FUN = sum)				#Sum the mm by event
x=as.data.frame(evmm)								#Convert to dataframe 
y=as.data.frame(rainev) 
fmtot<-aggregate(x, by=list(substr(rownames(x),1,6)), FUN="sum")		#Sum events mm by month 
rmtot<-aggregate(y, by=list(substr(rownames(y),1,6)), FUN="sum")
allmtot<-cbind(fmtot,rmtot)							#Put the two dataframes together
mplot<-allmtot[ c(1,2,4) ]							#Exclude duplicate columns  

##SUM OF ALL FOG PRECIP IN MM PER MONTH##
#substring by rowname digits 1-6 and sum - this gives you the sum of all events in mm by month
aggregate(x, by=list(substr(rownames(x),1,6)), FUN="sum") 

#to calculate monthly sums of events
monthsum<-aggregate(x, by=list(substr(rownames(x),1,6)), FUN="sum")
qplot(data=monthsum, x = Group.1, y = evmm, geom="bar", stat = "identity" ) #to plot a bar graph of monthly totals

months=unique(substr(rownames(x),1,6)) #this identifies the months based on digit 1-6 in the first row of object x

pdf("monthlyevents.pdf", width=8, height=20) # this creates a plotting window in a pdf
par(mfrow=c(7,2)) # this sets up the plotting matrix, 7 rows by 2 columns
for(i in 1:length(months))
{
  barplot(x[which(substr(rownames(x),1,6)%in%months[i]),1],main=months[i])
}
dev.off() #remember to run dev.off at the end of plotting to a pdf or somewhere outside of the r plotting window

#to calculate daily sums of events
daysum<-aggregate(x, by=list(substr(rownames(x),1,8)), FUN="sum")
qplot(data=daysum, x = Group.1, y = evmm, geom="bar", stat = "identity" )
 


ifelse(dat$Fog.corrected.mm > 0.03, TRUE, FALSE)

evplusnon <- eventseq(test$Fog.corrected.mm, thresh=0.03, all = TRUE) ##this identifies starts and ends of events and periods of no precip
eventtable <- eventapply(test$Fog.corrected.mm, evplusnon, FUN = sum) ##this sums the precip in fog events and dry events

fevs = as.vector(evplusnon)

#hmm = eventapply(test[,which(names(test)%in%c("Fog.corrected.mm", "Rain_Tot.mm"))], evfogonly, FUN = sum)


###Getting summary stats using eventapply()

#To calculate total rainfall in events where rainfall is over a certain threshold
total_rain_only = eventapply(test[,which(names(test)%in%c("Rain_Tot.mm"))], dat$Rain_Tot.mm>0, FUN = sum)

#To calculate total fogonly (sum of fog when rain=0)
total_fog_only = eventapply(test[,which(names(test)%in%c("Fog.corrected.mm"))], dat$Rain_Tot.mm==0, FUN = sum)

#To produce a list of fog only events with their precip in mm            
fog_only = eventapply(test[,which(names(test)%in%c("Fog.corrected.mm"))], evfogonly, FUN = sum)

###Getting summary stats using aggregate()

#To create a dataframe of precip and non precip events and the total precip in mm for each
fevs_sums = aggregate(dat[,which(colnames(dat)%in%c("Fog.corrected.mm", "Rain_Tot.mm"))], by = list(fevs), FUN="sum")

#To create a dataframe of rain event duration for each precip or non precip event
fevs_sum_rain_hours = aggregate(dat$Rain_Tot.mm>0, by = list(fevs), FUN="sum")

#To calculate the number of hours with no rain during dry and within fog events ie. "fogonly"
fevs_sum_norain_hours = aggregate(dat$Rain_Tot.mm==0, by = list(fevs), FUN="sum")

#Mean hourly rainfall within rain events
rain_hourly_mean = aggregate(dat[,which(colnames(dat)%in% "Rain_Tot.mm")], by = list(fevs), FUN="mean")

#Sum of fogonly in mm by "event"
fevs_only_sum = aggregate(dat[which(dat$Rain_Tot.mm==0), which(colnames(dat)%in% "Fog.corrected.mm")], by = list(fevs[dat$Rain_Tot.mm==0]), FUN="sum")

###Similar to aggregate if you don't want "Group.1" as your first column
#fevs_only_sum = tapply(dat[which(dat$Rain_Tot.mm==0), which(colnames(dat)%in% "Fog.corrected.mm")], INDEX = list(fevs[dat$Rain_Tot.mm==0]), FUN="sum")


###Merge outputs by common column names
hmm = merge(fevs_sums, fevs_only_sum)


+

allmm <- cbind(index(evmm), coredata(evmm))

sumtable <- as.data.frame(eventapply(test$Fog.corrected.mm, evfog, FUN = sum))
colnames(sumtable) = ("PRECIP")


monthmm=table(eventmm)

jantab <- monthtab[which(substr(names(monthtab),5,6)=="01")]


**********************************************************************
#Rubbish
*********  

# to take month data from a timestamp and create a month column in a dataframe
mydata$month <- as.numeric( substring( mydata$dayMonthYear, first = 4, last = 5) )  
  
#to check the maX value
max_y <- max(Fog.corrected.mm)

  # convert factor to numeric for convenience 
  Fog.corrected.mm <- as.numeric(TIMESTAMP)
TIMESTAMP <- as.numeric(Fog.corrected.mm)


nfog <- max(Fog.corrected.mm)
# get the range for the x and y axis 

xrange <- range(TIMESTAMP) 
yrange <- range(Fog.corrected.mm) 


# set up the plot 
plot(xrange, yrange, type="n", xlab="Hour since start",
     ylab="Fog precip (mm)" ) 
colors <- rainbow(dat) 
linetype <- c(1:nfog) 
plotchar <- seq(18,18+nfog,1)

# add lines 
for (i in 1:nfog) { 
  tree <- subset(Orange, Tree==i) 
  lines(tree$age, tree$circumference, type="b", lwd=1.5,
        lty=linetype[i], col=colors[i], pch=plotchar[i]) 
} 

# add a title and subtitle 
title("Tree Growth", "example of line plot")

# add a legend 
legend(xrange[1], yrange[2], 1:ntrees, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Tree")

?sapply
sapply(dat, class)

#sites<-read.xls("locations.xlsx", sheet=1, stringsAsFactors=F) #Note that TRUE and FALSE can be abbreviated to T and F
dat<-read.csv("Constantiaberg event based.csv", nrows=10222, stringsAsFactors=F) #Use this one if read.xls gives an error
sapply(dat, class)

###Splitting values using strsplit
?strsplit #"string split"

strsplit(dat[,1]," ") #returns a "list" of vectors with 2 values each

unlist(strsplit(dat[,1]," ")) #unlist() returns all values in one long vector

TIMESTAMP<-matrix(unlist(strsplit(dat[,1]," ")), ncol = 2, byrow=T) #matrix() can be used to return the data in a matrix with the specified number of columns or rows

colnames(TIMESTAMP)<-c("Date", "Time")


*********************************************************************************************************
check.for.updates.R() # tells you if there is a new version of R or not.
install.R() # download and run the latest R installer
copy.packages.between.libraries() # copy your packages to the newest R installation from the one version before it (if ask=T, it will ask you between which two versions to perform the copying)
