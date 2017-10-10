##This script compares event duration across sites using Hydromad eventseq to ID events and then  ANOVA

setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses")

#For short time series to compare identical periods
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis/Short time series")


Condat<-read.csv("Constantiaberg_clean.csv",skip=0)
Dwadat<-read.csv("Dwarsberg_clean.csv",skip=0)
dat500<-read.csv("Fog500_clean.csv",skip=0)
dat600<-read.csv("Fog600_clean.csv",skip=0)
dat700<-read.csv("Fog700_clean.csv",skip=0)
dat800<-read.csv("Fog800_clean.csv",skip=0)

testc<-zooreg(Condat[,2:15], order.by=Condat$TIMESTAMP) 	## This is applied to hourly data records and requires that the Timestamp is in the format yyyymmddhh (use Excel column format to set this)
testd<-zooreg(Dwadat[,2:22], order.by=Dwadat$TIMESTAMP)		## Note the number of columns
test5<-zooreg(dat500[,2:13], order.by=dat500$TIMESTAMP)		## Make sure the raw data are clean with no duplicate rows before running this
test6<-zooreg(dat600[,2:13], order.by=dat600$TIMESTAMP)
test7<-zooreg(dat700[,2:13], order.by=dat700$TIMESTAMP)
test8<-zooreg(dat800[,2:13], order.by=dat800$TIMESTAMP)

#For all PRECIP run this
evc <- eventseq(testc$Fog.corrected.mm > 0.03)
evd <- eventseq(testd$Fog.corrected.mm > 0.03)
ev5 <- eventseq(test5$Fog.corrected.mm > 0.03)
ev6 <- eventseq(test6$Fog.corrected.mm > 0.03)
ev7 <- eventseq(test7$Fog.corrected.mm > 0.03)
ev8 <- eventseq(test8$Fog.corrected.mm > 0.03)

#For FOG ONLY run this
evc <- eventseq(testc$Fog.corrected.mm > 0.03 & testc$Rain_Tot.mm < 0.253)
evd <- eventseq(testd$Fog.corrected.mm > 0.03 & testd$Rain_Tot.mm < 0.253)
ev5 <- eventseq(test5$Fog.corrected.mm > 0.03 & test5$Rain_Tot.mm < 0.253)
ev6 <- eventseq(test6$Fog.corrected.mm > 0.03 & test6$Rain_Tot.mm < 0.253)
ev7 <- eventseq(test7$Fog.corrected.mm > 0.03 & test7$Rain_Tot.mm < 0.253)
ev8 <- eventseq(test8$Fog.corrected.mm > 0.03 & test8$Rain_Tot.mm < 0.253)

#For DRY run this
evc <- eventseq(testc$Fog.corrected.mm < 0.03 & testc$Rain_Tot.mm < 0.253)
evd <- eventseq(testd$Fog.corrected.mm < 0.03 & testd$Rain_Tot.mm < 0.253)
ev5 <- eventseq(test5$Fog.corrected.mm < 0.03 & test5$Rain_Tot.mm < 0.253)
ev6 <- eventseq(test6$Fog.corrected.mm < 0.03 & test6$Rain_Tot.mm < 0.253)
ev7 <- eventseq(test7$Fog.corrected.mm < 0.03 & test7$Rain_Tot.mm < 0.253)
ev8 <- eventseq(test8$Fog.corrected.mm < 0.03 & test8$Rain_Tot.mm < 0.253)

#Then create a dataframe from the identified events
durc<-as.data.frame(table(coredata(evc)))
durd<-as.data.frame(table(coredata(evd)))
dur5<-as.data.frame(table(coredata(ev5)))
dur6<-as.data.frame(table(coredata(ev6)))
dur7<-as.data.frame(table(coredata(ev7)))
dur8<-as.data.frame(table(coredata(ev8)))

#Plot histograms of the event duration
qplot(Freq, data=durd, geom="histogram", binwidth=0.5)
qplot(Freq, data=durc, geom="histogram", binwidth=0.5)
qplot(Freq, data=dur5, geom="histogram", binwidth=0.5)
#qplot(Freq, data=dur6, geom="histogram", binwidth=0.5)
#qplot(Freq, data=dur7, geom="histogram", binwidth=0.5)
#qplot(Freq, data=dur8, geom="histogram", binwidth=0.5)

#Subset the relevant columns
d<-subset(durd, select = c(Freq))
d$id<-"J1214"
c<-subset(durc, select = c(Freq))
c$id<-"Const"
ff5<-subset(dur5, select = c(Freq))
ff5$id<-"J500"
ff6<-subset(dur6, select = c(Freq))
ff6$id<-"J600"
ff7<-subset(dur7, select = c(Freq))
ff7$id<-"J700"
ff8<-subset(dur8, select = c(Freq))
ff8$id<-"J800"

total <- rbind(d, c,ff8, ff7, ff6, ff5)
t<-na.omit(total)

ggplot(t, aes(x = id, y = Freq)) + geom_boxplot(fill = "grey80", colour = "black") +  scale_x_discrete() + xlab("Site") +   ylab("Event duration")


aov1<-aov(Freq ~ id, data=total)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

a=HSD.test(aov1, "id", group=TRUE)
head(a)
 ###################CHECK IF THIS IS THE RIGHT WAY TO CALCULATE STANDARD DEVIATION!!!!
graph_summary<-ddply(t, c("id"), summarize, AVERAGE=mean(Freq),SD = sd(Freq), SE=sqrt(var(Freq)/length(Freq)))

dry<-ggplot(graph_summary)+ aes(x=id, y=Freq, colour=id)+ geom_boxplot(colour = "black", fill = "grey80") +
  theme_classic() +  geom_errorbar(aes(ymax=AVERAGE+SD, ymin=AVERAGE-SD), colour = "black")+  scale_x_discrete("") + ggtitle("Dry") + ylim(c(-20, 50))
pre<-ggplot(graph_summary)+ aes(x=id, y=AVERAGE, colour=id)+ geom_boxplot(colour = "black")+  geom_errorbar(aes(ymax=AVERAGE+SD, ymin=AVERAGE-SD), colour = "black")+  scale_x_discrete("")  + ggtitle("All precip") + ylim(c(-20, 50))
fog<-ggplot(graph_summary)+ aes(x=id, y=AVERAGE, colour=id)+ geom_boxplot(colour = "black")+  geom_errorbar(aes(ymax=AVERAGE+SD, ymin=AVERAGE-SD), colour = "black")+  scale_x_discrete("Site")  + ggtitle("Fog only") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(c(-20, 50)) 

grid.arrange(dry,pre,fog,ncol=1, nrow = 3)

