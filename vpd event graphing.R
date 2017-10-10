##This script calculates VPD, identifies VPD events (for example VPD = 0) and graphs these events for all stations
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis/Temp time series")

#Read in all your data files as separate dataframes, then calculate SVP and VPD as extra columns

dwa<-read.csv("Dwarsberg.csv",skip=0)
con<-read.csv("Constantiaberg.csv",skip=0)
f5<-read.csv("fog500.csv",skip=0)
f6<-read.csv("fog600.csv",skip=0)
f7<-read.csv("fog700.csv",skip=0)
f8<-read.csv("fog800.csv",skip=0)


dwa$es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + dwa$AirTC_Avg)))
dwa$vpd <- ((100 - dwa$RH) / 100) * dwa$es
con$es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + con$AirTC_Avg)))
con$vpd <- ((100 - con$RH) / 100) * con$es
f5$es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + f5$AirTC_Avg)))
f5$vpd <- ((100 - f5$RH) / 100) * f5$es
f6$es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + f6$AirTC_Avg)))
f6$vpd <- ((100 - f6$RH) / 100) * f6$es
f7$es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + f7$AirTC_Avg)))
f7$vpd <- ((100 - f7$RH) / 100) * f7$es
f8$es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + f8$AirTC_Avg)))
f8$vpd <- ((100 - f8$RH) / 100) * f8$es

#Create a column TS with YYmm
dwa$TS <- strptime(dwa$TIMESTAMP, "%Y/%m/%d %H:%M")
dwa$TS <- as.factor(format(dwa$TS,'%Y%m%d%H'))
dwa$TS <- sapply(dwa$TS, substring, 0, 6)
con$TS <- strptime(con$TIMESTAMP, "%Y/%m/%d %H:%M")
con$TS <- as.factor(format(con$TS,'%Y%m%d%H'))
con$TS <- sapply(con$TS, substring, 0, 6)
f5$TS <- strptime(f5$TIMESTAMP, "%Y/%m/%d %H:%M")
f5$TS <- as.factor(format(f5$TS,'%Y%m%d%H'))
f5$TS <- sapply(f5$TS, substring, 0, 6)
f6$TS <- strptime(f6$TIMESTAMP, "%Y/%m/%d %H:%M")
f6$TS <- as.factor(format(f6$TS,'%Y%m%d%H'))
f6$TS <- sapply(f6$TS, substring, 0, 6)
f7$TS <- strptime(f7$TIMESTAMP, "%Y/%m/%d %H:%M")
f7$TS <- as.factor(format(f7$TS,'%Y%m%d%H'))
f7$TS <- sapply(f7$TS, substring, 0, 6)
f8$TS <- strptime(f8$TIMESTAMP, "%Y/%m/%d %H:%M")
f8$TS <- as.factor(format(f8$TS,'%Y%m%d%H'))
f8$TS <- sapply(f8$TS, substring, 0, 6)

#Convert the TIMESTAMP into an appropriate format
f5$TIMESTAMP <- strptime(f5$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
f6$TIMESTAMP <- strptime(f6$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
f7$TIMESTAMP <- strptime(f7$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
f8$TIMESTAMP <- strptime(f8$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
dwa$TIMESTAMP <- strptime(dwa$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
con$TIMESTAMP <- strptime(con$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")

#Subset the columns you want to graph (below example is for fog and VPD. If you want all VPD on the same graph, exclude fog at this point by subsetting only TIMESTAMP and vpd)
d<-subset(dwa, select = c(TIMESTAMP,Fog.corrected.mm, vpd, TS))
c<-subset(con, select = c(TIMESTAMP,Fog.corrected.mm, vpd, TS))
ff5<-subset(f5, select = c(TIMESTAMP,Fog.corrected.mm, vpd, TS))	
ff6<-subset(f6, select = c(TIMESTAMP,Fog.corrected.mm, vpd, TS))
ff7<-subset(f7, select = c(TIMESTAMP,Fog.corrected.mm, vpd, TS))
ff8<-subset(f8, select = c(TIMESTAMP,Fog.corrected.mm, vpd, TS))

#Convert the subset into a zoo object and do the event identification
#d$TIMESTAMP <- as.factor(format(d$TIMESTAMP,'%Y%m%d%H'))
#testd<-zooreg(d[,2:3], order.by=d$TIMESTAMP)
#ev0d <- eventseq(testd$vpd =="0")
#ev5d <- eventseq(testd$vpd<5)
#ev20d <- eventseq(testd$vpd>20)

d$TIMESTAMP <- as.factor(format(d$TIMESTAMP,'%Y%m%d%H'))
testd<-zooreg(d[,2:3], order.by=d$TIMESTAMP)
evfogd <- eventseq(testd$vpd =="0")

c$TIMESTAMP <- as.factor(format(c$TIMESTAMP,'%Y%m%d%H'))
testc<-zooreg(c[,2:3], order.by=c$TIMESTAMP)
evfogc <- eventseq(testc$vpd =="0")
ff5$TIMESTAMP <- as.factor(format(ff5$TIMESTAMP,'%Y%m%d%H'))
test5<-zooreg(ff5[,2:3], order.by=ff5$TIMESTAMP)
evfog5 <- eventseq(test5$vpd =="0")
ff6$TIMESTAMP <- as.factor(format(ff6$TIMESTAMP,'%Y%m%d%H'))
test6<-zooreg(ff6[,2:3], order.by=ff6$TIMESTAMP)
evfog6 <- eventseq(test6$vpd =="0")
ff7$TIMESTAMP <- as.factor(format(ff7$TIMESTAMP,'%Y%m%d%H'))
test7<-zooreg(ff7[,2:3], order.by=ff7$TIMESTAMP)
evfog7 <- eventseq(test7$vpd =="0")
ff8$TIMESTAMP <- as.factor(format(ff8$TIMESTAMP,'%Y%m%d%H'))
test8<-zooreg(ff8[,2:3], order.by=ff8$TIMESTAMP)
evfog8 <- eventseq(test8$vpd =="0")

#Identify unique values in TS column, count them, use the count as the number of x-axis ticks
m5<- unique(ff5$TS)
x.tick.number <- length(m5)
at5 <- seq(1, nrow(f5), length.out=x.tick.number)
labels5 <- c(m5, length.out=x.tick.number)

m6<- unique(ff6$TS)
x.tick.number <- length(m6)
at6 <- seq(1, nrow(f6), length.out=x.tick.number)
labels6 <- c(m6, length.out=x.tick.number)

m7<- unique(ff7$TS)
x.tick.number <- length(m7)
at7 <- seq(1, nrow(f7), length.out=x.tick.number)
labels7 <- c(m7, length.out=x.tick.number)

m8<- unique(ff8$TS)
x.tick.number <- length(m8)
at8 <- seq(1, nrow(f8), length.out=x.tick.number)
labels8 <- c(m8, length.out=x.tick.number)

mcon<- unique(con$TS)
x.tick.number <- length(mcon)
atcon <- seq(1, nrow(con), length.out=x.tick.number)
labelscon <- c(mcon, length.out=x.tick.number)

mdwa<- unique(dwa$TS)
x.tick.number <- length(mdwa)
atdwa <- seq(1, nrow(dwa), length.out=x.tick.number)
labelsdwa <- c(mdwa, length.out=x.tick.number)


##Plot the data with events highlighted in black
dvpd<- xyplot(testd[,c(2)], main ="Jonkershoek 1214", ylab = "VPD (mbar)", ylim = c(0, 50), scales=list(x=list(at=atdwa,labels=labelsdwa, rot=90))) + layer_(panel.xblocks(evfogd, col = c("grey90", "grey80"), border = "grey80")) + layer(panel.xblocks(evfogd, block.y = 0, vjust = 1, col = 1))
cvpd<- xyplot(testc[,c(2)], main ="Constantiaberg 890", ylab = "VPD (mbar)", ylim = c(0, 50), scales=list(x=list(at=atcon,labels=labelscon, rot=90))) + layer_(panel.xblocks(evfogc, col = c("grey90", "grey80"), border = "grey80")) + layer(panel.xblocks(evfogc, block.y = 0, vjust = 1, col = 1))
f5vpd<- xyplot(test5[,c(2)], main ="Jonkershoek 500", ylab = "VPD (mbar)", ylim = c(0, 50), scales=list(x=list(at=at5,labels=labels5, rot=90))) + layer_(panel.xblocks(evfog5, col = c("grey90", "grey80"), border = "grey80")) + layer(panel.xblocks(evfog5, block.y = 0, vjust = 1, col = 1))
f6vpd<- xyplot(test6[,c(2)], main ="Jonkershoek 600", ylab = "VPD (mbar)", ylim = c(0, 50), scales=list(x=list(at=at6,labels=labels6, rot=90))) + layer_(panel.xblocks(evfog6, col = c("grey90", "grey80"), border = "grey80")) + layer(panel.xblocks(evfog6, block.y = 0, vjust = 1, col = 1))
f7vpd<- xyplot(test7[,c(2)], main ="Jonkershoek 700", ylab = "VPD (mbar)", ylim = c(0, 50), scales=list(x=list(at=at7,labels=labels7, rot=90))) + layer_(panel.xblocks(evfog7, col = c("grey90", "grey80"), border = "grey80")) + layer(panel.xblocks(evfog7, block.y = 0, vjust = 1, col = 1))
f8vpd<- xyplot(test8[,c(2)], main ="Jonkershoek 800", ylab = "VPD (mbar)", ylim = c(0, 50), scales=list(x=list(at=at8,labels=labels8, rot=90))) + layer_(panel.xblocks(evfog8, col = c("grey90", "grey80"), border = "grey80")) + layer(panel.xblocks(evfog8, block.y = 0, vjust = 1, col = 1))

grid.arrange(dvpd, cvpd,f5vpd,f6vpd,f7vpd,f8vpd, ncol=2, nrow=3)
grid.arrange(dvpd,f8vpd,f7vpd,f6vpd,f5vpd,cvpd, ncol=2, nrow=3)
grid.arrange(dvpd,f8vpd,f7vpd,f6vpd,f5vpd, ncol=2, nrow=3)
grid.arrange(dvpd,f8vpd,cvpd, ncol=1, nrow=3)

dt<-table(coredata(evfogd))
eventdur<-as.data.frame(table(coredata(evfogd))) 
ggplot(eventdur, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", colour="black", position= "dodge")

d0<-xyplot(testd[,c(2)], main ="Dwarsberg", scales=list(x=list(at=atdwa,labels=labelsdwa, rot=90))) + layer_(panel.xblocks(ev0d, col = c("grey90", "grey80"), border = "grey80")) + layer(panel.xblocks(ev0d, block.y = 0, vjust = 1, col = 1))
d5<-xyplot(testd[,c(2)], main ="Dwarsberg", scales=list(x=list(at=atdwa,labels=labelsdwa, rot=90))) + layer_(panel.xblocks(ev5d, col = c("grey90", "grey80"), border = "grey80")) + layer(panel.xblocks(ev5d, block.y = 0, vjust = 1, col = 1))
d20<-xyplot(testd[,c(2)], main ="Dwarsberg", scales=list(x=list(at=atdwa,labels=labelsdwa, rot=90))) + layer_(panel.xblocks(ev20d, col = c("grey90", "grey80"), border = "grey80")) + layer(panel.xblocks(ev20d, block.y = 0, vjust = 1, col = 1))
    



#Convert to an xts object
dw <- xts(d[,3], order.by = d$TIMESTAMP)
l5 <- xts(ff5[,3], order.by = ff5$TIMESTAMP)
l6 <- xts(ff6[,3], order.by = ff6$TIMESTAMP)
l7 <- xts(ff7[,3], order.by = ff7$TIMESTAMP)

#Bind them together in the same xts object
vpd <- cbind(dw,l5,l6,l7)
colnames(vpd)<-c("dwa", "fog5", "fog6", "fog7")

#Graph
dygraph(vpd, main = "VPD - All", ylab = "VPD") %>% dyRangeSelector()

