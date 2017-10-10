setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis/Temp time series")

##This script graphs the temperature records from all stations using DYGRAPHS and performs an ANOVA on them
dwa<-read.csv("Dwarsberg.csv",skip=0)
con<-read.csv("Constantiaberg.csv",skip=0)
f5<-read.csv("Fog500.csv",skip=0)
f6<-read.csv("Fog600.csv",skip=0)
f7<-read.csv("Fog700.csv",skip=0)
f8<-read.csv("Fog800.csv",skip=0)

##The following is only needed if a DYGRAPHS plot is needed
#Convert the TIMESTAMP into an appropriate format
f5$TIMESTAMP <- strptime(f5$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
f6$TIMESTAMP <- strptime(f6$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
f7$TIMESTAMP <- strptime(f7$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
f8$TIMESTAMP <- strptime(f8$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
dwa$TIMESTAMP <- strptime(dwa$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")

#Subset the columns you want to graph (below example is for AirTC_Avg . If you want all AirTC_Avg on the same graph, exclude fog at this point by subsetting only TIMESTAMP and AirTC_Avg)
d<-subset(dwa, select = c(TIMESTAMP,AirTC_Avg))
ff5<-subset(f5, select = c(TIMESTAMP,AirTC_Avg))	
ff6<-subset(f6, select = c(TIMESTAMP,AirTC_Avg))
ff7<-subset(f7, select = c(TIMESTAMP,AirTC_Avg))
ff8<-subset(f8, select = c(TIMESTAMP,AirTC_Avg))

#Convert to an xts object
dw <- xts(d[,2], order.by = d$TIMESTAMP)
l5 <- xts(ff5[,2], order.by = ff5$TIMESTAMP)
l6 <- xts(ff6[,2], order.by = ff6$TIMESTAMP)
l7 <- xts(ff7[,2], order.by = ff7$TIMESTAMP)
l8 <- xts(ff8[,2], order.by = ff8$TIMESTAMP)

#Bind them together in the same xts object
AirTC_Avg <- cbind(dw,l5,l6,l7,l8)
colnames(AirTC_Avg)<-c("dwa", "fog5", "fog6", "fog7", "fog8")

#Graph
dygraph(AirTC_Avg, main = "AirTC_Avg", ylab = "AirTC_Avg") %>% dyRangeSelector()


#Testing the difference between temps during PRECIP events across sites
---------------------------------------------------------------------------------
#Select only hours where fog>0 (precip hours) and turn them into a dataframe
dp<-dwa[!(dwa$Fog.corrected.mm=="0"),]			##Create dataframe that excludes all fog=0 rows
cp<-con[!(con$Fog.corrected.mm=="0"),]	
p5<-f5[!(f5$Fog.corrected.mm=="0"),]	
p6<-f6[!(f6$Fog.corrected.mm=="0"),]	
p7<-f7[!(f7$Fog.corrected.mm=="0"),]	
p8<-f8[!(f8$Fog.corrected.mm=="0"),]	

#Subset precip dataframe by Air temp
pred<-subset(dp, select = c(TIMESTAMP,AirTC_Avg))
prec<-subset(cp, select = c(TIMESTAMP,AirTC_Avg))
pre5<-subset(p5, select = c(TIMESTAMP,AirTC_Avg))	
pre6<-subset(p6, select = c(TIMESTAMP,AirTC_Avg))
pre7<-subset(p7, select = c(TIMESTAMP,AirTC_Avg))
pre8<-subset(p8, select = c(TIMESTAMP,AirTC_Avg))

##Prepare data for ANOVA (by selecting just the AirTC_Avg column and adding a column with the station name for ID)
d<-subset(pred, select = c(AirTC_Avg))
d$id<-"J1214"
c<-subset(prec, select = c(AirTC_Avg))
c$id<-"Const"
ff5<-subset(pre5, select = c(AirTC_Avg))
ff5$id<-"J500"
ff6<-subset(pre6, select = c(AirTC_Avg))
ff6$id<-"J600"
ff7<-subset(pre7, select = c(AirTC_Avg))
ff7$id<-"J700"
ff8<-subset(pre8, select = c(AirTC_Avg))
ff8$id<-"J800"

#Bind all the stations together in one dataframe and omit NAs
total <- rbind(d, c,ff8, ff7, ff6, ff5)
t<-na.omit(total)

pre<-ggplot(t, aes(x = id, y = AirTC_Avg)) + geom_boxplot(fill = "grey", colour = "black") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + theme_bw() + ylim(c(0, 45)) + ylab("Temperature") + ggtitle ("All precipitation") 


aov1<-aov(AirTC_Avg ~ id, data=t)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

a=HSD.test(aov1, "id", group=TRUE)
head(a)

graph_summary<-ddply(t, c("id"), summarize, AVERAGE=mean(AirTC_Avg), SE=sqrt(var(AirTC_Avg)/length(AirTC_Avg)), SD = round(sd(AirTC_Avg), 1))

ggplot(graph_summary)+ aes(x=id, y=AVERAGE, colour=id)+ geom_point()+  geom_errorbar(aes(ymax=AVERAGE+SD, ymin=AVERAGE-SD))+  scale_x_discrete("Site") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



#Testing the difference between temps during FOG ONLY events across sites
---------------------------------------------------------------------------------
#Select only hours where fog>0 (precip hours) AND rain=0 and turn them into a dataframe
dp<-dwa[!(dwa$Fog.corrected.mm=="0"),]			##Create dataframe that excludes all fog=0 rows
cp<-con[!(con$Fog.corrected.mm=="0"),]	
p5<-f5[!(f5$Fog.corrected.mm=="0"),]	
p6<-f6[!(f6$Fog.corrected.mm=="0"),]	
p7<-f7[!(f7$Fog.corrected.mm=="0"),]	
p8<-f8[!(f8$Fog.corrected.mm=="0"),]	

dm<-dp[!(dp$Rain_Tot.mm>"0"),]
cm<-cp[!(cp$Rain_Tot.mm>"0"),]	
m5<-p5[!(p5$Rain_Tot.mm>"0"),]	
m6<-p6[!(p6$Rain_Tot.mm>"0"),]	
m7<-p7[!(p7$Rain_Tot.mm>"0"),]	
m8<-p8[!(p8$Rain_Tot.mm>"0"),]	

#Subset precip dataframe by Air temp
pred<-subset(dm, select = c(TIMESTAMP,AirTC_Avg))
prec<-subset(cm, select = c(TIMESTAMP,AirTC_Avg))
pre5<-subset(m5, select = c(TIMESTAMP,AirTC_Avg))	
pre6<-subset(m6, select = c(TIMESTAMP,AirTC_Avg))
pre7<-subset(m7, select = c(TIMESTAMP,AirTC_Avg))
pre8<-subset(m8, select = c(TIMESTAMP,AirTC_Avg))

##Prepare data for ANOVA (by selecting just the AirTC_Avg column and adding a column with the station name for ID)
d<-subset(pred, select = c(AirTC_Avg))
d$id<-"J1214"
c<-subset(prec, select = c(AirTC_Avg))
c$id<-"Const"
ff8<-subset(pre8, select = c(AirTC_Avg))
ff8$id<-"J800"
ff7<-subset(pre7, select = c(AirTC_Avg))
ff7$id<-"J700"
ff6<-subset(pre6, select = c(AirTC_Avg))
ff6$id<-"J600"
ff5<-subset(pre5, select = c(AirTC_Avg))
ff5$id<-"J500"


#Bind all the stations together in one dataframe and omit NAs
total <- rbind(d,ff8, ff7, ff6, ff5, c)
t<-na.omit(total)

fog<-ggplot(t, aes(x = id, y = AirTC_Avg)) + geom_boxplot(fill = "grey", colour = "black") +   scale_x_discrete() + xlab("") + theme_bw() + ylim(c(0, 45)) + ylab("Temperature") + theme(axis.title.x=element_blank()) + ggtitle ("Fog only") 

aov1<-aov(AirTC_Avg ~ id, data=t)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

a=HSD.test(aov1, "id", group=TRUE)
head(a)

graph_summary<-ddply(t, c("id"), summarize, AVERAGE=mean(AirTC_Avg), SE=sqrt(var(AirTC_Avg)/length(AirTC_Avg)), SD = round(sd(AirTC_Avg), 1))

ggplot(graph_summary)+ aes(x=id, y=AVERAGE, colour=id)+ geom_point()+  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+  scale_x_discrete("Site") 


#Testing the difference between temps during DRY events across sites
---------------------------------------------------------------------------------
#Select only hours where fog=0 and rain=0 (dryhours) and turn them into a dataframe
dd<-dwa[!(dwa$Fog.corrected.mm>"0"),]			
cd<-con[!(con$Fog.corrected.mm>"0"),]	
d5<-f5[!(f5$Fog.corrected.mm>"0"),]	
d6<-f6[!(f6$Fog.corrected.mm>"0"),]	
d7<-f7[!(f7$Fog.corrected.mm>"0"),]	
d8<-f8[!(f8$Fog.corrected.mm>"0"),]	

ddry<-dd[!(dd$Rain_Tot.mm>"0"),]
cdry<-cd[!(cd$Rain_Tot.mm>"0"),]	
dry5<-d5[!(d5$Rain_Tot.mm>"0"),]	
dry6<-d6[!(d6$Rain_Tot.mm>"0"),]	
dry7<-d8[!(d7$Rain_Tot.mm>"0"),]	
dry8<-d8[!(d8$Rain_Tot.mm>"0"),]	

#Subset precip dataframe by Air temp
pred<-subset(ddry, select = c(TIMESTAMP,AirTC_Avg))
prec<-subset(cdry, select = c(TIMESTAMP,AirTC_Avg))
pre5<-subset(dry5, select = c(TIMESTAMP,AirTC_Avg))	
pre6<-subset(dry6, select = c(TIMESTAMP,AirTC_Avg))
pre7<-subset(dry7, select = c(TIMESTAMP,AirTC_Avg))
pre8<-subset(dry8, select = c(TIMESTAMP,AirTC_Avg))

##Prepare data for ANOVA (by selecting just the AirTC_Avg column and adding a column with the station name for ID)
d<-subset(pred, select = c(AirTC_Avg))
d$id<-"J1214"
c<-subset(prec, select = c(AirTC_Avg))
c$id<-"Const"
ff5<-subset(pre5, select = c(AirTC_Avg))
ff5$id<-"J500"
ff6<-subset(pre6, select = c(AirTC_Avg))
ff6$id<-"J600"
ff7<-subset(pre7, select = c(AirTC_Avg))
ff7$id<-"J700"
ff8<-subset(pre8, select = c(AirTC_Avg))
ff8$id<-"J800"

#Bind all the stations together in one dataframe and omit NAs
total <- rbind(d, c,ff8, ff7, ff6, ff5)
t<-na.omit(total)

dry<- ggplot(t, aes(x = id, y = AirTC_Avg)) + geom_boxplot(fill = "grey", colour = "black") +   scale_x_discrete() + xlab("Site") + theme_bw() + ylim(c(0, 45)) + ylab("Temperature") + theme(axis.title.x=element_blank()) + ggtitle ("Dry") 

aov1<-aov(AirTC_Avg ~ id, data=t)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

a=HSD.test(aov1, "id", group=TRUE)
head(a)

graph_summary<-ddply(t, c("id"), summarize, AVERAGE=mean(AirTC_Avg), SE=sqrt(var(AirTC_Avg)/length(AirTC_Avg)))

ggplot(graph_summary)+ aes(x=id, y=AVERAGE, colour=id)+ geom_point()+  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+  scale_x_discrete("Site")

-------------------------------------------------------------------------------------------------------------------------------------------------
#TESTING ALL AVERAGE TEMPERATURE

#Subset dataframe by Air temp
pred<-subset(dwa, select = c(TIMESTAMP,AirTC_Avg))
prec<-subset(con, select = c(TIMESTAMP,AirTC_Avg))
pre5<-subset(f5, select = c(TIMESTAMP,AirTC_Avg))	
pre6<-subset(f6, select = c(TIMESTAMP,AirTC_Avg))
pre7<-subset(f7, select = c(TIMESTAMP,AirTC_Avg))
pre8<-subset(f8, select = c(TIMESTAMP,AirTC_Avg))

##Prepare data for ANOVA (by selecting just the AirTC_Avg column and adding a column with the station name for ID)
d<-subset(pred, select = c(AirTC_Avg))
d$id<-"J1214"
c<-subset(prec, select = c(AirTC_Avg))
c$id<-"Const"
ff5<-subset(pre5, select = c(AirTC_Avg))
ff5$id<-"J500"
ff6<-subset(pre6, select = c(AirTC_Avg))
ff6$id<-"J600"
ff7<-subset(pre7, select = c(AirTC_Avg))
ff7$id<-"J700"
ff8<-subset(pre8, select = c(AirTC_Avg))
ff8$id<-"J800"

#Bind all the stations together in one dataframe and omit NAs
total <- rbind(d, c,ff8, ff7, ff6, ff5)
t<-na.omit(total)

all<-ggplot(t, aes(x = id, y = AirTC_Avg)) + geom_boxplot(fill = "grey", colour = "black") +   scale_x_discrete() + xlab("") + theme_bw() + ylim(c(0, 45)) + ylab("Temperature") + ggtitle ("All") 

aov1<-aov(AirTC_Avg ~ id, data=t)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

a=HSD.test(aov1, "id", group=TRUE)
head(a)

graph_summary<-ddply(t, c("id"), summarize, AVERAGE=mean(AirTC_Avg), SE=sqrt(var(AirTC_Avg)/length(AirTC_Avg)), SD = round(sd(AirTC_Avg), 1))

ggplot(graph_summary)+ aes(x=id, y=AVERAGE, colour=id)+ geom_point()+  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+  scale_x_discrete("Site")



grid.arrange(dry,pre,fog,ncol=1, nrow = 3)

