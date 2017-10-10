setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses")

dwa<-read.csv("Constantiaberg.csv",skip=0)
dwa$TIMESTAMP <- strptime(dwa$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
#dwa$TIMESTAMP <- as.factor(format(dwa$TIMESTAMP,'%Y%m%d%H'))
d<-subset(dwa, select = c(TIMESTAMP,AirTC_Avg,WindDir_D1_WVT, Rain_Tot.mm, Fog.corrected.mm))
#test<-zooreg(d[,2:4], order.by=d$TIMESTAMP)
#se <- eventseq(test$WindDir_D1_WVT > 120 & test$WindDir_D1_WVT < 150, continue = TRUE)
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

prec<- d[!(d$Fog.corrected.mm=="0"),] 
fog<-na.omit(prec[!(prec$Rain_Tot.mm>0.253),] )


#rainday<-group_by(dat,day=as.Date(dat$TIMESTAMP)) %>% summarise(total=sum(Rain_Tot.mm))			#Calculate the total rainfall in mm per day
#fogday<-group_by(dat,day=as.Date(dat$TIMESTAMP)) %>% summarise(total=sum(Fog.corrected.mm))		#Calculate the total mixed precip in mm per day
#dailyprec=merge(x = fogday, y = rainday, by = "day", all=TRUE)						#Merge the two daily total dataframes
#colnames(dailyprec) <- c("date", "mixed", "rain")							#Rename the headings

#fog$month <- as.factor(format(fog$TIMESTAMP,'%Y%m'))						#Create a column for yyyymm
fog$month <- as.factor(format(fog$TIMESTAMP,'%m'))

secount <- aggregate(fog[c("se")], by=list(fog$month), FUN=sum, na.rm=TRUE)		#Count days with rain as identified above
scount <- aggregate(fog[c("s")], by=list(fog$month), FUN=sum, na.rm=TRUE)
swcount <- aggregate(fog[c("sw")], by=list(fog$month), FUN=sum, na.rm=TRUE)
wcount <- aggregate(fog[c("w")], by=list(fog$month), FUN=sum, na.rm=TRUE)
nwcount <- aggregate(fog[c("nw")], by=list(fog$month), FUN=sum, na.rm=TRUE)
ncount <- aggregate(fog[c("n")], by=list(fog$month), FUN=sum, na.rm=TRUE)
necount <- aggregate(fog[c("ne")], by=list(fog$month), FUN=sum, na.rm=TRUE)
ecount <- aggregate(fog[c("e")], by=list(fog$month), FUN=sum, na.rm=TRUE)

fogwind<-cbind(secount, scount, swcount, wcount, nwcount, ncount, necount, ecount )
fw<-fogwind[ c(1,2,4,6,8,10,12,14,16) ] 
	
df <- melt(fw ,  id.vars = 'Group.1', variable.name = 'series')					#Melt the dataframe into the long form in order to plot it
colnames(df) <- c("month", "dir", "count")	

df$month <- as.numeric(df$month)
df$season <- ifelse((df$month > 4)&(df$month < 11),"winter","summer")

ggplot(df, aes(x=month, y=count)) + geom_bar(aes(fill=dir), stat="identity", position="stack") + coord_polar(theta="x", direction = 1)    + theme_bw() + xlab("Month") + ylab("Dry day count") + ggtitle("Jonkershoek 1214 dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(df, aes(x=dir, y=count)) + geom_bar(aes(fill=season), stat="identity", position="stack") + scale_fill_grey(start = .3, end = .7) + coord_polar(theta="x", direction = 10, start = 1.97)  + theme_bw() + xlab("Direction") + ylab("Fog hour count") + scale_y_continuous(limits = c(-100, 2000))+ ggtitle("Jonkershoek 1214 dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1))

+ scale_fill_gradient2(low = "orange", high = "orange", mid = "lightblue") 

ggplot(df, aes(x=Group.1, y=value)) + geom_bar(aes(fill=variable), stat="identity", position="stack") + theme_bw() + annotate("text", x = c("201303","201306","201406","201505","201506", "201507"), y = 25, label = c("*")) + xlab("Month") + ylab("Dry day count") + ggtitle("Jonkershoek 1214 dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))










********************************************************************************************************************

dwa<-read.csv("Dwarsberg.csv",skip=0)
d<-subset(dwa, select = c(TIMESTAMP,AirTC_Avg,WindDir_D1_WVT, Rain_Tot.mm, Fog.corrected.mm))
Dwa120<-d[!(d$WindDir_D1_WVT < 120),]
dwase<-Dwa120[!(Dwa120$WindDir_D1_WVT > 150),] 

dwa$TIMESTAMP<-as.Date(dwa$TIMESTAMP)
dwase$TIMESTAMP<-as.Date(dwase$TIMESTAMP)

dwaseprec<-dwase[!(dwase$Fog.corrected.mm=="0"),] 
dwasefog<-na.omit(dwaseprec[!(dwaseprec$Rain_Tot.mm>0.253),] )

ggplot() + geom_point(data=dwa, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "lightblue", size=0.5) + (geom_point(data=dwanwfog, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "red", size=0.5)) + (geom_point(data=dwasefog, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "green", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + ggtitle("Dwarsberg") + geom_hline(yintercept = 15)


Dwa290<-d[!(d$WindDir_D1_WVT < 290),]
dwanw<-Dwa290[!(Dwa290$WindDir_D1_WVT > 345),] 

dwanw$TIMESTAMP<-as.Date(dwanw$TIMESTAMP)
dwanwprec<-dwanw[!(dwanw$Fog.corrected.mm=="0"),] 
dwanwfog<-na.omit(dwanwprec[!(dwanwprec$Rain_Tot.mm>0.253),] )


se<-subset(dwasefog, select = c(AirTC_Avg))
se$id<-"southeast"
nw<-subset(dwanwfog, select = c(AirTC_Avg))
nw$id<-"northwest"


total <- rbind(se, nw)
t<-na.omit(total)

ggplot(t, aes(x = id, y = AirTC_Avg)) + geom_boxplot(fill = "grey80", colour = "blue") +   scale_x_discrete() + xlab("Site") +   ylab("Air temp")

res = lm(AirTC_Avg ~ id, data = t)
summary(res)
anova(res)

aov1<-aov(AirTC_Avg ~ id, data=t)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

posthoc <- TukeyHSD(x=id, 'AirTC_Avg', conf.level=0.95)

res = data.frame(Fitted = fitted(res), Residuals = resid(res), Treatment = t$id)
#and then produce the plot:

ggplot(res, aes(Fitted, Residuals, colour = Treatment)) + geom_point()


ggplot(t, aes(x = id, y = AirTC_Avg)) + geom_boxplot(fill = "grey80", colour = "blue") +   scale_x_discrete() + xlab("Site") +   ylab("AirTC_Avg")








