dwa<-read.csv("Dwarsberg.csv",skip=0)
slr<-read.csv("slrd_clean.csv",skip=0)
db=merge(x = dwa, y = slr, by = "TIMESTAMP", all=TRUE)

d<-subset(db, select = c(TIMESTAMP,AirTC_Avg,WindDir_D1_WVT, Rain_Tot.mm, Fog.corrected.mm, solrad))
d<-na.omit(d)

Dwa120<-d[!(d$WindDir_D1_WVT < 120),]
dwase<-Dwa120[!(Dwa120$WindDir_D1_WVT > 150),] 
dwase<- na.omit(dwase)
#dwa$TIMESTAMP<-as.Date(dwa$TIMESTAMP)
#dwase$TIMESTAMP<-as.Date(dwase$TIMESTAMP)

dwaseprec<-dwase[!(dwase$Fog.corrected.mm=="0"),] 
dwasefog<-na.omit(dwaseprec[!(dwaseprec$Rain_Tot.mm>0.253),] )


night<-dwasefog[(dwasefog$solrad=="0"),]
day<-dwasefog[!(dwasefog$solrad=="0"),]

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

ggplot() + geom_point(data=dwa, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "lightblue", size=0.5) + (geom_point(data=dwanwfog, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "red", size=0.5)) + (geom_point(data=dwasefog, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "green", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + ggtitle("Dwarsberg") + geom_hline(yintercept = 15)


Dwa310<-d[!(d$WindDir_D1_WVT < 310),]
dwannw<-na.omit(Dwa310[!(Dwa310$WindDir_D1_WVT > 340),] )

#dwanw$TIMESTAMP<-as.Date(dwanw$TIMESTAMP)
dwanwprec<-dwannw[!(dwannw$Fog.corrected.mm=="0"),] 
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








