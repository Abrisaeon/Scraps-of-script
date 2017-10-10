#Testing the difference between temps during events of different types within sites
-----------------------------------------------------------------------------------
dwa<-read.csv("Dwarsberg.csv",skip=0)
con<-read.csv("Constantiaberg.csv",skip=0)
f5<-read.csv("Langrivier fog 500.csv",skip=0)
f6<-read.csv("Langrivier fog 600.csv",skip=0)
f7<-read.csv("Langrivier fog 700.csv",skip=0)
f8<-read.csv("Langrivier fog 800.csv",skip=0)


#Prep for PRECIP
---------------------------------------------------------------------------------
dp<-dwa[!(dwa$Fog.corrected.mm=="0"),]			
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
d1<-subset(pred, select = c(AirTC_Avg))
d1$id<-"Precip"
c1<-subset(prec, select = c(AirTC_Avg))
c1$id<-"Precip"
f51<-subset(pre5, select = c(AirTC_Avg))
f51$id<-"Precip"
f61<-subset(pre6, select = c(AirTC_Avg))
f61$id<-"Precip"
f71<-subset(pre7, select = c(AirTC_Avg))
f71$id<-"Precip"
f81<-subset(pre8, select = c(AirTC_Avg))
f81$id<-"Precip"


#Prep for FOG ONLY
---------------------------------------------------------------------------------
#Select only hours where fog>0 (precip hours) and turn them into a dataframe
df<-dwa[(dwa$Fog.corrected.mm>"0"),]			##Create dataframe that excludes all fog=0 rows
cf<-con[(con$Fog.corrected.mm>"0"),]	
fo5<-f5[(f5$Fog.corrected.mm>"0"),]	
fo6<-f6[(f6$Fog.corrected.mm>"0"),]	
fo7<-f7[(f7$Fog.corrected.mm>"0"),]	
fo8<-f8[(f8$Fog.corrected.mm>"0"),]	

dm<-df[!(df$Rain_Tot.mm>"0"),]
cm<-cf[!(cf$Rain_Tot.mm>"0"),]	
m5<-fo5[!(fo5$Rain_Tot.mm>"0"),]	
m6<-fo6[!(fo6$Rain_Tot.mm>"0"),]	
m7<-fo7[!(fo7$Rain_Tot.mm>"0"),]	
m8<-fo8[!(fo8$Rain_Tot.mm>"0"),]	

#Subset precip dataframe by Air temp
fogd<-subset(dm, select = c(TIMESTAMP,AirTC_Avg))
fogc<-subset(cm, select = c(TIMESTAMP,AirTC_Avg))
fog5<-subset(m5, select = c(TIMESTAMP,AirTC_Avg))	
fog6<-subset(m6, select = c(TIMESTAMP,AirTC_Avg))
fog7<-subset(m7, select = c(TIMESTAMP,AirTC_Avg))
fog8<-subset(m8, select = c(TIMESTAMP,AirTC_Avg))

##Prepare data for ANOVA (by selecting just the AirTC_Avg column and adding a column with the station name for ID)
d2<-subset(fogd, select = c(AirTC_Avg))
d2$id<-"Fog only"
c2<-subset(fogc, select = c(AirTC_Avg))
c2$id<-"Fog only"
f52<-subset(fog5, select = c(AirTC_Avg))
f52$id<-"Fog only"
f62<-subset(fog6, select = c(AirTC_Avg))
f62$id<-"Fog only"
f72<-subset(fog7, select = c(AirTC_Avg))
f72$id<-"Fog only"
f82<-subset(fog8, select = c(AirTC_Avg))
f82$id<-"Fog only"


#Prep for DRY
---------------------------------------------------------------------------------
#Select only hours where fog=0 and rain=0 (dryhours) and turn them into a dataframe
dd<-dwa[!(dwa$Fog.corrected.mm>"0"),]			##Create dataframe that excludes all fog=0 rows
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
drd<-subset(ddry, select = c(TIMESTAMP,AirTC_Avg))
drc<-subset(cdry, select = c(TIMESTAMP,AirTC_Avg))
dr5<-subset(dry5, select = c(TIMESTAMP,AirTC_Avg))	
dr6<-subset(dry6, select = c(TIMESTAMP,AirTC_Avg))
dr7<-subset(dry7, select = c(TIMESTAMP,AirTC_Avg))
dr8<-subset(dry8, select = c(TIMESTAMP,AirTC_Avg))

##Prepare data for ANOVA (by selecting just the AirTC_Avg column and adding a column with the station name for ID)
d3<-subset(drd, select = c(AirTC_Avg))
d3$id<-"Dry"
c3<-subset(drc, select = c(AirTC_Avg))
c3$id<-"Dry"
f53<-subset(dr5, select = c(AirTC_Avg))
f53$id<-"Dry"
f63<-subset(dr6, select = c(AirTC_Avg))
f63$id<-"Dry"
f73<-subset(dr7, select = c(AirTC_Avg))
f73$id<-"Dry"
f83<-subset(dr8, select = c(AirTC_Avg))
f83$id<-"Dry"

#Bind all the stations together in one dataframe and omit NAs
dwarsberg <- rbind(d1, d2, d3)
td<-na.omit(dwarsberg)
constantiaberg <- rbind(c1, c2, c3)
tc<-na.omit(constantiaberg)
Lang500 <- rbind(f51, f52, f53)
t5<-na.omit(Lang500)
Lang600 <- rbind(f61, f62, f63)
t6<-na.omit(Lang600)
Lang700 <- rbind(f71, f72, f73)
t7<-na.omit(Lang700)
Lang800 <- rbind(f81, f82, f83)
t8<-na.omit(Lang800)

ggplot(t7, aes(x = id, y = AirTC_Avg)) + geom_boxplot(fill = "grey", colour = "black") +   scale_x_discrete() + xlab("Site") +   ylab("AirTC_Avg")

aov1<-aov(AirTC_Avg ~ id, data=t7)
summary(aov1)
model.tables(aov1,"means")  

TukeyHSD(aov1)

a=HSD.test(aov1, "id", group=TRUE)
head(a)

graph_summary<-ddply(t7, c("id"), summarize, AVERAGE=mean(AirTC_Avg), SE=sqrt(var(AirTC_Avg)/length(AirTC_Avg)))

ggplot(graph_summary)+ aes(x=id, y=AVERAGE, colour=id)+ geom_point()+  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+  scale_x_discrete("Site") 
