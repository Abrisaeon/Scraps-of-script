setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis")

dwa<-read.csv("Fog800.csv",skip=0)
d<-subset(dwa, select = c(TIMESTAMP,Fog.corrected.mm, Rain_Tot.mm))
d$date <- strptime(dwa$TIMESTAMP, "%Y/%m/%d %H:%M")
d$date  <- as.factor(format(d$date,'%Y%m%d'))

rainday<-group_by(d,day=as.Date(d$TIMESTAMP)) %>% summarise(total=sum(Rain_Tot.mm))			#Calculate the total rainfall in mm per day
fogday<-group_by(d,day=as.Date(d$TIMESTAMP)) %>% summarise(total=sum(Fog.corrected.mm))		#Calculate the total mixed precip in mm per day
dailyprec=merge(x = fogday, y = rainday, by = "day", all=TRUE)						              #Merge the two daily total dataframes
colnames(dailyprec) <- c("date", "mixed", "rain")							#Rename the headings

testd<-zooreg(dailyprec[,2:3], order.by=dailyprec$date)

evddr <- eventseq(testd$rain =="0")
evddf <- eventseq(testd$mixed =="0")

dwaddr<-as.data.frame(table(coredata(evddr)))
dwaddf<-as.data.frame(table(coredata(evddf)))


#drycount$yearmon<- as.Date(format(drycount$Group.1,'%Y%m'))
dwaddf$months<-substr(dwaddf$Var1,6,7)
dwaddr$months<-substr(dwaddr$Var1,6,7)

f<-dwaddf
colnames(f) <- c("date", "fogdry", "month")	
f$month <- as.numeric(f$month)
#Create a column called season and classify records into seasons based on month
f$season <- ifelse((f$month > 4)&(f$month < 11),"winter","summer")

fs<-subset(f, select = c(fogdry))
fs$id<-"J1214fog"
colnames(fs) <- c("cdd", "id")	

r<-dwaddr
colnames(r) <- c("date", "raindry", "month")	
r$month <- as.numeric(r$month)
#Create a column called season and classify records into seasons based on month
r$season <- ifelse((r$month > 4)&(r$month < 11),"winter","summer")

rs<-subset(r, select = c(raindry))
rs$id<-"J1214rain"
colnames(rs) <- c("cdd", "id")	

total <- rbind(fs, rs)


t<-na.omit(total)

ggplot(t, aes(x = id, y = cdd)) + geom_boxplot(fill = "grey80", colour = "black") +  scale_x_discrete() + xlab("") +   ylab("Mean CDD") + theme_bw() + ylim(c(0, 20))

aov1<-aov(cdd ~ id, data=total)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

a=HSD.test(aov1, "id", group=TRUE)
head(a)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#By season

fs<-subset(f, select = c(fogdry, season))
fs$id<-"J1214fog"
colnames(fs) <- c("cdd", "season", "id")	


rs<-subset(r, select = c(raindry, season))
rs$id<-"J1214rain"
colnames(rs) <- c("cdd","season", "id")	

total <- rbind(fs, rs)

t<-na.omit(total)

ggplot(t, aes(x = id, y = cdd)) + geom_boxplot(aes(fill=season), colour = "black") +  scale_x_discrete() + xlab("") + scale_fill_grey(start = 0.5, end = .9) +ylab("Mean CDD") + theme_bw() + ylim(c(0, 20))



aov1<-aov(cdd ~ id*season, data=total)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

a=HSD.test(aov1, "season", group=TRUE)
head(a)







t.test(f$fogdry, r$raindry)

rmcount <- aggregate(r[c("raindry")], by=list(r$month), FUN=mean, na.rm=TRUE)		#Count days with rain as identified above
fmcount <- aggregate(f[c("fogdry")], by=list(f$month), FUN=mean, na.rm=TRUE)		#Count days with mixed as identified above
drycount=merge(x = fmcount, y = rmcount, by = "Group.1", all=TRUE)					#Merge the count data by month


drycount=merge(x = dwarain, y = dwafog, by = "Var1", all=TRUE)					#Merge the count data by month

d<-drycount
colnames(d) <- c("date", "fog", "rain", "month")	
d$month <- as.numeric(d$month)
#Create a column called season and classify records into seasons based on month
d$season <- ifelse((d$month > 4)&(d$month < 11),"winter","summer")

ds<-subset(d, select = c(season, fog, rain))
df <- melt(ds, id.var = "season")

dwa<- ggplot(r, aes(x=season, y=raindry)) + geom_boxplot()+ ggtitle("Jonkershoek 1214") + theme_bw() + ylim(c(0, 20)) + ylab("Mean seasonal CDD") + xlab("") + scale_fill_grey(start = 0, end = .9) + theme(legend.position = "none") + theme(axis.text=element_text(size=12))

dwa<- ggplot(r, aes(x=season, y=raindry, fill=variable)) + geom_boxplot()+ ggtitle("Jonkershoek 1214") + theme_bw() + ylim(c(0, 31)) + ylab("Mean seasonal CDD") + xlab("") + scale_fill_grey(start = 0, end = .9) + theme(legend.position = "none") + theme(axis.text=element_text(size=12))


df <- melt(drycount ,  id.vars = 'Group.1', variable.name = 'series')	

con<-read.csv("Constantiaberg.csv",skip=0)
f5<-read.csv("Langrivier fog 500.csv",skip=0)
f6<-read.csv("Langrivier fog 600.csv",skip=0)
f7<-read.csv("Langrivier fog 700.csv",skip=0)
f8<-read.csv("Langrivier fog 800.csv",skip=0)


df <- melt(drycount ,  id.vars = 'Group.1', variable.name = 'series')					#Melt the dataframe into the long form in order to plot it
ggplot(df, aes(x=Group.1, y=value)) + geom_bar(aes(fill=variable), stat="identity", colour="black", position=position_dodge()) + xlab("Month") + ylab("Dry day count") + ggtitle("Constantiaberg dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


