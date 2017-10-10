setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis/Temp time series")

setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Temp time series")

setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses")

dwa<-read.csv("Dwarsberg.csv",skip=0)
con<-read.csv("Constantiaberg.csv",skip=0)
f5<-read.csv("fog500.csv",skip=0)
f6<-read.csv("fog600.csv",skip=0)
f7<-read.csv("fog700.csv",skip=0)
f8<-read.csv("fog800.csv",skip=0)

dwa<-read.csv("Dwarsberg.csv",skip=0)
con<-read.csv("Constantiaberg.csv",skip=0)
f5<-read.csv("Langrivier fog 500.csv",skip=0)
f6<-read.csv("Langrivier fog 600.csv",skip=0)
f7<-read.csv("Langrivier fog 700.csv",skip=0)
f8<-read.csv("Langrivier fog 800.csv",skip=0)


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


#Convert the TIMESTAMP into an appropriate format
f5$TIMESTAMP <- strptime(f5$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
f6$TIMESTAMP <- strptime(f6$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
f7$TIMESTAMP <- strptime(f7$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
f8$TIMESTAMP <- strptime(f8$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
con$TIMESTAMP <- strptime(con$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")
dwa$TIMESTAMP <- strptime(dwa$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")

#Subset the columns you want to graph (below example is for fog and VPD. If you want all VPD on the same graph, exclude fog at this point by subsetting only TIMESTAMP and vpd)
d<-subset(dwa, select = c(TIMESTAMP,Fog.corrected.mm, vpd))
c<-subset(con, select = c(TIMESTAMP,Fog.corrected.mm, vpd))
ff5<-subset(f5, select = c(TIMESTAMP,Fog.corrected.mm, vpd))	
ff6<-subset(f6, select = c(TIMESTAMP,Fog.corrected.mm, vpd))
ff7<-subset(f7, select = c(TIMESTAMP,Fog.corrected.mm, vpd))
ff8<-subset(f8, select = c(TIMESTAMP,Fog.corrected.mm, vpd))

#Convert to an xts object
dw <- xts(d[,3], order.by = d$TIMESTAMP)
cb <- xts(c[,3], order.by = c$TIMESTAMP)
l5 <- xts(ff5[,3], order.by = ff5$TIMESTAMP)
l6 <- xts(ff6[,3], order.by = ff6$TIMESTAMP)
l7 <- xts(ff7[,3], order.by = ff7$TIMESTAMP)
l8 <- xts(ff8[,3], order.by = ff8$TIMESTAMP)

#Bind them together in the same xts object
#vpd <- cbind(dw,l5,l6,l7,l8)
#colnames(vpd)<-c("dwa", "fog5", "fog6", "fog7", "fog8")

#Graph
#dygraph(vpd, main = "VPD - All", ylab = "VPD") %>% dyRangeSelector()

#new <- cbind.data.frame(dwa$vpd, ff5$vpd, ff6$vpd)


d<-subset(dwa, select = c(vpd))
d$id<-"Dwarsberg"
c<-subset(con, select = c(vpd))
c$id<-"Constantiaberg"
ff5<-subset(f5, select = c(vpd))
ff5$id<-"Lang500"
ff6<-subset(f6, select = c(vpd))
ff6$id<-"Lang600"
ff7<-subset(f7, select = c(vpd))
ff7$id<-"Lang700"
ff8<-subset(f8, select = c(vpd))
ff8$id<-"Lang800"

total <- rbind(d, c, ff8, ff7, ff6, ff5)
t<-na.omit(total)

ggplot(t, aes(x = id, y = vpd)) + geom_boxplot(fill = "grey80", colour = "blue") +   scale_x_discrete() + xlab("Site") +   ylab("Vapour pressure deficit")

res = lm(vpd ~ id, data = t)
summary(res)
anova(res)

aov1<-aov(vpd ~ id, data=t)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

#posthoc <- TukeyHSD(x=id, 'vpd', conf.level=0.95)

a=HSD.test(aov1, "id", group=TRUE)
head(a)


res = data.frame(Fitted = fitted(res), Residuals = resid(res), Treatment = t$id)
#and then produce the plot:

ggplot(res, aes(Fitted, Residuals, colour = Treatment)) + geom_point()


ggplot(t, aes(x = id, y = vpd)) + geom_boxplot(fill = "grey80", colour = "blue") +   scale_x_discrete() + xlab("Site") +   ylab("VPD")

########################################

aov1<-aov(vpd ~ id, data=total)
summary(aov1)
model.tables(aov1,"means")

TukeyHSD(aov1)

a=HSD.test(aov1, "id", group=TRUE)
head(a)

graph_summary<-ddply(t, c("id"), summarize, AVERAGE=mean(vpd), SE=sqrt(var(vpd)/length(vpd)))

ggplot(graph_summary)+ aes(x=id, y=AVERAGE, colour=id)+ geom_point()+  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+  scale_x_discrete("Site") 



