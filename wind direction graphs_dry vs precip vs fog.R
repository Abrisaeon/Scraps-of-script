setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses")

dat500<-read.csv("Langrivier Fog 500.csv",skip=0) 	
pre500<-dat500[!(dat500$Fog.corrected=="0"),]
fog500<-pre500[(pre500$Rain_Tot.mm=="0"),]	
dat500$TIMESTAMP<-as.Date(dat500$TIMESTAMP)
pre500$TIMESTAMP<-as.Date(pre500$TIMESTAMP)
fog500$TIMESTAMP<-as.Date(fog500$TIMESTAMP)

dat600<-read.csv("Langrivier Fog 600.csv",skip=0) 	
pre600<-dat600[!(dat600$Fog.corrected=="0"),]
fog600<-pre600[(pre600$Rain_Tot.mm=="0"),]	
dat600$TIMESTAMP<-as.Date(dat600$TIMESTAMP)
pre600$TIMESTAMP<-as.Date(pre600$TIMESTAMP)
fog600$TIMESTAMP<-as.Date(fog600$TIMESTAMP)

dat700<-read.csv("Langrivier Fog 700.csv",skip=0) 	
pre700<-dat700[!(dat700$Fog.corrected=="0"),]
fog700<-pre700[(pre700$Rain_Tot.mm=="0"),]	
dat700$TIMESTAMP<-as.Date(dat700$TIMESTAMP)
pre700$TIMESTAMP<-as.Date(pre700$TIMESTAMP)
fog700$TIMESTAMP<-as.Date(fog700$TIMESTAMP)

dat800<-read.csv("Langrivier Fog 800.csv",skip=0) 	
pre800<-dat800[!(dat800$Fog.corrected=="0"),]
fog800<-pre800[(pre800$Rain_Tot.mm=="0"),]	
dat800$TIMESTAMP<-as.Date(dat800$TIMESTAMP)
pre800$TIMESTAMP<-as.Date(pre800$TIMESTAMP)
fog800$TIMESTAMP<-as.Date(fog800$TIMESTAMP)

Dwadat<-read.csv("Dwarsberg.csv",skip=0) 	
predwa<-Dwadat[!(Dwadat$Fog.corrected=="0"),]
fogdwa<-predwa[(predwa$Rain_Tot.mm=="0"),]	
Dwadat$TIMESTAMP<-as.Date(Dwadat$TIMESTAMP)
predwa$TIMESTAMP<-as.Date(predwa$TIMESTAMP)
fogdwa$TIMESTAMP<-as.Date(fogdwa$TIMESTAMP)

Condat<-read.csv("Constantiaberg.csv",skip=0) 
precon<-Condat[!(Condat$Fog.corrected=="0"),]
fogcon<-precon[(precon$Rain_Tot.mm=="0"),]	
Condat$TIMESTAMP<-as.Date(Condat$TIMESTAMP)
precon$TIMESTAMP<-as.Date(precon$TIMESTAMP)
fogcon$TIMESTAMP<-as.Date(fogcon$TIMESTAMP)

#For all wind, fog wind and all precip wind
#WDwa<- ggplot() + geom_point(data=Dwadat, aes(x=TIMESTAMP, y=WindDir_D1_WVT), alpha = 1/10, size=1) + (geom_point(data=predwa, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + (geom_point(data=fogdwa, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "red", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) +  scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + theme(axis.text.y = element_text(size = 7)) + ylab("Wind direction") + ggtitle("Dwarsberg") 
#WCon<- ggplot() + geom_point(data=Condat, aes(x=TIMESTAMP, y=WindDir_D1_WVT), alpha = 1/10, size=1) + (geom_point(data=precon, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + (geom_point(data=fogcon, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "red", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) +  scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + theme(axis.text.y = element_text(size = 7)) + ylab("Wind direction") + ggtitle("Constantiaberg") 
#W500<- ggplot() + geom_point(data=dat500, aes(x=TIMESTAMP, y=WindDir_D1_WVT), alpha = 1/10, size=1) + (geom_point(data=pre500, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + (geom_point(data=fog500, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "red", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) +  scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + theme(axis.text.y = element_text(size = 7)) + ylab("Wind direction") + ggtitle("Langrivier 500") 
#W600<- ggplot() + geom_point(data=dat600, aes(x=TIMESTAMP, y=WindDir_D1_WVT), alpha = 1/10, size=1) + (geom_point(data=pre600, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + (geom_point(data=fog600, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "red", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) +  scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + theme(axis.text.y = element_text(size = 7)) + ylab("Wind direction") + ggtitle("Langrivier 600")
#W700<- ggplot() + geom_point(data=dat700, aes(x=TIMESTAMP, y=WindDir_D1_WVT), alpha = 1/10, size=1) + (geom_point(data=pre700, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + (geom_point(data=fog700, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "red", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) +  scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + theme(axis.text.y = element_text(size = 7)) + ylab("Wind direction") + ggtitle("Langrivier 700") 
#W800<- ggplot() + geom_point(data=dat800, aes(x=TIMESTAMP, y=WindDir_D1_WVT), alpha = 1/10, size=1) + (geom_point(data=pre800, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + (geom_point(data=fog800, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "red", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) +  scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + theme(axis.text.y = element_text(size = 7)) + ylab("Wind direction") + ggtitle("Langrivier 800") 


WDwa<- ggplot() + geom_point(data=Dwadat, aes(x=TIMESTAMP, y=WindDir_D1_WVT),alpha = 1/10, size=1) +(geom_point(data=fogdwa, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + theme(axis.text.y = element_text(size = 6)) + ylab("Wind direction")  + ggtitle("Jonkershoek 1214")
WCon<- ggplot() + geom_point(data=Condat, aes(x=TIMESTAMP, y=WindDir_D1_WVT),alpha = 1/10, size=1) +(geom_point(data=fogcon, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1 , size = 7)) + theme(axis.text.y = element_text(size = 6)) + ylab("Wind direction")  + ggtitle("Constantiaberg 890")
W500<- ggplot() + geom_point(data=dat500, aes(x=TIMESTAMP, y=WindDir_D1_WVT),alpha = 1/10, size=1) +(geom_point(data=fog500, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + theme(axis.text.y = element_text(size = 6)) + ylab("Wind direction")  + ggtitle("Jonkershoek 500")
W600<- ggplot() + geom_point(data=dat600, aes(x=TIMESTAMP, y=WindDir_D1_WVT),alpha = 1/10, size=1) +(geom_point(data=fog600, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + theme(axis.text.y = element_text(size = 6)) + ylab("Wind direction")  + ggtitle("Jonkershoek 600")
W700<- ggplot() + geom_point(data=dat700, aes(x=TIMESTAMP, y=WindDir_D1_WVT),alpha = 1/10, size=1) +(geom_point(data=fog700, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + theme(axis.text.y = element_text(size = 6)) + ylab("Wind direction")  + ggtitle("Jonkershoek 700")
W800<- ggplot() + geom_point(data=dat800, aes(x=TIMESTAMP, y=WindDir_D1_WVT),alpha = 1/10, size=1) +(geom_point(data=fog800, aes(x=TIMESTAMP, y=WindDir_D1_WVT),colour = "blue", size=0.5)) + scale_y_continuous(breaks=seq(0, 360, 30)) + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))+ theme(axis.text.y = element_text(size = 6)) + ylab("Wind direction")  + ggtitle("Jonkershoek 800")



grid.arrange(WDwa, W800, W700, W600, W500, WCon, ncol=2, nrow=3)
grid.arrange(WDwa, W800, W700, W600, W500,ncol=2, nrow=3)
