setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Short time series")
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Temp time series")
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis")

Dwadat<-read.csv("Dwarsberg.csv",skip=0)				    ##Read in
Dwaprec<-Dwadat[!(Dwadat$Fog.corrected.mm=="0"),]			##Create dataframe that excludes all fog=0 rows
Dwafogonly<-Dwaprec[(Dwaprec$Rain_Tot.mm=="0"),]				##From the "Prec" dataframe, create a new dataframe that excludes all Rain_Tot.mm=="0"
Dwadat$TIMESTAMP<-as.Date(Dwadat$TIMESTAMP)
Dwaprec$TIMESTAMP<-as.Date(Dwaprec$TIMESTAMP)
Dwafogonly$TIMESTAMP<-as.Date(Dwafogonly$TIMESTAMP)

Condat<-read.csv("Constantiaberg.csv",skip=0)				##Read in
Conprec<-Condat[!(Condat$Fog.corrected.mm=="0"),]			##Create dataframe that excludes all fog=0 rows
Confogonly<-Conprec[(Conprec$Rain_Tot.mm=="0"),]	
Condat$TIMESTAMP<-as.Date(Condat$TIMESTAMP)
Conprec$TIMESTAMP<-as.Date(Conprec$TIMESTAMP)
Confogonly$TIMESTAMP<-as.Date(Confogonly$TIMESTAMP)


F8dat<-read.csv("Fog800.csv",skip=0)				##Read in
F8prec<-F8dat[!(F8dat$Fog.corrected.mm=="0"),]			##Create dataframe that excludes all fog=0 rows
F8fogonly<-F8prec[(F8prec$Rain_Tot.mm=="0"),]	
F8dat$TIMESTAMP<-as.Date(F8dat$TIMESTAMP)
F8prec$TIMESTAMP<-as.Date(F8prec$TIMESTAMP)
F8fogonly$TIMESTAMP<-as.Date(F8fogonly$TIMESTAMP)

F7dat<-read.csv("Langrivier Fog 700.csv",skip=0)				##Read in
F7prec<-F7dat[!(F7dat$Fog.corrected.mm=="0"),]			##Create dataframe that excludes all fog=0 rows
F7fogonly<-F7prec[(F7prec$Rain_Tot.mm=="0"),]	
F7dat$TIMESTAMP<-as.Date(F7dat$TIMESTAMP)
F7prec$TIMESTAMP<-as.Date(F7prec$TIMESTAMP)
F7fogonly$TIMESTAMP<-as.Date(F7fogonly$TIMESTAMP)

F6dat<-read.csv("Langrivier Fog 600.csv",skip=0)				##Read in
F6prec<-F6dat[!(F6dat$Fog.corrected.mm=="0"),]			##Create dataframe that excludes all fog=0 rows
F6fogonly<-F6prec[(F6prec$Rain_Tot.mm=="0"),]	
F6dat$TIMESTAMP<-as.Date(F6dat$TIMESTAMP)
F6prec$TIMESTAMP<-as.Date(F6prec$TIMESTAMP)
F6fogonly$TIMESTAMP<-as.Date(F6fogonly$TIMESTAMP)

F5dat<-read.csv("Langrivier Fog 500.csv",skip=0)				##Read in
F5prec<-F5dat[!(F5dat$Fog.corrected.mm=="0"),]			##Create dataframe that excludes all fog=0 rows
F5fogonly<-F5prec[(F5prec$Rain_Tot.mm=="0"),]	
F5dat$TIMESTAMP<-as.Date(F5dat$TIMESTAMP)
F5prec$TIMESTAMP<-as.Date(F5prec$TIMESTAMP)
F5fogonly$TIMESTAMP<-as.Date(F5fogonly$TIMESTAMP)


d<-ggplot() + geom_point(data=Dwadat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=Dwaprec, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "blue", size=0.5)) + (geom_point(data=Dwafogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + ggtitle("Jonkershoek 1214") + geom_hline(yintercept = 17)
c<-ggplot() + geom_point(data=Condat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=Conprec, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "blue", size=0.5)) + (geom_point(data=Confogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + ggtitle("Constantiaberg 890") + geom_hline(yintercept = 17)
F8<-ggplot() + geom_point(data=F8dat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=F8prec, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "blue", size=0.5)) + (geom_point(data=F8fogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + ggtitle("Jonkershoek 800") + geom_hline(yintercept = 17)
F7<-ggplot() + geom_point(data=F7dat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=F7prec, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "blue", size=0.5)) + (geom_point(data=F7fogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + ggtitle("Jonkershoek 700") + geom_hline(yintercept = 17)
F6<-ggplot() + geom_point(data=F6dat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=F6prec, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "blue", size=0.5)) + (geom_point(data=F6fogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + ggtitle("Jonkershoek 600") + geom_hline(yintercept = 17)
F5<-ggplot() + geom_point(data=F5dat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=F5prec, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "blue", size=0.5)) + (geom_point(data=F5fogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) + ggtitle("Jonkershoek 500") + geom_hline(yintercept = 17)

d<-ggplot() + geom_point(data=Dwadat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=Dwafogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + ggtitle("Jonkershoek 1214") + geom_hline(yintercept = 17) + theme(axis.title.x = element_blank()) 
c<-ggplot() + geom_point(data=Condat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=Confogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + ggtitle("Constantiaberg 890") + geom_hline(yintercept = 17) + theme(axis.title.x = element_blank())
F8<-ggplot() + geom_point(data=F8dat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=F8fogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + ggtitle("Jonkershoek 800") + geom_hline(yintercept = 17) + theme(axis.title.x = element_blank())
F7<-ggplot() + geom_point(data=F7dat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=F7fogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + ggtitle("Jonkershoek 700") + geom_hline(yintercept = 17)
F6<-ggplot() + geom_point(data=F6dat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=F6fogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + ggtitle("Jonkershoek 600") + geom_hline(yintercept = 17)
F5<-ggplot() + geom_point(data=F5dat, aes(x=TIMESTAMP, y=AirTC_Avg), colour = "darkgrey", size=0.5) + (geom_point(data=F5fogonly, aes(x=TIMESTAMP, y=AirTC_Avg),colour = "black", size=0.5)) + scale_y_continuous(breaks=seq(0, 40, 5)) + theme(axis.title.x = element_blank()) + ylab("Temperature") + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + ggtitle("Jonkershoek 500") + geom_hline(yintercept = 17)



grid.arrange(d,F8,F7,F6,F5,c, ncol=2, nrow=3)
grid.arrange(d,F8,F7,F6,F5, ncol=2, nrow=3)
grid.arrange(d,F8,c, ncol=1, nrow=3)
