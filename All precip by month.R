#ALL MONTHLY SUMMARIES IN A GRIDDED PLOT
---------------------------------------
  setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis")
  
#For short time series to compare identical periods
  setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis/Short time series")

  ##CREATE PLOTABLE SUMMARY FOR 800
  dat800<-read.csv("Fog800_clean.csv",skip=0)
  test<-zooreg(dat800[,2:13], order.by=dat800$TIMESTAMP)
  evfog <- eventseq(test$Fog.corrected.mm, thresh=0.03)
  evrain <- eventseq(test$Rain_Tot.mm, thresh=0.253, below = FALSE)
  evfogonly <- eventseq(test$Fog.corrected.mm > 0.03 & test$Rain_Tot.mm<0.254)
  fog<-eventapply(test$Fog.corrected.mm, evfogonly, FUN = sum)
  rain<-eventapply(test$Rain_Tot.mm, evrain, FUN = sum)
  x=as.data.frame(fog)
  y=as.data.frame(rain)
  fmtot<-aggregate(x, by=list(substr(rownames(x),1,6)), FUN="sum")
  rmtot<-aggregate(y, by=list(substr(rownames(y),1,6)), FUN="sum")
  allmtot<-cbind(fmtot,rmtot)
  mplot<-allmtot[ c(1,2,4) ]
  L800 <- melt(mplot, id.vars = 'Group.1', variable.name = 'series')
  
  ##CREATE PLOTABLE SUMMARY FOR 700
  dat700<-read.csv("Fog700_clean.csv",skip=0)
  test<-zooreg(dat700[,2:13], order.by=dat700$TIMESTAMP)
  evfog <- eventseq(test$Fog.corrected.mm, thresh=0.03)
  evrain <- eventseq(test$Rain_Tot.mm, thresh=0.253, below = FALSE)
  evfogonly <- eventseq(test$Fog.corrected.mm > 0.03 & test$Rain_Tot.mm<0.254)
  fog<-eventapply(test$Fog.corrected.mm, evfogonly, FUN = sum)
  rain<-eventapply(test$Rain_Tot.mm, evrain, FUN = sum)
  x=as.data.frame(fog)
  y=as.data.frame(rain)
  fmtot<-aggregate(x, by=list(substr(rownames(x),1,6)), FUN="sum")
  rmtot<-aggregate(y, by=list(substr(rownames(y),1,6)), FUN="sum")
  allmtot<-cbind(fmtot,rmtot)
  mplot<-allmtot[ c(1,2,4) ]
  L700 <- melt(mplot ,  id.vars = 'Group.1', variable.name = 'series')
  
  ##CREATE PLOTABLE SUMMARY FOR 600
  dat600<-read.csv("Fog600_clean.csv",skip=0)
  test<-zooreg(dat600[,2:13], order.by=dat600$TIMESTAMP)
  evfog <- eventseq(test$Fog.corrected.mm, thresh=0.03)
  evrain <- eventseq(test$Rain_Tot.mm, thresh=0.253, below = FALSE)
  evfogonly <- eventseq(test$Fog.corrected.mm > 0.03 & test$Rain_Tot.mm<0.254)
  fog<-eventapply(test$Fog.corrected.mm, evfogonly, FUN = sum)
  rain<-eventapply(test$Rain_Tot.mm, evrain, FUN = sum)
  x=as.data.frame(fog)
  y=as.data.frame(rain)
  fmtot<-aggregate(x, by=list(substr(rownames(x),1,6)), FUN="sum")
  rmtot<-aggregate(y, by=list(substr(rownames(y),1,6)), FUN="sum")
  allmtot<-rbind.fill(fmtot,rmtot)
  L600 <- melt(allmtot ,  id.vars = 'Group.1', variable.name = 'series')
  
  ##CREATE PLOTABLE SUMMARY FOR 500
  dat500<-read.csv("Fog500_clean.csv",skip=0)
  test<-zooreg(dat500[,2:13], order.by=dat500$TIMESTAMP)
  evfog <- eventseq(test$Fog.corrected.mm, thresh=0.03)
  evrain <- eventseq(test$Rain_Tot.mm, thresh=0.253, below = FALSE)
  evfogonly <- eventseq(test$Fog.corrected.mm > 0.03 & test$Rain_Tot.mm<0.254)
  fog<-eventapply(test$Fog.corrected.mm, evfogonly, FUN = sum)
  rain<-eventapply(test$Rain_Tot.mm, evrain, FUN = sum)
  x=as.data.frame(fog)
  y=as.data.frame(rain)
  fmtot<-aggregate(x, by=list(substr(rownames(x),1,6)), FUN="sum")
  rmtot<-aggregate(y, by=list(substr(rownames(y),1,6)), FUN="sum")
  allmtot<-rbind.fill(fmtot,rmtot)
  L500 <- melt(allmtot ,  id.vars = 'Group.1', variable.name = 'series')
  
  ##CREATE PLOTABLE SUMMARY FOR Dwarsberg
  datdwa<-read.csv("Dwarsberg_clean.csv",skip=0)
  test<-zooreg(datdwa[,2:22], order.by=datdwa$TIMESTAMP)
  evfog <- eventseq(test$Fog.corrected.mm, thresh=0.03)
  evrain <- eventseq(test$Rain_Tot.mm, thresh=0.253, below = FALSE)
  evfogonly <- eventseq(test$Fog.corrected.mm > 0.03 & test$Rain_Tot.mm<0.254)
  fog<-eventapply(test$Fog.corrected.mm, evfogonly, FUN = sum)
  rain<-eventapply(test$Rain_Tot.mm, evrain, FUN = sum)
  x=as.data.frame(fog)
  y=as.data.frame(rain)
  fmtot<-aggregate(x, by=list(substr(rownames(x),1,6)), FUN="sum")
  rmtot<-aggregate(y, by=list(substr(rownames(y),1,6)), FUN="sum")
  allmtot<-cbind(fmtot,rmtot)
  mplot<-allmtot[ c(1,2,4) ]
  Dwa <- melt(mplot ,  id.vars = 'Group.1', variable.name = 'series')
  
  ##CREATE PLOTABLE SUMMARY FOR Constantiaberg
  datcon<-read.csv("Constantiaberg_clean.csv",skip=0)
  test<-zooreg(datcon[,2:15], order.by=datcon$TIMESTAMP)
  evfog <- eventseq(test$Fog.corrected.mm, thresh=0.03)
  evrain <- eventseq(test$Rain_Tot.mm, thresh=0.253, below = FALSE)
  evfogonly <- eventseq(test$Fog.corrected.mm > 0.03 & test$Rain_Tot.mm<0.254)
  fog<-eventapply(test$Fog.corrected.mm, evfogonly, FUN = sum)
  rain<-eventapply(test$Rain_Tot.mm, evrain, FUN = sum)
  x=as.data.frame(fog)
  y=as.data.frame(rain)
  fmtot<-aggregate(x, by=list(substr(rownames(x),1,6)), FUN="sum")
  rmtot<-aggregate(y, by=list(substr(rownames(y),1,6)), FUN="sum")
  allmtot<-cbind(fmtot,rmtot)
  mplot<-allmtot[ c(1,2,4) ]
  combined <- rbind.fill(fmtot[c("Group.1", "fog")], rmtot[c("Group.1", "rain")])
  Con <- melt(combined ,  id.vars = 'Group.1', variable.name = 'series')
  
  ##CREATE THE PLOTS
  Pdwa<- ggplot(Dwa, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + ylim(c(0, 900)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 1214") + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  Pcon<- ggplot(Con, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + ylim(c(0, 900)) + ylab("Precip mm") + xlab("Month") + ggtitle("Constantiaberg 890") + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "bottom")
  P800<- ggplot(L800, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + ylim(c(0, 900)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 800") + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  P700<- ggplot(L700, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + ylim(c(0, 900)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 700") + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  P600<- ggplot(L600, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + ylim(c(0, 900)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 600") + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  P500<- ggplot(L500, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + ylim(c(0, 900)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 500") + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  
  
  ##CREATE THE PLOTS (FOR SHORT TIME SERIES WITH LOWER RAINFALL AND ADJUSTED YLIM)
  Pdwa<- ggplot(Dwa, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + ylim(c(0, 600)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 1214") + theme(axis.text=element_text(size=11)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  Pcon<- ggplot(Con, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) +  ylim(c(0, 600)) + ylab("Precip mm") + xlab("Month") + ggtitle("Constantiaberg 890") + theme(axis.text=element_text(size=11)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "bottom")
  P800<- ggplot(L800, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + ylim(c(0, 600)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 800") + theme(axis.text=element_text(size=11)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  P700<- ggplot(L700, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + ylim(c(0, 600)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 700") + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  P600<- ggplot(L600, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + ylim(c(0, 600)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 600") + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  P500<- ggplot(L500, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + ylim(c(0, 600)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 500") + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  
  
  ##PUT THE PLOTS IN A GRID
  grid.arrange(Pdwa, P800,Pcon, ncol=1, nrow=3)
  grid.arrange(Pdwa, P800,P700,P600,P500,Pcon, ncol=2, nrow=3)
  grid.arrange(Pdwa, P800,P700,P600,P500, ncol=2, nrow=3)
  
##Calculate annual totals
  aggregate(mplot$rain, by=list(substr(mplot$Group.1,1,4)), FUN="sum") 
  aggregate(mplot$fog, by=list(substr(mplot$Group.1,1,4)), FUN="sum") 
  
  aggregate(mplot$rain, by=list(substr(mplot$Group.1,1,6)), FUN="sum") 
  aggregate(mplot$fog, by=list(substr(mplot$Group.1,1,6)), FUN="sum")
  
  ##Use annotate to put stars above bars with missing data
  ggplot(Dwa, aes(x=Group.1, y=value, fill=variable)) + geom_bar(stat="identity", colour="black", position= "dodge") + scale_fill_grey(start = 0, end = .9) + theme_bw() + annotate("text", x = c("201509","201511"), y = 500, label = c("*")) + ylim(c(0, 900)) + ylab("Precip mm") + xlab("Month") + ggtitle("Jonkershoek 1214") + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
  
  
  
  write.csv(Dwa, "C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/Raw data/DWAFOGRAIN.csv", sep="\t", col.names=T,quote=F, row.names=T, na = "NA")