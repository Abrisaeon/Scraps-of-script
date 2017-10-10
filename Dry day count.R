setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses") 

# SUM OF DRY DAYS BY MONTH FOR FOG VS RAIN:
    ------------------------------------------
  dat<-read.csv("Constantiaberg.csv",skip=0)
  rainday<-group_by(dat,day=as.Date(dat$TIMESTAMP)) %>% summarise(total=sum(Rain_Tot.mm))			#Calculate the total rainfall in mm per day
  fogday<-group_by(dat,day=as.Date(dat$TIMESTAMP)) %>% summarise(total=sum(Fog.corrected.mm))		#Calculate the total mixed precip in mm per day
  dailyprec=merge(x = fogday, y = rainday, by = "day", all=TRUE)						#Merge the two daily total dataframes
  colnames(dailyprec) <- c("date", "mixed", "rain")							#Rename the headings
  dailyprec$month <- as.factor(format(dailyprec$date,'%Y%m'))						#Create a column for yyyymm
  dailyprec["rcount"] <- dailyprec$rain < 0.254								#Identify days with rain less than one tip of the gauge (in other words zero)
  dailyprec["fcount"] <- dailyprec$mixed < 0.04								#Identify days with mixed precip less than one tip of the gauge (in other words zero)
  
  rmcount <- aggregate(dailyprec[c("rcount")], by=list(dailyprec$month), FUN=sum, na.rm=TRUE)		#Count days with rain as identified above
  fmcount <- aggregate(dailyprec[c("fcount")], by=list(dailyprec$month), FUN=sum, na.rm=TRUE)		#Count days with mixed as identified above
  drycount=merge(x = fmcount, y = rmcount, by = "Group.1", all=TRUE)					#Merge the count data by month
  
  df <- melt(drycount ,  id.vars = 'Group.1', variable.name = 'series')					#Melt the dataframe into the long form in order to plot it
  #ggplot(df, aes(x=Group.1, y=value)) + geom_bar(aes(fill=variable), stat="identity", colour="black", position=position_dodge()) + scale_fill_grey(start = 0, end = .9) + theme_bw() + annotate("text", x = c("201306","201406","201505","201506", "201507"), y = 24, label = c("*")) + xlab("Month") + ylab("Dry day count") + ggtitle("Jonkershoek 1214 dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #Dwarsberg
  ggplot(df, aes(x=Group.1, y=value)) + geom_bar(aes(fill=variable), stat="identity", colour="black", position=position_dodge()) + scale_fill_grey(start = 0, end = .9) + theme_bw() + annotate("text", x = c("201303","201306","201406","201505","201506", "201507"), y = 25, label = c("*")) + xlab("Month") + ylab("Dry day count") + ggtitle("Jonkershoek 1214 dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #Constantiaberg
  ggplot(df, aes(x=Group.1, y=value)) + geom_bar(aes(fill=variable), stat="identity", colour="black", position=position_dodge()) + scale_fill_grey(start = 0, end = .9) + theme_bw() + annotate("text", x = c("201502","201503"), y = 25, label = c("*")) + xlab("Month") + ylab("Dry day count") + ggtitle("Constantiaberg 890 dry days") + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
 # *******************************************************************************************************