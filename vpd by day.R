##APPLICATION TO CALCULATE AND PLOT NUMBER OF VPD = 0 HOURS PER DAY FOR DAYLIGHT HOURS
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ##Read in the data with original time format
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
  
  
  #Then: Create a column in the dataframe that records the presence/absence of fog by hour (in this case fog>0.03 and rain<0.2)
  
  con$vpdyn <- ifelse((con$vpd =="0"),1,0)
  dwa$vpdyn <- ifelse((dwa$vpd =="0"),1,0)
  f5$vpdyn <- ifelse((f5$vpd =="0"),1,0)
  f6$vpdyn <- ifelse((f6$vpd =="0"),1,0)
  f7$vpdyn <- ifelse((f7$vpd =="0"),1,0)
  f8$vpdyn <- ifelse((f8$vpd =="0"),1,0)
  

 con$TS<-as.POSIXct(con$TIMESTAMP,format = "%Y/%m/%d %H:%M",  tz = "Africa/Johannesburg") 
  a<- with( con , con[ hour( con$TS ) >= 7 & hour( con$TS ) < 19 , ] ) #this required the lubridate library
  
 
  tscon <- xts(a$vpdyn, as.Date(a$TIMESTAMP, "%Y/%m/%d %H:%M"))
  ts_con = apply.daily(tscon, sum)
  TSDcon<-data.frame(date=index(ts_con), coredata(ts_con)) 
  colnames(TSDcon) <- c("date", "hours")
  pcon<-ggplot(TSDcon, aes(x=date, y=hours)) + geom_bar(stat="identity", colour="black") + ylim(c(0, 25)) + ggtitle("Constantiaberg 890")
  ggplot(TSDcon, aes(x=date, y=hours)) + geom_bar(stat="identity", colour="black") + ylim(c(0, 25)) + ggtitle("Constantiaberg 890")
  
  grid.arrange(pdwa,pcon,p5,p6,p7,p8, ncol=2, nrow=3)
  
  