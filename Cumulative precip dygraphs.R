##Set up a dataframe with cumulative rainfall and all precip for plotting by year
setwd("C:/Users/ABRI/Documents/SAEON/Projects/Fog project_Abri/Data for analyses/2017 reanalysis")

dat<-read.csv("Dwarsberg.csv",skip=0)
x<-subset(dat, select = c(TIMESTAMP,Rain_Tot.mm,Fog.corrected.mm))				#Choose columns to work with
x <- x %>% mutate(Fog.corrected.mm = ifelse(is.na(Fog.corrected.mm),0,Fog.corrected.mm))	#Change NA to "0" 
x <- x %>% mutate(Rain_Tot.mm = ifelse(is.na(Rain_Tot.mm),0,Rain_Tot.mm))			#Change NA to "0"
x$prec <- ifelse(x[,2] > "0", x[,2], x[,3] )							#Create prec column with rain and fogonly (this puts rainfall in the "prec" column when there is rain and fog and it puts fog in the "prec" column when there is fog and rain=0)
x$year <- as.numeric(format(as.Date(x$TIMESTAMP), format="%Y"))					#Create a year column from the TIMESTAMP  
cumRain<-ddply(x,.(year),transform,cumRain = cumsum(Rain_Tot.mm))				#create a dataframe with cumulative rain as a column
cumPrec<-ddply(x,.(year),transform,cumPrec = cumsum(prec))					#create a dataframe with cumulative Prec as a column
all<-merge(x = cumPrec, y = cumRain, by = "TIMESTAMP", all=TRUE)				#Merge the two dfs
z<-subset(all, select = c(TIMESTAMP,cumPrec,cumRain))						#Choose the three relevant columns to plot

#then, for dygraphs:
z$TIMESTAMP <- strptime(z$TIMESTAMP, format = "%Y/%m/%d %H:%M", tz="Africa/Johannesburg")	#Set the correct time format
zz <- xts(z[,2:3], order.by = z$TIMESTAMP)							#Convert to xts
dygraph(zz, main = "Cumulative rainfall vs cumulative summed precipitation - Jonkershoek 1214", ylab = "Precip in mm") # %>% dyRangeSelector()
dygraph(zz, main = "", ylab = "Precip in mm") # %>% dyRangeSelector()

#OR, for ggplot:
Con <- melt(z ,  id.vars = 'TIMESTAMP', variable.name = 'series')
Con$year <- as.numeric(format(as.Date(Con$TIMESTAMP), format="%Y"))
ggplot(Con, aes(x=TIMESTAMP, y=value, colour=variable, fill=variable )) + geom_point()+ theme(axis.title.x=element_blank(), axis.text.x=element_blank())
