#script to check on the coverage of raw data files
library(ggplot2)
library(scales)
theme_set(theme_bw(20))

presHver <- read.csv("./stream-data/2451126_Hver.txt", header = T, sep = "\t", quote = "")
presHver$Pd <- as.POSIXct(paste(presHver$Date, presHver$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-21 10:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(presHver, aes(x = Pd)) + geom_point(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_point(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#Hver starts at mid 2011, good.

pres11D <- read.table("./stream-data/9736055_ST11L.txt", header = T, se = "\t", quote = "")
pres11D$Pd <- as.POSIXct(paste(pres11D$Date, pres11D$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2015-08-10 10:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres11D, aes(x = Pd)) + geom_point(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_point(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST11D starts 2010, early to mid 2013 hole in depth measures. Good! Ends late 2014

pres11U <- read.table("./stream-data/2451129_ST11U.txt", header = T, sep = "\t", quote = "")
pres11U$Pd <- as.POSIXct(paste(pres11U$Date, pres11U$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2015-08-10 14:15:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres11U, aes(x = Pd)) + geom_point(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_point(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST11U depth is empty until mid 2012--filled in with ST17 (r2 ~ 0.9)

pres17 <- read.table("./stream-data/9736062_ST17.txt", header = T, sep = "\t", quote = "")
pres17$Pd <- as.POSIXct(paste(pres17$Date, pres17$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2015-08-10 14:15:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres17, aes(x = Pd)) + geom_point(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_point(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST17 is good for both depth & temp until late 2014

pres13 <- read.table("./stream-data/9736053_ST13.txt", header = T, sep = "\t", quote = "")
pres13$Pd <- as.POSIXct(paste(pres13$Date, pres13$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
##ST 13 edit ## 
#removing outlier
pres13 = pres13[which(pres13$TempC <= 9),]

######

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2015-08-10 14:15:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres13, aes(x = Pd)) + geom_point(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_point(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST13 is good until mid 2011 where there is a temperature anomoly. 
#mean - 14.5 L/s
#sd - 1.644
#CV - 0.113

write.table(pres13, file = "./stream-data/9736053_ST13.txt", row.names = F, sep = "\t", quote = F)
#####

pres14 <- read.table("./stream-data/9736060_ST14.txt", header = T, sep = "\t", quote = "")
pres14$Pd <- as.POSIXct(paste(pres14$Date, pres14$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2015-08-10 14:15:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres14, aes(x = Pd)) + geom_point(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_point(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST14 is good for depth and temperature until late 2014

pres1 <- read.table("./stream-data/9736056_ST1b.txt", header= T, sep = "\t", quote = "")
pres1$Pd <- as.POSIXct(paste(pres1$Date, pres1$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres1, aes(x = Pd)) + geom_point(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_point(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST1 is good until 2014, then depth needs to be input. temp all good.

pres5 <- read.table("./stream-data/9736061_ST5.txt", header = T, sep = "\t", quote = "")
pres5$Pd <- as.POSIXct(paste(pres5$Date, pres5$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres5, aes(x = Pd)) + geom_point(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_point(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST5 is good.
pres6 <- read.table("./stream-data/9736057_ST6.txt", header = T, sep = "\t", quote = "")
pres6$Pd <- as.POSIXct(paste(pres6$Date, pres6$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres6, aes(x = Pd)) + geom_point(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_point(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

pres6 = pres6[-which(pres6$TempC >= 35),]

pres6[which(pres6$Pd >= "2013-02-25 15:00:00" & pres6$Pd <= "2013-07-10 13:00:00"), "TempC"] = NA

#ST6 is good until early mid, then needs depth. Temp is weird. Switched to 
# pendant rather than well and cut a couple spots.

pres8 <- read.csv("./stream-data/9736058_ST8.csv")
pres8$Pd <- as.POSIXct(paste(pres8$Date, pres8$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres8, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST8 is good.
pres9 <- read.csv("./stream-data/9736054_ST9.csv")
pres9$Pd <- as.POSIXct(paste(pres9$Date, pres9$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres9, aes(x = Pd)) + geom_point(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_point(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST9 is pretty darn good. Need to rerun with new Q data.