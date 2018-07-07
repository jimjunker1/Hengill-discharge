#script to check on the coverage of raw data files
library(ggplot2)
library(scales)

presHver <- read.csv("./stream-data/2451126_Hver.csv")
presHver$Pd <- as.POSIXct(paste(presHver$Date, presHver$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-21 10:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(presHver, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#Hver starts at mid 2011. Small hole in early 2013 data, but otherwise good.

pres11D <- read.csv("./stream-data/9736055_ST11L.csv")
pres11D$Pd <- as.POSIXct(paste(pres11D$Date, pres11D$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-21 10:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres11D, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST11D starts 2010, early to mid 2013 hole in depth measures. Otherwise good.

pres11U <- read.csv("./stream-data/2451129_ST11U.csv")
pres11U$Pd <- as.POSIXct(paste(pres11U$Date, pres11U$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2015-08-10 14:15:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres11U, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST11U is empty until mid 2012. Need to update temperature data and depth before 
# mid 2012. Depth good late 2013 until mid 2015. Temperature not.

pres17 <- read.csv("./stream-data/9736062_ST17.csv")
pres17$Pd <- as.POSIXct(paste(pres17$Date, pres17$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres17, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST17 is missing early 2013. Otherwise good for both depth & temp.

pres13 <- read.csv("./stream-data/9736053_ST13.csv")
pres13$Pd <- as.POSIXct(paste(pres13$Date, pres13$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres13, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST13 is good until mid 2011 where there is a temperature anomoly. Hole in late 2012
# to mid 2013 in depth. Then good until 2015

pres14 <- read.csv("./stream-data/9736060_ST14.csv")
pres14$Pd <- as.POSIXct(paste(pres14$Date, pres14$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres14, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST14 is good until late 2012-late 2013 for depth and temperature. Then good to 2015

pres1 <- read.csv("./stream-data/9736056_ST1b.csv")
pres1$Pd <- as.POSIXct(paste(pres1$Date, pres1$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-3-28 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres1, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST1 is good until 2013. Then weird temperature signal and depth needs to be input

pres5 <- read.csv("./stream-data/9736061_ST5.csv")
pres5$Pd <- as.POSIXct(paste(pres5$Date, pres5$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres5, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST5 is good.
pres6 <- read.csv("./stream-data/9736057_ST6.csv")
pres6$Pd <- as.POSIXct(paste(pres6$Date, pres6$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

lims = as.POSIXct(strptime(c("2010-07-20 18:00:00", "2014-10-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))
ggplot(pres6, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST6 is good until early 2013, then needs depth. Still questioning the 
# temperature need to address

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
ggplot(pres9, aes(x = Pd)) + geom_line(aes(y = Depthm), color = 'blue', size = 1.5) +
  geom_line(aes(y = TempC), color = 'red', size = 1.5) + 
  scale_x_datetime(limits = lims)

#ST9 is pretty darn good. Need to rerun with new Q data.