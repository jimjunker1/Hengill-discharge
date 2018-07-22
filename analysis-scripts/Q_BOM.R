#Script to create estimates of Q, CV, Q.ct, power, etc for BOM chapter 

##load libraries
library(ggplot2)
library(chron)
library(gridExtra)
library(zoo)
library(scales)
library(GGally)
library(plyr)
library(data.table)
library(corrplot)
library(reshape2)
theme_set(theme_bw(20))
#need to adjust this eventually
Q_all <- read.csv("./output-files/Q_all_fin.csv")
st_temps <- read.csv("./stream-data/stream_temps.csv",T)
####  Isolate all Q between Oct-11 and Aug-12 ####

Q_all$Pd <- as.POSIXct(paste(Q_all$Date), format = "%d-%m-%y", tz = "UTC")
Q_BOM <- subset(Q_all, Pd >= as.POSIXct('2011-09-01') & Pd <= as.POSIXct('2012-08-15'))


colnames(Q_BOM) <- c("Date", "Time", "ST1", "ST5", "ST6", "ST8", "ST9", 
 "ST11D", "ST13", "ST14", "ST17", "Hver", "Pd")

Q_BOM.l <- melt(Q_BOM, id.vars = c("Date", "Time", "Pd"))
colnames(Q_BOM.l) <- c("Date", "Time", "Pd", "Stream", "Discharge")

Q_BOM.sum <- Q_BOM.l %>%
	group_by(Stream) %>%
	summarize(median = median(Discharge, na.rm = T))

st_temps.feb <- st_temps[which(st_temps$Date == "Feb"),]

Q_BOM.l <- merge(Q_BOM.l, st_temps.feb, by = c("Stream"))

Q_BOM.l <- Q_BOM.l[,c(1:5,7:11)]
colnames(Q_BOM.l) <- c("Stream", "Date", "Time", "Pd", "Discharge", "Temp", "Slope", "CV", "substrate", "temp.mean")

Q_BOM.l <- transform(Q_BOM.l, power = 9800 * (Discharge/1000) * (Slope/100))


##now merging the new measure of Q.ct and max power with st_temps to use in BOM analysis

Q_BOM.l <- merge(Q_BOM.l, Q_BOM.sum, by = c("Stream"))

Q_BOM.jul <- subset(Q_BOM.l, Pd >= as.POSIXct('2012-02-01') & Pd <= as.POSIXct('2012-08-15'))
Q_BOM.feb <- subset(Q_BOM.l, Pd >= as.POSIXct('2011-09-01') & Pd <= as.POSIXct('2012-02-01'))

Q.jul <- Q_BOM.jul %>%
	group_by(Stream) %>%
	mutate(max.power = max(power, na.rm = T))

Q.jul <- Q.jul %>%
	group_by(Stream) %>%
	mutate(Q.ct = length(which(Discharge > 7 * median)))

Q.feb <- Q_BOM.feb %>%
	group_by(Stream) %>%
	mutate(max.power = max(power, na.rm = T))

Q.feb <- Q.feb %>%
	group_by(Stream) %>%
	mutate(Q.ct = length(which(Discharge > 7 * median)))

Q_jul <- ddply(Q.jul, c("Stream"), summarize, mean(Q.ct))
Q_jul1 <- ddply(Q.jul, c("Stream"), summarize, mean(max.power))

Q_feb <- ddply(Q.feb, c("Stream"), summarize, mean(Q.ct))
Q_feb1 <- ddply(Q.feb, c("Stream"), summarize, mean(max.power))

Q_jul <- merge(Q_jul, Q_jul1, by = "Stream", all = T)
Q_feb <- merge(Q_feb, Q_feb1, by = "Stream", all = T)

Q_jul$Month = "Jul"
Q_feb$Month = "Feb"

Q_sum  <- rbind(Q_jul,Q_feb)
colnames(Q_sum) <- c("Stream", "Q.ct", "max.power", "Date")
st_temps <- merge(st_temps, Q_sum, by = c("Stream", "Date"))

write.csv(st_temps, file = "C:/Users/Jim/Documents/Projects/Iceland/BOMs/BOM final/FinalRfiles/stream_temps.csv", row.names = F)




