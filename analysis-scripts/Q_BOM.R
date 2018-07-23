#Script to create estimates of Q, CV, Q.ct, power, etc for BOM chapter 
source("./analysis-scripts/Qsubstrate.R")
#about 2 mins
####  Isolate all Q between 2011-July-31 and 2012-Aug-15 ####

Q_BOM <- subset(Q_allhr, Pd >= as.POSIXct('2011-07-31') & Pd <= as.POSIXct('2012-08-15'))
temp_BOM = subset(temp_allhr, Pd >= as.POSIXct('2011-07-31') & Pd <= as.POSIXct('2012-08-15'))
tforce_BOM = subset(tforce_allhr, Pd >= as.POSIXct('2011-07-31') & Pd <= as.POSIXct('2012-08-15'))
RBS_BOM = subset(RBS_allhr, Pd >= as.POSIXct('2011-07-31') & Pd <= as.POSIXct('2012-08-15'))

dfs = list(Q_BOM, temp_BOM, tforce_BOM, RBS_BOM)

dfs = lapply(dfs, setNames, nm = c("Pd","st1", "st5", "st6", "st8", "st9","st11L", "st11U",
                             "st13","st14","st17","hver"))


colnames(Q_BOM) = c("Pd","st1", "st5", "st6", "st8", "st9","st11L", "st11U",
                            "st13","st14","st17","hver")
colnames(temp_BOM) = c("Pd","st1", "st5", "st6", "st8", "st9","st11L", "st11U",
                            "st13","st14","st17","hver")
colnames(tforce_BOM) = c("Pd","st1", "st5", "st6", "st8", "st9","st11L", "st11U",
                                 "st13","st14","st17","hver")
colnames(RBS_BOM) = c("Pd","st1", "st5", "st6", "st8", "st9","st11L", "st11U",
                                 "st13","st14","st17","hver")

dfs_l = melt(dfs, id = c("Pd"))



Q_BOM.l <- melt(Q_BOM, id.vars = c("Pd"))
colnames(Q_BOM.l) <- c("Pd", "Stream", "Discharge")

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

