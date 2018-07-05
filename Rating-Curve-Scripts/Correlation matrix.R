#Correlation of all depths

library(ggplot2)
library(chron)
library(gridExtra)
library(zoo)
library(scales)
library(GGally)
library(plyr)
library(data.table)
library(corrplot)
library(PerformanceAnalytics)
theme_set(theme_bw(20))

setwd("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/")


#load data

datetime <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/All_DateTime.csv")
Q <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/Q_data_summary_working.csv")
presL <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736059_7LO.csv")
presH <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736163_7HI_noNAs.csv")
pres1 <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736056_ST1b.csv")
pres5 <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736061_ST5.csv")
pres6 <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736057_ST6b.csv")
pres8 <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736058_ST8.csv")
pres9 <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736054_ST9.csv")
pres11D <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736055_ST11L.csv")
pres11U <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/2451129_ST11U.csv")
pres13 <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736053_ST13.csv")
pres14 <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736060_ST14.csv")
pres17 <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736062_ST17.csv")
presHver <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/2451126_Hver.csv")

pres1 <- pres1[,1:4]
#####
datetime$Pd <- as.POSIXct(paste(datetime$Date, datetime$Time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
Q$Pd <- as.POSIXct(paste(Q$Qdate, Q$Qtime), format = "%m/%d/%y %H:%M:%S", tz = "UTC")
Q <- Q[!is.na(Q$Pd),]
presL$Pd <- as.POSIXct(paste(presL$Date, presL$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
presH$Pd <- as.POSIXct(paste(presH$Date, presH$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres1$Pd <- as.POSIXct(paste(pres1$Date, pres1$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres5$Pd <- as.POSIXct(paste(pres5$Date, pres5$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres6$Pd <- as.POSIXct(paste(pres6$Date, pres6$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres8$Pd <- as.POSIXct(paste(pres8$Date, pres8$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres9$Pd <- as.POSIXct(paste(pres9$Date, pres9$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres11D$Pd <- as.POSIXct(paste(pres11D$Date, pres11D$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres11U$Pd <- as.POSIXct(paste(pres11U$Date, pres11U$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres13$Pd <- as.POSIXct(paste(pres13$Date, pres13$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres14$Pd <- as.POSIXct(paste(pres14$Date, pres14$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres17$Pd <- as.POSIXct(paste(pres17$Date, pres17$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
presHver$Pd <- as.POSIXct(paste(presHver$Date, presHver$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

#merging the full datetime file for 15min intervals from 7/20/2010 18:00:00 to Oct14

#pres1 <- merge(datetime, pres1, by = "Pd", all.x = T)

#pres1 <- rbind.fill(pres1[c("Depthm", "TempC", "Pd")], datetime["Pd"])
#pres1 <- do.call(rbind.fill, mylist) #this works

mylist <- list(pres1, datetime)
pres1 <- do.call(rbind.fill, mylist)

mylist <- list(presL, datetime)
presL <- do.call(rbind.fill, mylist)

mylist <- list(presH, datetime)
presH <- do.call(rbind.fill, mylist)

mylist <- list(pres5, datetime)
pres5 <- do.call(rbind.fill, mylist)

mylist <- list(pres6, datetime)
pres6 <- do.call(rbind.fill, mylist)

mylist <- list(pres8, datetime)
pres8 <- do.call(rbind.fill, mylist)

mylist <- list(pres9, datetime)
pres9 <- do.call(rbind.fill, mylist)

mylist <- list(pres11D, datetime)
pres11D <- do.call(rbind.fill, mylist)

mylist <- list(pres11U, datetime)
pres11U <- do.call(rbind.fill, mylist)

mylist <- list(pres13, datetime)
pres13 <- do.call(rbind.fill, mylist)

mylist <- list(pres14, datetime)
pres14 <- do.call(rbind.fill, mylist)

mylist <- list(pres17, datetime)
pres17 <- do.call(rbind.fill, mylist)

mylist <- list(presHver, datetime)
presHver <- do.call(rbind.fill, mylist)

#Make hourly means
	presLhr_d <- data.frame(presL$Pd, presL$Depthm, presL$TempC)
	presHhr_d <- data.frame(presH$Pd, presH$Depthm, presH$TempC)
	pres1hr_d <- data.frame(pres1$Pd, pres1$Depthm, pres1$TempC)
	pres5hr_d <- data.frame(pres5$Pd, pres5$Depthm, pres5$TempC)
	pres6hr_d <- data.frame(pres6$Pd, pres6$Depthm, pres6$TempC)
	pres8hr_d <- data.frame(pres8$Pd, pres8$Depthm, pres8$TempC)
	pres9hr_d <- data.frame(pres9$Pd, pres9$Depthm, pres9$TempC)
	pres11Dhr_d <- data.frame(pres11D$Pd, pres11D$Depthm, pres11D$TempC)
	pres11Uhr_d <- data.frame(pres11U$Pd, pres11U$Depthm, pres11U$TempC)
	pres13hr_d <- data.frame(pres13$Pd, pres13$Depthm, pres13$TempC)
	pres14hr_d <- data.frame(pres14$Pd, pres14$Depthm, pres14$TempC)
	pres17hr_d <- data.frame(pres17$Pd, pres17$Depthm, pres17$TempC)
	presHverhr_d <- data.frame(presHver$Pd, presHver$Depthm, presHver$TempC)

	
	names(presLhr_d) <- c("time", "st7L_depthm", "st7L_tempC")
	names(presHhr_d) <- c("time", "st7H_depthm", "st7H_tempC")
	names(pres1hr_d) <- c("time", "st1_depthm", "st1_tempC")
	names(pres5hr_d) <- c("time", "st5_depthm", "st5_tempC")
	names(pres6hr_d) <- c("time", "st6_depthm", "st6_tempC")
	names(pres8hr_d) <- c("time", "st8_depthm", "st8_tempC")
	names(pres9hr_d) <- c("time", "st9_depthm", "st9_tempC")
	names(pres11Dhr_d) <- c("time", "st11D_depthm", "st11D_tempC")
	names(pres11Uhr_d) <- c("time", "st11U_depthm", "st11U_tempC")
	names(pres13hr_d) <- c("time", "st13_depthm", "st13_tempC")
	names(pres14hr_d) <- c("time", "st14_depthm", "st14_tempC")
	names(pres17hr_d) <- c("time", "st17_depthm", "st17_tempC")
	names(presHverhr_d) <- c("time", "Hver_depthm", "Hver_tempC")

##First merge all the depth data by time
	
	presLhr <- aggregate(presLhr_d[c("st7L_depthm", "st7L_tempC")],
					list(hour = cut(presLhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)
					
	presHhr <- aggregate(presHhr_d[c("st7H_depthm", "st7H_tempC")],
					list(hour = cut(presHhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres1hr <- aggregate(pres1hr_d[c("st1_depthm", "st1_tempC")],
					list(hour = cut(pres1hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres5hr <- aggregate(pres5hr_d[c("st5_depthm", "st5_tempC")],
					list(hour = cut(pres5hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres6hr <- aggregate(pres6hr_d[c("st6_depthm", "st6_tempC")],
					list(hour = cut(pres6hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres8hr <- aggregate(pres8hr_d[c("st8_depthm", "st8_tempC")],
					list(hour = cut(pres8hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres9hr <- aggregate(pres9hr_d[c("st9_depthm", "st9_tempC")],
					list(hour = cut(pres9hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres11Dhr <- aggregate(pres11Dhr_d[c("st11D_depthm", "st11D_tempC")],
					list(hour = cut(pres11Dhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres11Uhr <- aggregate(pres11Uhr_d[c("st11U_depthm", "st11U_tempC")],
					list(hour = cut(pres11Uhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres13hr <- aggregate(pres13hr_d[c("st13_depthm", "st13_tempC")],
					list(hour = cut(pres13hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres14hr <- aggregate(pres14hr_d[c("st14_depthm", "st14_tempC")],
					list(hour = cut(pres14hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres17hr <- aggregate(pres17hr_d[c("st17_depthm", "st17_tempC")],
					list(hour = cut(pres17hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	presHverhr <- aggregate(presHverhr_d[c("Hver_depthm", "Hver_tempC")],
					list(hour = cut(presHverhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)


#convert times to posix object

	presLhr$time <- as.POSIXct(presLhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	presHhr$time <- as.POSIXct(presHhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres1hr$time <- as.POSIXct(pres1hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres5hr$time <- as.POSIXct(pres5hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres6hr$time <- as.POSIXct(pres6hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres8hr$time <- as.POSIXct(pres8hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres9hr$time <- as.POSIXct(pres9hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres11Dhr$time <- as.POSIXct(pres11Dhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres11Uhr$time <- as.POSIXct(pres11Uhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres13hr$time <- as.POSIXct(pres13hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres14hr$time <- as.POSIXct(pres14hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres17hr$time <- as.POSIXct(pres17hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	presHverhr$time <- as.POSIXct(presHverhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")

# # RUNNING ONE TIME, THEN EXPORTING TO GET PT TEMPS
# #get temp at times of slugs
	# #create zoo objects

		 temp_L <- with(presLhr, zoo(st7L_tempC, time))
		 temp_H <- with(presHhr, zoo(st7H_tempC, time))
		 temp_1 <- with(pres1hr, zoo(st1_tempC, time))
		 temp_5 <- with(pres5hr, zoo(st5_tempC, time))
		 temp_6 <- with(pres6hr, zoo(st6_tempC, time))
		 temp_8 <- with(pres8hr, zoo(st8_tempC, time))
		 temp_9 <- with(pres9hr, zoo(st9_tempC, time))
		 temp_11D <- with(pres11Dhr, zoo(st11D_tempC, time))
		 temp_11U <- with(pres11Uhr, zoo(st11U_tempC, time))
		 temp_13 <- with(pres13hr, zoo(st13_tempC, time))
		 temp_14 <- with(pres14hr, zoo(st14_tempC, time))
		 temp_17 <- with(pres17hr, zoo(st17_tempC, time))
		 temp_Hver <- with(presHverhr, zoo(Hver_tempC, time))


		 d1 <- with(pres1hr, zoo(st1_depthm, time))
		 dL <- with(presLhr, zoo(st7L_depthm, time))
		 dH <- with(presHhr, zoo(st7H_depthm, time))
		 d5 <- with(pres5hr, zoo(st5_depthm, time))
		 d6 <- with(pres6hr, zoo(st6_depthm, time))
		 d8 <- with(pres8hr, zoo(st8_depthm, time))
		 d9 <- with(pres9hr, zoo(st9_depthm, time))
		 d11D <- with(pres11Dhr, zoo(st11D_depthm, time))
		 d11U <- with(pres11Uhr, zoo(st11U_depthm, time))
		 d13 <- with(pres13hr, zoo(st13_depthm, time))
		 d14 <- with(pres14hr, zoo(st14_depthm, time))
		 d17 <- with(pres17hr, zoo(st17_depthm, time))
		 dHver <- with(presHverhr, zoo(Hver_depthm, time))




depths <- merge(presLhr[,c(2,4)], presHhr[,c(2,4)], by = "time", all = TRUE)
depths <- merge(depths, pres1hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","L_depthm", "H_depthm", "st1_depthm")
depths <- merge(depths, pres5hr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, pres6hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","L_depthm", "H_depthm", "st1_depthm", "st5_depthm", "st6_depthm")
depths <- merge(depths, pres8hr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, pres9hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","L_depthm", "H_depthm", "st1_depthm", "st5_depthm", "st6_depthm", "st8_depthm", "st9_depthm")
depths <- merge(depths, pres11Dhr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, pres11Uhr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","L_depthm", "H_depthm", "st1_depthm", "st5_depthm", "st6_depthm", "st8_depthm", "st9_depthm", "st11D_depthm", "st11U_depthm")
depths <- merge(depths, pres13hr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, pres14hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","L_depthm", "H_depthm", "st1_depthm", "st5_depthm", "st6_depthm", "st8_depthm", "st9_depthm", "st11D_depthm", "st11U_depthm", 
	"st13_depthm", "st14_depthm")
depths <- merge(depths, pres17hr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, presHverhr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","L_depthm", "H_depthm", "st1_depthm", "st5_depthm", "st6_depthm", "st8_depthm", "st9_depthm", "st11D_depthm", "st11U_depthm", 
	"st13_depthm", "st14_depthm",	"st17_depthm", "Hver_depthm")


depths <- depths[!as.numeric(format(depths$time, "%y")) == 1,]

save.image(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/all_streams.RData")
load( "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/all_streams.RData")

write.csv(depths, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/depths_all.csv")

temps <- merge(presLhr[,c(3,4)], presHhr[,c(3,4)], by = "time", all = TRUE)
temps <- merge(temps, pres1hr[,c(3,4)], by = "time", all = T)
names(temps) <- c("time","L_tempC", "H_tempC", "st1_tempC")
temps <- merge(temps, pres5hr[,c(3,4)], by = "time", all = T)
temps <- merge(temps, pres6hr[,c(3,4)], by = "time", all = T)
names(temps) <- c("time","L_tempC", "H_tempC", "st1_tempC", "st5_tempC", "st6_tempC")
temps <- merge(temps, pres8hr[,c(3,4)], by = "time", all = T)
temps <- merge(temps, pres9hr[,c(3,4)], by = "time", all = T)
names(temps) <- c("time","L_tempC", "H_tempC", "st1_tempC", "st5_tempC", "st6_tempC", "st8_tempC", "st9_tempC")
temps <- merge(temps, pres11Dhr[,c(3,4)], by = "time", all = T)
temps <- merge(temps, pres11Uhr[,c(3,4)], by = "time", all = T)
names(temps) <- c("time","L_tempC", "H_tempC", "st1_tempC", "st5_tempC", "st6_tempC", "st8_tempC", "st9_tempC", "st11D_tempC", "st11U_tempC")
temps <- merge(temps, pres13hr[,c(3,4)], by = "time", all = T)
temps <- merge(temps, pres14hr[,c(3,4)], by = "time", all = T)
names(temps) <- c("time","L_tempC", "H_tempC", "st1_tempC", "st5_tempC", "st6_tempC", "st8_tempC", "st9_tempC", "st11D_tempC", "st11U_tempC", 
	"st13_tempC", "st14_tempC")
temps <- merge(temps, pres17hr[,c(3,4)], by = "time", all = T)
temps <- merge(temps, presHverhr[,c(3,4)], by = "time", all = T)
names(temps) <- c("time","L_tempC", "H_tempC", "st1_tempC", "st5_tempC", "st6_tempC", "st8_tempC", "st9_tempC", "st11D_tempC", "st11U_tempC", 
	"st13_tempC", "st14_tempC",	"st17_tempC", "Hver_tempC")


temps <- temps[!as.numeric(format(temps$time, "%y")) == 1,]

write.csv(temps, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/temp_all.csv")


## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}
 
## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}
 
 mydata <- depths[,2:14]
 
# correlation matrix
cor(mydata)
 
# correlation matrix with p-values
cor.prob(mydata)
 
# "flatten" that table
flattenSquareMatrix(cor.prob(mydata))
 
# plot the data
library(PerformanceAnalytics)
depth_cor <- chart.Correlation(mydata)

pdf("Depth_cor_all.pdf", height = 30, width = 30)
chart.Correlation(mydata, pch = 19)
dev.off()

jpeg("Depth_cor_all.jpg", height = 6000, width = 6000, quality = 50, pointsize = 30)
chart.Correlation(mydata, pch = 21)
dev.off()

###Combining all Z and temps

dt1 <- data.table(pres1hr_d, key = "time")
dt5 <- data.table(pres5hr_d, key = "time")


mydf_list <- list(pres1hr_d, pres5hr_d, pres6hr_d, pres8hr_d, pres9hr_d, pres11Dhr_d, pres11Uhr_d, pres13hr_d, pres14hr_d, pres17hr_d, presHver)

TZ <- join_all(mydf_list, by = "time", type = "full")

TZ <- Reduce(function(...) merge(..., all = T), mydf_list)


TZ <- dt1[dt5]



TZ <- join(pres1hr_d, pres5hr_d, by = "time")
TZ <- join(TZ, pres6hr_d, by = "time")
names(TZ) <- c("time","st1_depthm", "st1_tempC", "st5_depthm", "st5_tempC",  "st6_depthm", "st6_tempC")
TZ <- TZ[!as.numeric(format(TZ$time, "%y")) == 1,]

TZ <- merge(TZ, presLhr_d, by = "time", all = T)
TZ <- merge(TZ, presHhr_d, by = "time", all = T)
names(TZ) <- c("time","st1_depthm", "st1_tempC", "st5_depthm", "st5_tempC",  "st6_depthm", "st6_tempC", "st7L_depthm", "st7L_tempC", "st7H_depthm", "st7H_tempC")
TZ <- merge(TZ, pres8hr_d, by = "time", all = T)
TZ <- merge(TZ, pres9hr_d, by = "time", all = T)
names(TZ) <- c("time","st1_depthm", "st1_tempC", "st5_depthm", "st5_tempC",  "st6_depthm", "st6_tempC", "st7L_depthm", "st7L_tempC", "st7H_depthm", "st7H_tempC",
		"st8_depthm", "st8_tempC", "st9_depthm", "st9_tempC")
TZ <- merge(TZ, pres11Dhr_d, by = "time", all = T)
TZ <- merge(TZ, pres11Uhr_d, by = "time", all = T)
names(TZ) <- c("time","st1_depthm", "st1_tempC", "st5_depthm", "st5_tempC",  "st6_depthm", "st6_tempC", "st7L_depthm", "st7L_tempC", "st7H_depthm", "st7H_tempC",
		"st8_depthm", "st8_tempC", "st9_depthm", "st9_tempC", "st11L_depthm", "st11L_tempC", "st11U_depthm", "st11U_tempC")

TZ <- merge(TZ, pres13hr_d, by = "time", all = T)
TZ <- merge(TZ, pres14hr_d, by = "time", all = T)
names(TZ) <- c("time","st1_depthm", "st1_tempC", "st5_depthm", "st5_tempC",  "st6_depthm", "st6_tempC", "st7L_depthm", "st7L_tempC", "st7H_depthm", "st7H_tempC",
		"st8_depthm", "st8_tempC", "st9_depthm", "st9_tempC", "st11L_depthm", "st11L_tempC", "st11U_depthm", "st11U_tempC", "st13_depthm", "st13_tempC", "st14_depthm",
		"st14_tempC")


TZ <- merge(TZ, pres17hr_d, by = "time", all = T)
TZ <- merge(TZ, presHverhr_d, by = "time", all = T)
names(TZ) <- c("time","st1_depthm", "st1_tempC", "st5_depthm", "st5_tempC",  "st6_depthm", "st6_tempC", "st7L_depthm", "st7L_tempC", "st7H_depthm", "st7H_tempC",
		"st8_depthm", "st8_tempC", "st9_depthm", "st9_tempC", "st11L_depthm", "st11L_tempC", "st11U_depthm", "st11U_tempC", "st13_depthm", "st13_tempC", "st14_depthm",
		"st14_tempC", "st17_depthm", "st17_tempC", "Hver_depthm", "Hver_tempC")


TZ <- TZ[!as.numeric(format(TZ$time, "%y")) == 1,]

write.csv(TZ, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/TempZ_all.csv")

save.image(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/TempZ_all.Rdata")


