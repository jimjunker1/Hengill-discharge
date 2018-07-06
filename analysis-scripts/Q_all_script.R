#Purpose: To create pressure-discharge rating curves across gradient streams
#Original code for st7 by Jim Hood
#Modified by Jim Junker. See log


##################### Change computer clock to Iceland time UCT  ############################

#load libraries
library(ggplot2)
library(chron)
library(gridExtra)
library(zoo)
library(scales)
library(GGally)
library(plyr)
library(dplyr)
library(data.table)
library(corrplot)
library(lubridate)
theme_set(theme_bw(20))

setwd("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Stream Discharge/All Q")

#########################load data from all streams###############################

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
names(depths) <- c("time","dL", "dH", "d1")
depths <- merge(depths, pres5hr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, pres6hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1", "d5", "d6")
depths <- merge(depths, pres8hr[,c(2:4)], by = "time", all = T)
depths <- merge(depths, pres9hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1", "d5", "d6", "d8", "temp_8", "d9")
depths <- merge(depths, pres11Dhr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, pres11Uhr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1", "d5", "d6", "d8", "temp_8", "d9", "d11D", "d11U")
depths <- merge(depths, pres13hr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, pres14hr[,c(2,4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1", "d5", "d6", "d8", "temp_8", "d9", "d11D", "d11U", 
                   "d13", "d14")
depths <- merge(depths, pres17hr[,c(2,4)], by = "time", all = T)
depths <- merge(depths, presHverhr[,c(2:4)], by = "time", all = T)
names(depths) <- c("time","dL", "dH", "d1", "d5", "d6", "d8", "temp_8", "d9", "d11D", "d11U", 
                   "d13", "d14", "d17", "dHver", "temp_Hver")


#depths <- depths[!as.numeric(format(depths$time, "%y")) == 1,]

head(depths)

#####################################################################################################################

############################################################################################
#####################Build discharge rating curves for all streams#############################################################
#load data
#load(	"~/Projects/Iceland/Temp-Disch-Light/Stream Discharge/All Q/all_streams.RData")

#names(depths) <- c("time", "dL", "dH", "d1", "d5", "d6", "d8", "d9", "d11D", "d11U", "d13", "d14", "d17", "dHver")

Q$Pd <- as.POSIXct(paste(Q$Qdate, Q$Qtime), format = "%m/%d/%y %H:%M:%S", tz = "UTC")
Q <- Q[!is.na(Q$Pd),]

NAs <- is.na(Q$Q_DS)
Q <- cbind(Q, NAs)
Q.mod <- matrix(NA,nrow(Q), ncol=1)
Q.samp <- matrix(0, 1, ncol=1)
for(i in 1:nrow(Q)) {
  if(is.na(Q$Q_DS[i]) == "TRUE") {Q.samp <- Q$Q_US[i] 
  Q.mod[i] <- Q.samp}
  
  else{Q.samp <- Q$Q_DS[i]
  Q.mod[i] <- Q.samp}
}
Q <- cbind(Q, Q.mod)
Q <- Q[,c(1:13,15,17)]

##  merging the Q and PT depth data
#ST1
Q1 <- Q[which(Q$Qstream == "st1"),]
Q1 <- Q1[order(Q1$Pd),]
Q1z <- with(Q1, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(temp_1)) - as.numeric(u)))
ix <- vapply(index(Q1z), f, integer(1))
QP <- cbind(Q1, temp_1 = coredata(temp_1)[ix])
Qw1 <- data.frame(QP)


f <- function(u) which.min(abs(as.numeric(index(d1)) - as.numeric(u)))
dx <- sapply(index(Q1z), f)
QP <- cbind(Qw1, d1 = coredata(d1) [dx])
Q1_full <- data.frame(QP)

sm_rating1 <- lm(log(Q.mod) ~ log(d1) , Q1_full); summary(sm_rating1)

#ST5
Q5 <- Q[which(Q$Qstream == "st5"),]
Q5 <- Q5[!is.na(Q5$Q.mod),]
Q5 <- Q5[order(Q5$Pd),]
Q5z <- with(Q5, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(temp_5)) - as.numeric(u)))
ix <- vapply(index(Q5z), f, integer(1))
QP <- cbind(Q5, temp_5 = coredata(temp_5)[ix])
Qw5 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(d5)) - as.numeric(u)))
dx <- sapply(index(Q5z), f)
QP <- cbind(Qw5, d5 = coredata(d5) [dx])
Q5_full <- data.frame(QP)

Q5_full_mod <- Q5_full[-6,]
sm_rating5_mod <- lm(log(Q.mod) ~ log(d5), Q5_full_mod); summary(sm_rating5_mod)

#ST6
Q6 <- Q[which(Q$Qstream == "st6"),]
Q6 <- Q6[!is.na(Q6$Q.mod),]
Q6 <- Q6[order(Q6$Pd),]
Q6z <- with(Q6, zoo(Q.mod, Pd))


f <-  function(u) which.min(abs(as.numeric(index(temp_6)) - as.numeric(u)))
ix <- vapply(index(Q6z), f, integer(1))
QP <- cbind(Q6, temp_6 = coredata(temp_6)[ix])
Qw6 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(d6)) - as.numeric(u)))
dx <- sapply(index(Q6z), f)
QP <- cbind(Qw6, d6 = coredata(d6) [dx])
Q6_full <- data.frame(QP)
Q6_full_mod <- Q6_full[!is.na(Q6_full$Q.mod),]
sm_rating6 <- lm(log(Q.mod) ~ log(d6), Q6_full_mod); summary(sm_rating6)

#ST8

Q8 <- Q[which(Q$Qstream == "st8"),]
Q8 <- Q8[!is.na(Q8$Q.mod),]
Q8 <- Q8[order(Q8$Pd),]
Q8z <- with(Q8, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(temp_8)) - as.numeric(u)))
ix <- vapply(index(Q8z), f, integer(1))
QP <- cbind(Q8, temp_8 = coredata(temp_8)[ix])
Qw8 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(d8)) - as.numeric(u)))
dx <- sapply(index(Q8z), f)
QP <- cbind(Qw8, d8 = coredata(d8) [dx])
Q8_full <- data.frame(QP)
Q8_full_mod <- Q8_full[which(Q8_full$Q.mod <= 40),]

sm_rating8 <- lm(log(Q.mod) ~ log(d8) + temp_8, Q8_full_mod); summary(sm_rating8)

#ST9
Q9 <- Q[which(Q$Qstream == "st9"),]
Q9 <- Q9[!is.na(Q9$Q.mod),]
Q9 <- Q9[order(Q9$Pd),]
Q9z <- with(Q9, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(temp_9)) - as.numeric(u)))
ix <- vapply(index(Q9z), f, integer(1))
QP <- cbind(Q9, temp_9 = coredata(temp_9)[ix])
Qw9 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(d9)) - as.numeric(u)))
dx <- sapply(index(Q9z), f)
QP <- cbind(Qw9, d9 = coredata(d9) [dx])
Q9_full <- data.frame(QP)
Q9_full_mod <- Q9_full[which(Q9_full$Q.mod <= 20),]

sm_rating9 <- lm(log(Q.mod) ~ log(d9), Q9_full_mod); summary(sm_rating9)

#ST11D
Q11D <- Q[which(Q$Qstream == "st11L"),]
Q11D <- Q11D[!is.na(Q11D$Q.mod),]
Q11D <- Q11D[order(Q11D$Pd),]
Q11Dz <- with(Q11D, zoo(Q.mod, Pd))

Q7 <- Q[which(Q$Qstream == "st7"),]
Q7 <- Q7[order(Q7$Pd),]
Q7z <- with(Q7, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(temp_11D)) - as.numeric(u)))
ix <- vapply(index(Q11Dz), f, integer(1))
QP <- cbind(Q11D, temp_11D = coredata(temp_11D)[ix])
Qw11D <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(d11D)) - as.numeric(u)))
dx <- sapply(index(Q11Dz), f)
QP <- cbind(Qw11D, d11D = coredata(d11D) [dx])
Q11D_full <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(dL)) - as.numeric(u)))
dx <- sapply(index(Q11Dz), f)
QP <- cbind(Q11D_full, dL = coredata(dL) [dx])
Q11D_full <- data.frame(QP)
Q11D_full_mod <- Q11D_full[!is.na(Q11D_full$d11D),]

sm_rating11D <- lm(log(Q.mod) ~ log(d11D) + log(dL), Q11D_full_mod); summary(sm_rating11D)

#ST11U

Q11U <- Q[which(Q$Qstream == "st11U"),]
Q11U <- Q11U[!is.na(Q11U$Q.mod),]
Q11U <- Q11U[order(Q11U$Pd),]
Q11Uz <- with(Q11U, zoo(Q.mod, Pd))

Q17 <- Q[which(Q$Qstream == "st17"),]
Q17 <- Q17[!is.na(Q17$Q.mod),]
Q17 <- Q17[order(Q17$Pd),]
Q17z <- with(Q17, zoo(Q.mod, Pd))


f <- function(u) which.min(abs(as.numeric(index(d17)) - as.numeric(u)))
dx <- sapply(index(Q11Uz), f)
QP <- cbind(Q11Uz, d17 = coredata(d17) [dx])
Q11U_full <- data.frame(QP)
Q11U_full_mod <- Q11U_full[!is.na(Q11U_full$d17),]

sm_rating11U <- lm(log(Q11Uz) ~ log(d17), Q11U_full); summary(sm_rating11U)

#ST13

Q13 <- Q[which(Q$Qstream == "st13"),]
Q13 <- Q13[!is.na(Q13$Q.mod),]
Q13 <- Q13[order(Q13$Pd),]
Q13z <- with(Q13, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(temp_13)) - as.numeric(u)))
ix <- vapply(index(Q13z), f, integer(1))
QP <- cbind(Q13, temp_13 = coredata(temp_13)[ix])
Qw13 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(d13)) - as.numeric(u)))
dx <- sapply(index(Q13z), f)
QP <- cbind(Qw13, d13 = coredata(d13) [dx])
Q13_full <- data.frame(QP)

sm_rating13 <- lm(log(Q.mod) ~ log(d13), Q13_full); summary(sm_rating13)

#ST14
Q14 <- Q[which(Q$Qstream == "st14"),]
Q14 <- Q14[!is.na(Q14$Q.mod),]
Q14 <- Q14[order(Q14$Pd),]
Q14z <- with(Q14, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(temp_14)) - as.numeric(u)))
ix <- vapply(index(Q14z), f, integer(1))
QP <- cbind(Q14, temp_14 = coredata(temp_14)[ix])
Qw14 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(d14)) - as.numeric(u)))
dx <- sapply(index(Q14z), f)
QP <- cbind(Qw14, d14 = coredata(d14) [dx])
Q14_full <- data.frame(QP)

sm_rating14 <- lm(log(Q.mod) ~ log(d14), Q14_full); summary(sm_rating14)

#ST17
Q17 <- Q[which(Q$Qstream == "st17"),]
Q17 <- Q17[!is.na(Q17$Q.mod),]
Q17 <- Q17[order(Q17$Pd),]
Q17z <- with(Q17, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(temp_17)) - as.numeric(u)))
ix <- vapply(index(Q17z), f, integer(1))
QP <- cbind(Q17, temp_17 = coredata(temp_17)[ix])
Qw17 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(d17)) - as.numeric(u)))
dx <- sapply(index(Q17z), f)
QP <- cbind(Qw17, d17 = coredata(d17) [dx])
Q17_full <- data.frame(QP)
Q17_full_mod <- Q17_full[!is.na(Q17_full$d17),]

sm_rating17_mod <- lm(log(Q.mod) ~ log(d17), Q17_full_mod); summary(sm_rating17_mod)

#HVER

QHver <- Q[which(Q$Qstream == "hver"),]
QHver <- QHver[!is.na(QHver$Q.mod),]
QHver <- QHver[order(QHver$Pd),]
QHverz <- with(QHver, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(temp_Hver)) - as.numeric(u)))
ix <- vapply(index(QHverz), f, integer(1))
QP <- cbind(QHver, temp_Hver = coredata(temp_Hver)[ix])
QwHver <- data.frame(QP)		

f <- function(u) which.min(abs(as.numeric(index(dHver)) - as.numeric(u)))
dx <- sapply(index(QHverz), f)
QP <- cbind(QwHver, dHver = coredata(dHver) [dx])
QHver_full <- data.frame(QP)
QHver_full_mod <- QHver_full[!is.na(QHver_full$dHver),]

sm_ratingHver <- lm(log(Q.mod) ~ log(dHver) * temp_Hver, QHver_full_mod); summary(sm_ratingHver)

###########################End of stream import#########################################################

################################Building the final discharge file###################

depths$Q1 <- exp(predict(sm_rating1, depths))
depths$Q5 <- exp(predict(sm_rating5_mod, depths))
depths$Q6 <- exp(predict(sm_rating6, depths))
depths$Q8 <- exp(predict(sm_rating8, depths))
depths$Q9 <- exp(predict(sm_rating9, depths))
depths$Q11D <- exp(predict(sm_rating11D, depths))
depths$Q11U <- exp(predict(sm_rating11U, depths))
depths$Q13 <- exp(predict(sm_rating13, depths))
depths$Q14 <- exp(predict(sm_rating14, depths))
depths$Q17 <- exp(predict(sm_rating17, depths))
depths$QHVER <- exp(predict(sm_ratingHver, depths))

                  

write.csv(Q_all, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Stream Discharge/All Q/Q_all_fin.csv")
	

		
	#adding Q predicted estimates to Qw file	
st7_Qw2 <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw2.csv")

st7_Qw2$Q_DS_predicted <- exp(predict(sm_rating, st7_Qw2))

write.csv(st7_Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw3.csv")
	
#################################################################################################################################################################
#Working Log
#Feb-6-2015. added full date-time file and merged to get 15min intervals from Jul 2010