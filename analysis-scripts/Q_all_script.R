#Purpose: To create pressure-discharge rating curves across gradient streams
#Original code for st7 by Jim Hood
#Modified by Jim Junker. See log


##################### Change computer clock to Iceland time UCT  ############################

#load libraries
library(tidyverse)
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

#########################load data from all streams###############################

#datetime <- read.csv("./stream-data/All_DateTime.csv")

Q <- read.csv("./stream-data/Q_data_summary_working.csv")
presL <- read.csv("./stream-data/9736059_7LO.csv")
presH <- read.csv("./stream-data/9736163_7HI_noNAs.csv")
pres1 <- read.table("./stream-data/9736056_ST1b.txt", header = T, sep = "\t", quote="")
pres5 <- read.table("./stream-data/9736061_ST5.txt", header = T, sep = "\t", quote="")
pres6 <- read.table("./stream-data/9736057_ST6.txt", header = T, sep = "\t", quote="")
pres8 <- read.table("./stream-data/9736058_ST8.txt", header = T, sep = "\t", quote="")
pres9 <- read.table("./stream-data/9736054_ST9.txt", header = T, sep = "\t", quote="")
pres11D <- read.table("./stream-data/9736055_ST11L.txt", header = T, sep = "\t", quote="")
pres11U <- read.table("./stream-data/2451129_ST11U.txt", header = T, sep = "\t", quote="")
pres13 <- read.table("./stream-data/9736053_ST13.txt", header = T, sep = "\t", quote="")
pres14 <- read.table("./stream-data/9736060_ST14.txt", header = T, sep = "\t", quote="")
pres17 <- read.table("./stream-data/9736062_ST17.txt", header = T, sep = "\t", quote="")
presHver <- read.table("./stream-data/2451126_Hver.txt", header = T, sep = "\t", quote="")

pres1 <- pres1[,1:4]
### bring in modeled light 
source("./analysis-scripts/ModelingLight.R")
lightmod <- read.csv("./output-files/light-est_full.csv")

lightmod$Pd <- as.POSIXct(lightmod$hour,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

lightmodhr_d <- data.frame(lightmod$Pd, lightmod$light)

names(lightmodhr_d) <- c("Pd", "light.est")

lightmodhr_d$light_year <- as.numeric(format(lightmodhr_d$Pd, "%Y"))

lightmodhr_d$cum.light <- ave(lightmodhr_d$light.est, lightmodhr_d$light_year, FUN = cumsum)
lightmodhr_d = lightmodhr_d[,c(1:2,4)]

####
#####
#datetime$Pd <- as.POSIXct(paste(datetime$Date, datetime$Time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
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

### next remove all the 'Date', 'Time' columns so can join into one.
#datetime = datetime[,3]
presL = presL[,c(5,3:4)]
presH = presH[,c(5,3:4)]
pres1 = pres1[,c(5,3:4)]
pres5 = pres5[,c(5,3:4)]
pres6 = pres6[,c(5,3:4)]
pres8 = pres8[,c(5,3:4)]
pres9 = pres9[,c(5,3:4)]
pres11D = pres11D[,c(5,3:4)]
pres11U = pres11U[,c(5,3:4)]
pres13 = pres13[,c(5,3:4)]
pres14 = pres14[,c(5,3:4)]
pres17 = pres17[,c(5,3:4)]
presHver = presHver[,c(5,3:4)]

pres1[which(as.numeric(format(pres1$Pd, "%y")) == 1),]
pres5[which(as.numeric(format(pres5$Pd, "%y")) == 1),]
pres6[which(as.numeric(format(pres6$Pd, "%y")) == 1),]
presH[which(as.numeric(format(presH$Pd, "%y")) == 1),]
presL[which(as.numeric(format(presL$Pd, "%y")) == 1),]
pres8[which(as.numeric(format(pres8$Pd, "%y")) == 1),]
pres9[which(as.numeric(format(pres9$Pd, "%y")) == 1),]
pres11D[which(as.numeric(format(pres11D$Pd, "%y")) == 1),]
pres11U[which(as.numeric(format(pres11U$Pd, "%y")) == 1),]
pres13[which(as.numeric(format(pres13$Pd, "%y")) == 1),]
pres14[which(as.numeric(format(pres14$Pd, "%y")) == 1),]
pres17[which(as.numeric(format(pres17$Pd, "%y")) == 1),]
presHver[which(as.numeric(format(presHver$Pd, "%y")) == 1),]

### this for all the streams to get a single large df of all pressures
mylist = list(pres1, pres5, pres6, presL, presH, pres8, pres9, pres11D,
            pres11U, pres13, pres14, pres17, presHver)

pres_all = Reduce(function(df1, df2) merge(df1, df2, by = "Pd", all = T), mylist)

#pres_all= pres_all[,c(1,4:dim(pres_all)[2])]
colnames(pres_all) = c("Pd", "st1_tempC", "st1_depthm", "st5_tempC", "st5_depthm", 
                       "st6_tempC", "st6_depthm", "L_tempC", "L_depthm", "H_tempC",
                       "H_depthm", "st8_tempC", "st8_depthm", "st9_tempC", "st9_depthm",
                       "st11L_tempC", "st11L_depthm", "st11U_tempC", "st11U_depthm",
                       "st13_tempC", "st13_depthm", "st14_tempC", "st14_depthm",
                       "st17_tempC", "st17_depthm", "Hver_tempC", "Hver_depthm")
rm(mylist)
### 
pres_all = pres_all[-which(as.numeric(format(pres_all$Pd, "%y")) == "1"),]
#get rid of weird 2001 year
## 

pres_allhr = aggregate(pres_all[c("st1_tempC", "st1_depthm", "st5_tempC", "st5_depthm", 
                                  "st6_tempC", "st6_depthm", "L_tempC", "L_depthm", "H_tempC",
                                  "H_depthm", "st8_tempC", "st8_depthm", "st9_tempC", "st9_depthm",
                                  "st11L_tempC", "st11L_depthm", "st11U_tempC", "st11U_depthm",
                                  "st13_tempC", "st13_depthm", "st14_tempC", "st14_depthm",
                                  "st17_tempC", "st17_depthm", "Hver_tempC", "Hver_depthm")], 
                       list(Pd = cut(pres_all$Pd, breaks = "hour")),
                       mean, na.rm = T)
pres_allhr$Pd = as.POSIXct(pres_allhr$Pd,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#merge with hourly light
pres_allhr = merge(pres_allhr, lightmodhr_d, by = "Pd", all = T)
#convert times to posix object

#####################################################################################################################

############################################################################################
#####################Build discharge rating curves for all streams#############################################################
#load data
#load(	"~/Projects/Iceland/Temp-Disch-Light/Stream Discharge/All Q/all_streams.RData")

#names(depths) <- c("time", "dL", "dH", "d1", "d5", "d6", "d8", "d9", "d11D", "d11U", "d13", "d14", "d17", "dHver")
Q <- read.csv("./stream-data/Q_data_summary_working.csv")
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

# RUNNING ONE TIME, THEN EXPORTING TO GET PT TEMPS
# #get temp at times of slugs
# #create zoo objects

st7L_tempC <- with(pres_allhr, zoo(L_tempC, Pd))
st7H_tempC <- with(pres_allhr, zoo(H_tempC, Pd))
st1_tempC <- with(pres_allhr, zoo(st1_tempC, Pd))
st5_tempC <- with(pres_allhr, zoo(st5_tempC, Pd))
st6_tempC <- with(pres_allhr, zoo(st6_tempC, Pd))
st8_tempC <- with(pres_allhr, zoo(st8_tempC, Pd))
st9_tempC <- with(pres_allhr, zoo(st9_tempC, Pd))
st11D_tempC <- with(pres_allhr, zoo(st11L_tempC, Pd))
st11U_tempC <- with(pres_allhr, zoo(st11U_tempC, Pd))
st13_tempC <- with(pres_allhr, zoo(st13_tempC, Pd))
st14_tempC <- with(pres_allhr, zoo(st14_tempC, Pd))
st17_tempC <- with(pres_allhr, zoo(st17_tempC, Pd))
Hver_tempC <- with(pres_allhr, zoo(Hver_tempC, Pd))
lightmod <- with(pres_allhr, zoo(cum.light, Pd))


st1_depthm <- with(pres_allhr, zoo(st1_depthm, Pd))
st7L_depthm <- with(pres_allhr, zoo(L_depthm, Pd))
st7H_depthm <- with(pres_allhr, zoo(H_depthm, Pd))
st5_depthm <- with(pres_allhr, zoo(st5_depthm, Pd))
st6_depthm <- with(pres_allhr, zoo(st6_depthm, Pd))
st8_depthm <- with(pres_allhr, zoo(st8_depthm, Pd))
st9_depthm <- with(pres_allhr, zoo(st9_depthm, Pd))
st11D_depthm <- with(pres_allhr, zoo(st11L_depthm, Pd))
st11U_depthm <- with(pres_allhr, zoo(st11U_depthm, Pd))
st13_depthm <- with(pres_allhr, zoo(st13_depthm, Pd))
st14_depthm <- with(pres_allhr, zoo(st14_depthm, Pd))
st17_depthm <- with(pres_allhr, zoo(st17_depthm, Pd))
Hver_depthm <- with(pres_allhr, zoo(Hver_depthm, Pd))

##  merging the Q and PT depth data

#ST1
Q1 <- Q[which(Q$Qstream == "st1"),]
Q1 <- Q1[order(Q1$Pd),]
Q1z <- with(Q1, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(st1_tempC)) - as.numeric(u)))
ix <- vapply(index(Q1z), f, integer(1))
QP <- cbind(Q1, st1_tempC = coredata(st1_tempC)[ix])
Qw1 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st1_depthm)) - as.numeric(u)))
dx <- vapply(index(Q1z), f, integer(1))
QP <- cbind(Qw1, st1_depthm = coredata(st1_depthm) [dx])
Q1_full <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(lightmod)) - as.numeric(u)))
dx <- vapply(index(Q1z), f, integer(1))
QP <- cbind(Q1_full, cum.light = coredata(lightmod) [dx])
Q1_full <- data.frame(QP)

summer1 <- 	ifelse(as.numeric(format(Q1_full$Pd, "%m")) >= 6 & as.numeric(format(Q1_full$Pd, "%m")) <= 9, 1, 0)
Q1_full <- cbind(Q1_full, summer1)

Q1_full_mod = Q1_full[-9,]

sm_rating1 <- lm(log(Q.mod) ~ log(st1_depthm) + cum.light + I(cum.light^2), Q1_full_mod); summary(sm_rating1)

pres_allhr$summer1 <- as.factor(ifelse(as.numeric(format(pres_allhr$Pd, "%m")) >= 6 & as.numeric(format(pres_allhr$time, "%m")) <= 10, 1, 0))

pres_allhr$st1_Q = exp(predict(sm_rating1, pres_allhr))

pres_allhr$summer1 = NULL

ggplot(pres_allhr, aes(x = Pd, y = st1_Q)) + geom_point(size = 3)
#ST5
Q5 <- Q[which(Q$Qstream == "st5"),]
Q5 <- Q5[!is.na(Q5$Q.mod),]
Q5 <- Q5[order(Q5$Pd),]
Q5z <- with(Q5, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(st5_tempC)) - as.numeric(u)))
ix <- vapply(index(Q5z), f, integer(1))
QP <- cbind(Q5, st5_tempC = coredata(st5_tempC)[ix])
Qw5 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st5_depthm)) - as.numeric(u)))
dx <- vapply(index(Q5z), f, integer(1))
QP <- cbind(Qw5, st5_depthm = coredata(st5_depthm) [dx])
Q5_full <- data.frame(QP)

Q5_full_mod <- Q5_full[-6,]
sm_rating5_mod <- lm(log(Q.mod) ~ log(st5_depthm), Q5_full_mod); summary(sm_rating5_mod)

pres_allhr$st5_Q = exp(predict(sm_rating5_mod, pres_allhr))
ggplot(pres_allhr, aes(x = Pd, y = st5_Q)) + geom_point(size = 3)

#ST6
Q6 <- Q[which(Q$Qstream == "st6"),]
Q6 <- Q6[!is.na(Q6$Q.mod),]
Q6 <- Q6[order(Q6$Pd),]
Q6z <- with(Q6, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(st6_tempC)) - as.numeric(u)))
ix <- vapply(index(Q6z), f, integer(1))
QP <- cbind(Q6, st6_tempC = coredata(st6_tempC)[ix])
Qw6 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st6_depthm)) - as.numeric(u)))
dx <- vapply(index(Q6z), f, integer(1))
QP <- cbind(Qw6, st6_depthm = coredata(st6_depthm) [dx])
Q6_full <- data.frame(QP)

Q6_full_mod <- Q6_full[!is.na(Q6_full$Q.mod),]
sm_rating6 <- lm(log(Q.mod) ~ log(st6_depthm), Q6_full_mod); summary(sm_rating6)

pres_allhr$st6_Q = exp(predict(sm_rating6, pres_allhr))
#ST7

#ST8

Q8 <- Q[which(Q$Qstream == "st8"),]
Q8 <- Q8[!is.na(Q8$Q.mod),]
Q8 <- Q8[order(Q8$Pd),]
Q8z <- with(Q8, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(st8_tempC)) - as.numeric(u)))
ix <- vapply(index(Q8z), f, integer(1))
QP <- cbind(Q8, st8_tempC = coredata(st8_tempC)[ix])
Qw8 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st8_depthm)) - as.numeric(u)))
dx <- vapply(index(Q8z), f, integer(1))
QP <- cbind(Qw8, st8_depthm = coredata(st8_depthm) [dx])
Q8_full <- data.frame(QP)
Q8_full_mod <- Q8_full[which(Q8_full$Q.mod <= 40),]

sm_rating8 <- lm(log(Q.mod) ~ log(st8_depthm) + st8_tempC, Q8_full_mod); summary(sm_rating8)

pres_allhr$st8_Q = exp(predict(sm_rating8, pres_allhr))

#ST9
Q9 <- Q[which(Q$Qstream == "st9"),]
Q9 <- Q9[!is.na(Q9$Q.mod),]
Q9 <- Q9[order(Q9$Pd),]
Q9z <- with(Q9, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(st9_tempC)) - as.numeric(u)))
ix <- vapply(index(Q9z), f, integer(1))
QP <- cbind(Q9, st9_tempC = coredata(st9_tempC)[ix])
Qw9 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st9_depthm)) - as.numeric(u)))
dx <- vapply(index(Q9z), f, integer(1))
QP <- cbind(Qw9, st9_depthm = coredata(st9_depthm) [dx])
Q9_full <- data.frame(QP)

Q9_full[which(Q9_full$Q.mod >= 20),"Q.mod"] = Q9_full[which(Q9_full$Q.mod >= 20),"Q_US"]
Q9_full_mod = Q9_full[which(is.na(Q9_full$d9) == F),]

sm_rating9 <- lm(log(Q.mod) ~ log(st9_depthm) + st9_tempC  , Q9_full_mod); summary(sm_rating9)

pres_allhr$st9_Q = exp(predict(sm_rating9, pres_allhr))
#ST11D
Q11D <- Q[which(Q$Qstream == "st11L"),]
Q11D <- Q11D[!is.na(Q11D$Q.mod),]
Q11D <- Q11D[order(Q11D$Pd),]
Q11Dz <- with(Q11D, zoo(Q.mod, Pd))

Q7 <- Q[which(Q$Qstream == "st7"),]
Q7 <- Q7[order(Q7$Pd),]
Q7z <- with(Q7, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(st11D_tempC)) - as.numeric(u)))
ix <- vapply(index(st11D_tempC), f, integer(1))
QP <- cbind(Q11D, st11D_tempC = coredata(st11D_tempC)[ix])
Qw11D <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st11D_depthm)) - as.numeric(u)))
dx <- sapply(index(Q11Dz), f)
QP <- cbind(Qw11D, st11D_depthm = coredata(st11D_depthm) [dx])
Q11D_full <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(L_depthm)) - as.numeric(u)))
dx <- sapply(index(Q11Dz), f)
QP <- cbind(Q11D_full, L_depthm = coredata(L_dpethm) [dx])
Q11D_full <- data.frame(QP)

Q11D_full_mod <- Q11D_full[!is.na(Q11D_full$st11D_depthm),]

sm_rating11D <- lm(log(Q.mod) ~ log(st11D_depthm) + log(L_depthm), Q11D_full_mod); summary(sm_rating11D)

#ST11U

Q11U <- Q[which(Q$Qstream == "st11U"),]
Q11U <- Q11U[!is.na(Q11U$Q.mod),]
Q11U <- Q11U[order(Q11U$Pd),]
Q11Uz <- with(Q11U, zoo(Q.mod, Pd))

Q17 <- Q[which(Q$Qstream == "st17"),]
Q17 <- Q17[!is.na(Q17$Q.mod),]
Q17 <- Q17[order(Q17$Pd),]
Q17z <- with(Q17, zoo(Q.mod, Pd))

f <- function(u) which.min(abs(as.numeric(index(st11U_depthm)) - as.numeric(u)))
dx <- sapply(index(Q11Uz), f)
QP <- cbind(Q11Uz, d17 = coredata(pres_all$st11U_depthm) [dx])
Q11U_full <- data.frame(QP)
Q11U_full_mod <- Q11U_full[!is.na(Q11U_full$d17),]

sm_rating11U <- lm(log(Q11Uz) ~ log(d17), Q11U_full); summary(sm_rating11U)

#ST13

Q13 <- Q[which(Q$Qstream == "st13"),]
Q13 <- Q13[!is.na(Q13$Q.mod),]
Q13 <- Q13[order(Q13$Pd),]
Q13z <- with(Q13, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(pres_all$st13_tempC)) - as.numeric(u)))
ix <- vapply(index(Q13z), f, integer(1))
QP <- cbind(Q13, temp_13 = coredata(temp_13)[ix])
Qw13 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(pres_all$st13_depthm)) - as.numeric(u)))
dx <- sapply(index(Q13z), f)
QP <- cbind(Qw13, st13_depthm = coredata(st13_depthm) [dx])
Q13_full <- data.frame(QP)

sm_rating13 <- lm(log(Q.mod) ~ log(st13_depthm), Q13_full); summary(sm_rating13)

#ST14
Q14 <- Q[which(Q$Qstream == "st14"),]
Q14 <- Q14[!is.na(Q14$Q.mod),]
Q14 <- Q14[order(Q14$Pd),]
Q14z <- with(Q14, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(pres_all$st14_tempC)) - as.numeric(u)))
ix <- vapply(index(Q14z), f, integer(1))
QP <- cbind(Q14, temp_14 = coredata(temp_14)[ix])
Qw14 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(pres_all$st14_depthm)) - as.numeric(u)))
dx <- sapply(index(Q14z), f)
QP <- cbind(Qw14, st14_depthm = coredata(st14_depthm) [dx])
Q14_full <- data.frame(QP)

sm_rating14 <- lm(log(Q.mod) ~ log(st14_depthm), Q14_full); summary(sm_rating14)

#ST17
Q17 <- Q[which(Q$Qstream == "st17"),]
Q17 <- Q17[!is.na(Q17$Q.mod),]
Q17 <- Q17[order(Q17$Pd),]
Q17z <- with(Q17, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(pres_all$st17_tempC)) - as.numeric(u)))
ix <- vapply(index(Q17z), f, integer(1))
QP <- cbind(Q17, temp_17 = coredata(temp_17)[ix])
Qw17 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(pres_all$st17_depthm)) - as.numeric(u)))
dx <- sapply(index(Q17z), f)
QP <- cbind(Qw17, st17_depthm = coredata(st17_depthm) [dx])
Q17_full <- data.frame(QP)
Q17_full_mod <- Q17_full[!is.na(Q17_full$st17_depthm),]

sm_rating17_mod <- lm(log(Q.mod) ~ log(d17), Q17_full_mod); summary(sm_rating17_mod)

#HVER

QHver <- which(Q$Qstream == "hver")
QHver <- QHver.fix[!is.na(QHver.fix$Q.mod),]
QHver <- QHver.fix[order(QHver$Pd),]
QHverz <- with(QHver, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(pres_all$Hver_tempC)) - as.numeric(u)))
ix <- vapply(index(Hver_tempC), f, integer(1))
QP <- cbind(QHver, Hver_tempC = coredata(Hver_tempC)[ix])
QwHver <- data.frame(QP)		

f <- function(u) which.min(abs(as.numeric(index(pres_all$Hver_depthm)) - as.numeric(u)))
dx <- sapply(index(QHverz), f)
QP <- cbind(QwHver, Hver_depthm = coredata(Hver_depthm) [dx])
QHver_full <- data.frame(QP)
QHver_full_mod <- QHver_full[!is.na(QHver_full$Hver_depthm),]

sm_ratingHver <- lm(log(Q.mod) ~ log(Hver_depthm) * temp_Hver, QHver_full_mod); summary(sm_ratingHver)

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


write.csv(Q_all, file = "./output-files/Q_all_fin.csv")

	#adding Q predicted estimates to Qw file	
st7_Qw2 <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw2.csv")

st7_Qw2$Q_DS_predicted <- exp(predict(sm_rating, st7_Qw2))

write.csv(st7_Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw3.csv")
	
#################################################################################################################################################################
#Working Log
#Feb-6-2015. added full date-time file and merged to get 15min intervals from Jul 2010


###### Old code. Can clean once df is complete ######
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
#convert to POSIXct
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

#

# RUNNING ONE TIME, THEN EXPORTING TO GET PT TEMPS
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
lightmod <- with(lightmodhr_d, zoo(cum.light, time))


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
head(depths)
