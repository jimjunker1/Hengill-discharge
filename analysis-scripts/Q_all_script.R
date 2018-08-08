#Purpose: To create pressure-discharge rating curves across gradient streams
#Original code for st7 by Jim Hood
#Modified by Jim Junker. See log


##################### Change computer clock to Iceland time UCT  ############################

#load libraries
library(plyr)
library(tidyverse)
library(chron)
library(gridExtra)
library(zoo)
library(scales)
library(GGally)
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
pres11L <- read.table("./stream-data/9736055_ST11L.txt", header = T, sep = "\t", quote="")
pres11U <- read.table("./stream-data/2451129_ST11U.txt", header = T, sep = "\t", quote="")
pres13 <- read.table("./stream-data/9736053_ST13.txt", header = T, sep = "\t", quote="")
pres14 <- read.table("./stream-data/9736060_ST14.txt", header = T, sep = "\t", quote="")
pres17 <- read.table("./stream-data/9736062_ST17.txt", header = T, sep = "\t", quote="")
presHver <- read.table("./stream-data/2451126_Hver.txt", header = T, sep = "\t", quote="")

pres1 <- pres1[,1:4]
### bring in modeled light 
#source("./analysis-scripts/ModelingLight.R")
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
pres11L$Pd <- as.POSIXct(paste(pres11L$Date, pres11L$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
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
pres11L = pres11L[,c(5,3:4)]
pres11U = pres11U[,c(5,3:4)]
pres13 = pres13[,c(5,3:4)]
pres14 = pres14[,c(5,3:4)]
pres17 = pres17[,c(5,3:4)]
presHver = presHver[,c(5,3:4)]

### this for all the streams to get a single large df of all pressures
mylist = list(pres1, pres5, pres6, presL, presH, pres8, pres9, pres11L,
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
#lightmodd_d = aggregate(lightmodhr_d['light.est'], list(Pd = cut(lightmodhr_d$Pd, breaks = "day")),
#                        sum, na.rm = T)
#colnames(lightmodd_d)[2] = "light.day"
#lightmodd_d$day = as.factor(as.POSIXct(lightmodd_d$Pd, format = "%Y-%m-%d"))

#pres_allhr$day = as.factor(format(as.POSIXct(pres_allhr$Pd, format = "%Y-%m-%d %H:%M:%S"),format = "%Y-%m-%d"))
#pres_allhr = merge(pres_allhr, lightmodd_d[c("day","light.day")], by = "day")

#pres_allhr[c("day")] = NULL;pres_allhr = pres_allhr[order(pres_allhr$Pd),]
#####################################################################################################################

############################################################################################
#####################Build discharge rating curves for all streams#############################################################
#load data
#load(	"~/Projects/Iceland/Temp-Disch-Light/Stream Discharge/All Q/all_streams.RData")

#names(depths) <- c("time", "dL", "dH", "d1", "d5", "d6", "d8", "d9", "d11L", "d11U", "d13", "d14", "d17", "dHver")
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
st11L_tempC <- with(pres_allhr, zoo(st11L_tempC, Pd))
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
st11L_depthm <- with(pres_allhr, zoo(st11L_depthm, Pd))
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

Q1_full_mod = Q1_full[-c(7:9),]

ggplot(Q1_full_mod, aes(y = Q.mod, x = st1_depthm)) + geom_point(size = 2)

sm_rating1 = lm(Q.mod~st1_depthm, Q1_full_mod);summary(sm_rating1)
sm_seg1 = segmented::segmented(sm_rating1, seg.Z = ~st1_depthm)
plot(sm_seg1);summary(sm_seg1)
segmented::slope(sm_seg1)
##### model to see where it goes negative ####
#x = data.frame( st1_depthm = seq(min(pres_allhr$st1_depthm, na.rm = T), max(pres_allhr$st1_depthm, na.rm = T), 0.005))
#x$y = predict(sm_rating1, x)

#plot(y~st1_depthm, data = x);abline(a =0,b=0)
#max(x$y)
####

Q1_full_mod$fitted = fitted(sm_seg1)
ggplot(Q1_full_mod, aes(x = fitted, y = Q.mod)) + geom_point(size = 5) +
  geom_abline(intercept = 0, slope = 1)

#pres_allhr$st1_Q = predict(sm_rating1, pres_allhr)

pres_allhr$st1_Q = NA

depths_seg = which(st1_depthm >= 0.35125)
depths_est = which(st1_depthm < 0.35125)

pres_allhr[depths_seg, 'st1_Q'] = predict(sm_seg1, pres_allhr[depths_seg,])
pres_allhr[depths_est, 'st1_Q'] = (22.86*pres_allhr[depths_est,'st1_depthm']) 

ggplot(pres_allhr, aes(x = Pd, y = st1_Q)) + geom_point(size = 3) +coord_cartesian(ylim = c(0,150))
max(pres_allhr$st1_Q, na.rm =T)
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

ggplot(pres_allhr, aes(x = Pd, y = st6_Q)) + geom_point(size = 3)

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
ggplot(Q8_full_mod, aes(y = st8_depthm, x = Q.mod)) + geom_point(size = 2)
sm_rating8 <- lm(log(Q.mod) ~ log(st8_depthm) + st8_tempC, Q8_full_mod); summary(sm_rating8)
mean(Q8_full_mod$Q.mod, na.rm = T);sd(Q8_full_mod$Q.mod, na.rm = T)
pres_allhr$st8_Q = exp(predict(sm_rating8, pres_allhr))

ggplot(pres_allhr, aes(x = Pd, y = st8_Q)) + geom_point(size = 3)
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
Q9_full_mod = Q9_full[which(is.na(Q9_full$st9_depthm) == F),]

sm_rating9 <- lm(log(Q.mod) ~ log(st9_depthm) + st9_tempC  , Q9_full_mod); summary(sm_rating9)

pres_allhr$st9_Q = exp(predict(sm_rating9, pres_allhr))

ggplot(pres_allhr, aes(x = Pd, y = st9_Q)) + geom_point(size = 3)

#ST11L
Q11L <- Q[which(Q$Qstream == "st11L"),]
Q11L <- Q11L[!is.na(Q11L$Q.mod),]
Q11L <- Q11L[order(Q11L$Pd),]
Q11Lz <- with(Q11L, zoo(Q.mod, Pd))

Q7 <- Q[which(Q$Qstream == "st7"),]
Q7 <- Q7[order(Q7$Pd),]
Q7z <- with(Q7, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(st11L_tempC)) - as.numeric(u)))
ix <- vapply(index(Q11Lz), f, integer(1))
QP <- cbind(Q11L, st11L_tempC = coredata(st11L_tempC)[ix])
Qw11L <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st11L_depthm)) - as.numeric(u)))
dx <- vapply(index(Q11Lz), f, integer(1))
QP <- cbind(Qw11L, st11L_depthm = coredata(st11L_depthm) [dx])
Q11L_full <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st7L_depthm)) - as.numeric(u)))
dx <- vapply(index(Q11Lz), f, integer(1))
QP <- cbind(Q11L_full, st7L_depthm = coredata(st7L_depthm) [dx])
Q11L_full <- data.frame(QP)

Q11L_full_mod <- Q11L_full[!is.na(Q11L_full$st11L_depthm),]
Q11L_full_mod = Q11L_full_mod[-1,]

sm_rating11L <- lm(log(Q.mod) ~ log(st11L_depthm), Q11L_full_mod); summary(sm_rating11L)

ggplot(Q11L_full_mod, aes(Q.mod, x = st11L_depthm)) +geom_point(size = 2)

pres_allhr$st11L_Q = exp(predict(sm_rating11L, pres_allhr))
Q.fix  = which(pres_allhr$st11L_Q >= 10000)
pres_allhr[Q.fix, "st11L_Q"] = NA

ggplot(pres_allhr, aes(x = Pd, y = st11L_Q)) + geom_point(size = 3)

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
dx <- vapply(index(Q11Uz), f, integer(1))
QP <- cbind(Q11U, st11U_depthm = coredata(st11U_depthm) [dx])
Q11U_full <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st17_depthm)) - as.numeric(u)))
dx <- vapply(index(Q11Uz), f, integer(1))
QP <- cbind(Q11U_full, st17_depthm = coredata(st17_depthm) [dx])
Q11U_full <- data.frame(QP)

Q11U_full_mod <- Q11U_full[!is.na(Q11U_full$st11U_depthm),]

sm_rating11U <- lm(log(Q.mod) ~ log(st11U_depthm), Q11U_full_mod); summary(sm_rating11U)

Q11U_full_mod2 = Q11U_full[is.na(Q11U_full$st11U_depthm) & !is.na(Q11U_full$st17_depthm),]

sm_rating11U2 = lm(log(Q.mod)~ log(st17_depthm), Q11U_full_mod2);summary(sm_rating11U2)

pres_allhr$st11U_Q = NA

depths_d11 = which(!is.na(pres_allhr$st11U_depthm))
depths_d17 = which(is.na(pres_allhr$st11U_depthm) & !is.na(pres_allhr$st17_depthm))

pres_allhr[depths_d11, "st11U_Q"] = exp(predict(sm_rating11U, pres_allhr[depths_d11,]))
pres_allhr[depths_d17, "st11U_Q"] = exp(predict(sm_rating11U2, pres_allhr[depths_d17,]))                   
                   
ggplot(pres_allhr, aes(x = Pd, y = st11U_Q)) + geom_point(size = 3)

Q.fix = which(pres_allhr$st11U_Q >= 1000)
pres_allhr[Q.fix, "st11U_Q"] = NA
#ST13

######## For ST13 use the Q.mod for the measures ######
Q13 <- Q[which(Q$Qstream == "st13"),]
Q13_full <- Q13[!is.na(Q13$Q.mod),]

hist(Q13_full$Q.mod)
median(Q13_full$Q.mod, na.rm = T) 
mean(Q13_full$Q.mod, na.rm = T) 
sd(Q13_full$Q.mod, na.rm = T)

sd(Q13_full$Q.mod)/mean(Q13_full$Q.mod) 

pres_allhr$st13_Q = "14.6 +/-1.6*"
###### full code in Rating-Curve-Scripts folder if needed #####
#ST14
Q14 <- Q[which(Q$Qstream == "st14"),]
Q14 <- Q14[!is.na(Q14$Q.mod),]
Q14 <- Q14[order(Q14$Pd),]
Q14z <- with(Q14, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(st14_tempC)) - as.numeric(u)))
ix <- vapply(index(Q14z), f, integer(1))
QP <- cbind(Q14, st14_tempC = coredata(st14_tempC)[ix])
Qw14 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st14_depthm)) - as.numeric(u)))
dx <- vapply(index(Q14z), f, integer(1))
QP <- cbind(Qw14, st14_depthm = coredata(st14_depthm) [dx])
Q14_full <- data.frame(QP)

sm_rating14 <- lm(log(Q.mod) ~ log(st14_depthm) * st14_tempC, Q14_full); summary(sm_rating14)

pres_allhr$st14_Q = exp(predict(sm_rating14, pres_allhr))

ggplot(pres_allhr, aes(x = Pd, y = st14_Q)) + geom_point(size = 3)

#ST17
Q17 <- Q[which(Q$Qstream == "st17"),]
Q17 <- Q17[!is.na(Q17$Q.mod),]
Q17 <- Q17[order(Q17$Pd),]
Q17z <- with(Q17, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(st17_tempC)) - as.numeric(u)))
ix <- vapply(index(Q17z), f, integer(1))
QP <- cbind(Q17, st17_tempC = coredata(st17_tempC)[ix])
Qw17 <- data.frame(QP)

f <- function(u) which.min(abs(as.numeric(index(st17_depthm)) - as.numeric(u)))
dx <- vapply(index(Q17z), f, integer(1))
QP <- cbind(Qw17, st17_depthm = coredata(st17_depthm) [dx])
Q17_full <- data.frame(QP)

Q17_full_mod <- Q17_full[!is.na(Q17_full$st17_depthm),]

sm_rating17 <- lm(log(Q.mod) ~ log(st17_depthm), Q17_full_mod); summary(sm_rating17)

pres_allhr$st17_Q = exp(predict(sm_rating17, pres_allhr))

ggplot(pres_allhr, aes(x = Pd, y = st17_Q)) + geom_point(size = 3)

st17.fix = which(pres_allhr$st17_Q >= 30000)
pres_allhr[st17.fix, "st17_Q"] = NA
#HVER

QHver <- Q[which(Q$Qstream == "hver"),]
QHver <- QHver[!is.na(QHver$Q.mod),]
QHver <- QHver[order(QHver$Pd),]
QHverz <- with(QHver, zoo(Q.mod, Pd))

f <-  function(u) which.min(abs(as.numeric(index(Hver_tempC)) - as.numeric(u)))
ix <- vapply(index(QHverz), f, integer(1))
QP <- cbind(QHver, Hver_tempC = coredata(Hver_tempC)[ix])
QwHver <- data.frame(QP)		

f <- function(u) which.min(abs(as.numeric(index(Hver_depthm)) - as.numeric(u)))
dx <- vapply(index(QHverz), f, integer(1))
QP <- cbind(QwHver, Hver_depthm = coredata(Hver_depthm) [dx])
QHver_full <- data.frame(QP)

QHver_full <- QHver_full[!is.na(QHver_full$Hver_depthm),]

sm_ratingHver <- lm(log(Q.mod) ~ log(Hver_depthm) + Hver_tempC, QHver_full); summary(sm_ratingHver)

pres_allhr$Hver_Q = exp(predict(sm_ratingHver, pres_allhr))

ggplot(pres_allhr, aes(x = Pd, y = Hver_Q)) + geom_point(size = 3)

###########################End of stream import#########################################################

################################Building the final discharge file###################

write.csv(pres_allhr, file = "./output-files/Q_all_fin.csv", row.names = F)

rm(list = ls()[!ls() %in% c("pres_allhr", "Q")])
#################################################################################################################################################################
#Working Log
#Feb-6-2015. added full date-time file and merged to get 15min intervals from Jul 2010



