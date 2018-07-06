

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

##pull in data files for calculating Q-width relationships
#load data
datetime <- read.csv("./stream-data/All_DateTime.csv")
Q <- read.csv("./stream-data/Q_data_summary_working.csv")
LWAD <- read.csv("./stream-data/LWADS_summary.csv")
Q_all <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Stream Discharge/All Q/Q_all.csv")

#convert times to posix objects
datetime$Pd <- as.POSIXct(paste(datetime$Date, datetime$Time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
Q$Pd <- as.POSIXct(paste(Q$Qdate), format = "%m/%d/%y", tz = "UTC")
Q_all$Pd <- as.POSIXct(paste(Q_all$Date), format = "%d-%m-%y", tz = "UTC")
LWAD$Pd <- as.POSIXct(paste(LWAD$date), format = "%m/%d/%y", tz = "UTC")


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
Q <- Q[,c(2,4,8,15,17)]
colnames(Q) <- c("Date", "Stream", "tt.s", "day", "Q.mod")


##converting from wide to long all stream Q data
colnames(Q_all) <- c("Date", "Time", "st1", "st5", "st6", "st8", "st9", 
 "st11L", "st13", "st14", "st17", "hver", "Pd")

Q_all <- melt(Q_all, id.vars = c("Date", "Time", "Pd"))

	##must aggregate Q_all by day to allow to match with LWAD measures

colnames(Q_all) <- c("Date", "Time", "Pd", "Stream", "Discharge")
Q_all_ag <- aggregate(Q_all["Discharge"], by = list(day = cut(Q_all$Pd, breaks = "day"),Stream = Q_all$Stream), mean, na.rm = T)

Q_all_ag$Pd <- as.POSIXct(paste(Q_all_ag$day), format = "%Y-%m-%d", tz = "UTC")
Q_all_ag <- Q_all_ag[,c(2:4)]
colnames(Q_all_ag) <- c("Stream", "Discharge", "day")

Q_tt <- merge(Q, Q_all_ag, by = c("Stream", "day"))

	#check to see how the measured Q and predicted Q matchup across all streams

plot(Q_tt$Q.mod, Q_tt$Discharge)
abline(0,1)

	#Now merge in all the LWAD data with discharge data 
LWAD <- LWAD[,c(2,4:6,10,12)]
colnames(LWAD) <- c("Stream", "width", "depth", "area","length", "day")

Q_lwad <- merge(LWAD, Q_all_ag, by = c("Stream", "day"))

##merge together both Q_tt and Q_lwad

Q_LWAD.fin <- merge(Q_lwad, Q_tt, by = c("Stream", "day"), all = T)

NAs <- is.na(Q_LWAD.fin$Q.mod)
Q_LWAD.fin <- cbind(Q_LWAD.fin, NAs)
Q.mod1 <- matrix(NA,nrow(Q_LWAD.fin), ncol=1)
Q.samp <- matrix(0, 1, ncol=1)
for(i in 1:nrow(Q_LWAD.fin)) {
	 if(is.na(Q_LWAD.fin$Q.mod[i]) == "TRUE") {Q.samp <- Q_LWAD.fin$Discharge.x[i] 
		Q.mod1[i] <- Q.samp}

	else{Q.samp <- Q_LWAD.fin$Q.mod[i]
	 	Q.mod1[i] <- Q.samp}
}
Q_LWAD.fin <- cbind(Q_LWAD.fin, Q.mod1)

##Filling in missing q data with Discharge estimate

Q_LWAD.fin <- Q_LWAD.fin[,c(1:6,9,11,13)]

NAs <- is.na(Q_LWAD.fin$Q.mod1)
Q_LWAD.fin <- cbind(Q_LWAD.fin, NAs)
Q.mod <- matrix(NA,nrow(Q_LWAD.fin), ncol=1)
Q.samp <- matrix(0, 1, ncol=1)
for(i in 1:nrow(Q_LWAD.fin)) {
	 if(is.na(Q_LWAD.fin$Q.mod1[i]) == "TRUE") {Q.samp <- Q_LWAD.fin$Discharge.y[i] 
		Q.mod[i] <- Q.samp}

	else{Q.samp <- Q_LWAD.fin$Q.mod1[i]
	 	Q.mod[i] <- Q.samp}
}

Q_LWAD.fin <- cbind(Q_LWAD.fin, Q.mod)

Q_LWAD.fin <- Q_LWAD.fin[,c(1:7,11)]
colnames(Q_LWAD.fin) <- c("Stream", "day", "width", "depth", "area", "length", "tt.s", "Discharge")

##convert discharge to m^3/s

Q_LWAD.fin <- mutate(Q_LWAD.fin, Discharge = Discharge/1000)

## Plots of width, tt, depth, etc with Discharge
	###Width###
Q_width.plot <- ggplot(Q_LWAD.fin, aes(x = log(Discharge), y = log(width))) + geom_point(size = 2.5); Q_width.plot

Q_width.plot + aes(colour = Stream)

Q_width.plot + aes(colour = Stream) + geom_smooth(aes(group = Stream))
dev.new()
	###Travel time####
Q_tt.plot <- ggplot(Q_LWAD.fin, aes(x = log(Discharge), y = tt.s)) + geom_point(size = 2.5); Q_tt.plot

Q_tt.plot + aes(colour = Stream)

Q_tt.plot + aes(colour = Stream) + geom_line(aes(group = Stream))
dev.new()
	###depth###
Q_depth.plot <- ggplot(Q_LWAD.fin, aes(x = Discharge, y = depth)) + geom_point(size = 2.5); Q_depth.plot

Q_depth.plot + aes(colour = Stream)

Q_depth.plot + aes(colour = Stream) + geom_line(aes(group = Stream))

##### Knock them out by stream

st1 <- Q_LWAD.fin[which(Q_LWAD.fin$Stream == "st1"),]
st5 <- Q_LWAD.fin[which(Q_LWAD.fin$Stream == "st5"),]
st6 <- Q_LWAD.fin[which(Q_LWAD.fin$Stream == "st6"),]
st8 <- Q_LWAD.fin[which(Q_LWAD.fin$Stream == "st8"),]
st9 <- Q_LWAD.fin[which(Q_LWAD.fin$Stream == "st9"),]
st11L <- Q_LWAD.fin[which(Q_LWAD.fin$Stream == "st11L"),]
st13 <- Q_LWAD.fin[which(Q_LWAD.fin$Stream == "st13"),]
st14 <- Q_LWAD.fin[which(Q_LWAD.fin$Stream == "st14"),]
st17 <- Q_LWAD.fin[which(Q_LWAD.fin$Stream == "st17"),]
hver <- Q_LWAD.fin[which(Q_LWAD.fin$Stream == "hver"),]

############Modeling width-discharge for all streams########

st1.plot <- ggplot(st1, aes(x = Discharge, y = width)) + geom_point(); st1.plot
st1.lm <- lm(width ~ Discharge + I(Discharge^2), data = st1); summary(st1.lm)
st1.piecelm <- lm(width ~ Discharge*(Discharge < 0.018) + Discharge*(Discharge > 0.018), data = st1); summary(st1.piecelm)

##plotting the piecewise regression over the data

plot(width~Discharge, data = st1, main = "ST1 width-Discharge")
curve((200.04-83) + (44.58+4600)*x, add = T, from = 0, to = 0.018)
curve(200.04 + 44.58 * x, add = T, from = 0.018, to = max(st1$Discharge))
#st1 use split regression for with measurements

st5.plot <- ggplot(st5, aes(x = Discharge, y = width)) + geom_point() + stat_smooth(method = "lm", se = F); st5.plot
st5.lm <- lm(width ~ Discharge, data = st5); summary(st5.lm)
##st5 use regression for width

st6.plot <- ggplot(st6, aes(x = Discharge, y = width)) + geom_point(size = 2.5) + stat_smooth(method = "lm"); st6.plot
st6.lm <- lm(width ~ Discharge, data = st6); summary(st6.lm)
### for st6 use average width for all discharge??
st8.plot <- ggplot(st8, aes(x = Discharge, y = width)) + geom_point(size = 2.5) + stat_smooth(method = "lm"); st8.plot
## for st8 use average width for all discharge

st9.plot <- ggplot(st9, aes(x = Discharge, y = width)) + geom_point(size = 2.5) + stat_smooth(method = "lm");st9.plot
##for st9 use average width for all discharge???

st11L.plot <- ggplot(st11L, aes(x = Discharge, y = width)) + geom_point() + stat_smooth(method = "lm"); st11L.plot
st11L.lm <- lm(width~Discharge, data= st11L); summary(st11L.lm)
##ST11L use regression for widths

st13.plot <- ggplot(st13, aes(x = Discharge, y = width)) + geom_point() + stat_smooth(method = "lm"); st13.plot
###st13 use average width for all discharge

st14.plot <- ggplot(st14, aes(x = Discharge, y = width)) + geom_point() + stat_smooth(method = "lm");st14.plot
st14.lm <- lm(width~Discharge, data= st14);summary(st14.lm)
##st14 use regression for width-discharge

st17.plot <- ggplot(st17, aes(x = Discharge, y = width)) + geom_point() + stat_smooth(method = "lm");st17.plot
st17.lm <- lm(width~Discharge, data= st17); summary(st17.lm)
##st17 use regression for width-discharge

hver.plot <- ggplot(hver, aes(x = Discharge, y = width)) + geom_point() + stat_smooth(method = "lm"); hver.plot
hver.lm <- lm(width~Discharge, data=hver);summary(hver.lm)
##hver use regression

######################Modeling travel time-discharge in all streams##########
st1tt.plot <- ggplot(st1, aes(x = log(Discharge), y = log(tt.s))) + geom_point();st1tt.plot
st1tt.lm <- lm(log(tt.s)~log(Discharge), data = st1);summary(st1tt.lm)
plot(resid(st1tt.lm))
####use power regression for tt-discharge in st1


###Dropping an outlier for st5tt
st5tt <- st5[-c(1,9),]
st5tt.plot <- ggplot(st5tt, aes(x = Discharge, y = tt.s)) + geom_point();st5tt.plot
st5tt.lm <- lm(tt.s~Discharge, data = st5tt);summary(st5tt.lm)
plot(resid(st5tt.lm))
##use regression tt-discharge of the data w/ outliers removed

st6tt.plot <- ggplot(st6, aes(x = log(Discharge), y = log(tt.s))) + geom_point() + stat_smooth(method = "lm");st6tt.plot
st6tt.lm <- lm(log(tt.s)~log(Discharge), data = st6); summary(st6tt.lm)
###use regression for tt-Discharge


#taking out an  outlier that is waaaayy out there
st8tt <- st8[which(st8$Discharge < 0.06),]
st8tt.plot <- ggplot(st8tt, aes(x = log(Discharge), y = log(tt.s))) + geom_point() + stat_smooth(method = "lm");st8tt.plot
st8tt.lm <- lm(log(tt.s)~Discharge, data= st8tt);summary(st8tt.lm)
plot(resid(st8tt.lm))
##use regression with outlier removed for tt-discharge in st8

st9tt.plot <- ggplot(st9, aes(x = log(Discharge), y = log(tt.s))) + geom_point();st9tt.plot
st9tt.lm <- lm(log(tt.s)~log(Discharge), data= st9);summary(st9tt.lm)
##use regression of power lm for st9 tt-discharge

#pulling out 1 outlier
st11Ltt <- st11L[which(st11L$tt.s != 273),]
st11Ltt.plot <- ggplot(st11Ltt, aes(x = log(Discharge), y = log(tt.s))) + geom_point();st11Ltt.plot

st11Ltt.lm <- lm(log(tt.s)~log(Discharge), data=st11Ltt);summary(st11Ltt.lm)
##use regresssion with single outlier removed
st13tt <- st13[which(st13$Discharge > 0.0125),]
st13tt.plot <- ggplot(st13tt, aes(x = Discharge, y = tt.s))+geom_point()+stat_smooth(method = "lm");st13tt.plot

st13tt.lm <- lm(tt.s~Discharge, data = st13tt);summary(st13tt.lm)
#use regression with outlier removed for st13

st14tt.plot <- ggplot(st14, aes(x = log(Discharge), y = log(tt.s))) + geom_point(); st14tt.plot
st14tt.lm <- lm(log(tt.s)~log(Discharge), data=st14);summary(st14tt.lm)
##use power regression for tt-discharge in st14

st17tt.plot <- ggplot(st17, aes(x = log(Discharge), y = log(tt.s))) + geom_point();st17tt.plot
st17tt.lm <- lm(log(tt.s)~log(Discharge), data = st17);summary(st17tt.lm)
#use power regression for st17 tt-discharge relationships

hvertt.plot <- ggplot(hver, aes(x = log(Discharge), y = log(tt.s))) + geom_point();hvertt.plot

hvertt.lm <- lm(log(tt.s)~log(Discharge), data = hver);summary(hvertt.lm)
plot(resid(hvertt.lm))
plot(tt.s~Discharge, data=hver)

###take a look at the depths and discharge measures

st1d.plot <- ggplot(st1, aes(x = Discharge, y = depth)) + geom_point();st1d.plot
st5d.plot <- ggplot(st5, aes(x = Discharge, y = depth)) + geom_point();st5d.plot
st6d.plot <- ggplot(st6, aes(x = Discharge, y = depth)) + geom_point();st6d.plot
st8d.plot <- ggplot(st8, aes(x = Discharge, y = depth)) + geom_point();st8d.plot
st9d.plot <- ggplot(st9, aes(x = Discharge, y = depth)) + geom_point();st9d.plot
st11Ld.plot <- ggplot(st11L, aes(x = Discharge, y = depth)) + geom_point();st11Ld.plot
st13d.plot <- ggplot(st13, aes(x = Discharge, y = depth)) + geom_point();st13d.plot
st14d.plot <- ggplot(st14, aes(x = Discharge, y = depth)) + geom_point();st14d.plot
st17d.plot <- ggplot(st17, aes(x = Discharge, y = depth)) + geom_point();st17d.plot
hverd.plot <- ggplot(hver, aes(x = Discharge, y = depth)) + geom_point();hverd.plot
##These all suck!!! Not much data.


#Now take the estimated Widths and estimated travel times and add to discharge object: Q_all
##relisting all the regressions used for tt and widths
###Widths
w_rating1 <- lm(width ~ Discharge*(Discharge < 0.018) + Discharge*(Discharge > 0.018), data = st1)
w_rating5 <- lm(width ~ Discharge, data = st5)
w_rating6 <- mean(st6$width, na.rm = T)
w_rating8 <- mean(st8$width, na.rm = T)
w_rating9 <- mean(st9$width, na.rm = T)
w_rating11L <- lm(width~Discharge, data= st11L)
w_rating13 <- mean(st13$width, na.rm= T)
w_rating14 <- lm(width~Discharge, data= st14)
w_rating17 <- lm(width~Discharge, data= st17)
w_ratinghver <- lm(width~Discharge, data=hver)

###Travel Time

tt_rating1 <- lm(log(tt.s)~log(Discharge), data = st1)
tt_rating5 <- lm(tt.s~Discharge, data = st5tt)
tt_rating6 <- lm(log(tt.s)~log(Discharge), data = st6)
tt_rating8 <- lm(log(tt.s)~Discharge, data= st8tt)
tt_rating9 <- lm(log(tt.s)~log(Discharge), data= st9)
tt_rating11L <- lm(log(tt.s)~log(Discharge), data=st11Ltt)
tt_rating13 <- mean(st13$tt.s, na.rm = T)
tt_rating14 <- lm(log(tt.s)~log(Discharge), data=st14)
tt_rating17 <- lm(log(tt.s)~log(Discharge), data = st17)
tt_ratinghver <- lm(log(tt.s)~log(Discharge), data = hver)


##must convert Q_all_ag to m3/s
Q_all_m3 <- mutate(Q_all_ag, Discharge.m3 = Discharge/1000) 
Q_all_m3 <- Q_all_m3[,c(1,3,4)]
colnames(Q_all_m3) <- c("Stream", "day", "Discharge")
###isolate all the streams in Q_all
Q_all1 <- Q_all_m3[which(Q_all_ag$Stream == "st1"),]
Q_all5 <- Q_all_m3[which(Q_all_ag$Stream == "st5"),]
Q_all6 <- Q_all_m3[which(Q_all_ag$Stream == "st6"),]
Q_all8 <- Q_all_m3[which(Q_all_ag$Stream == "st8"),]
Q_all9 <- Q_all_m3[which(Q_all_ag$Stream == "st9"),]
Q_all11L <- Q_all_m3[which(Q_all_ag$Stream == "st11L"),]
Q_all13 <- Q_all_m3[which(Q_all_ag$Stream == "st13"),]
Q_all14 <- Q_all_m3[which(Q_all_ag$Stream == "st14"),]
Q_all17 <- Q_all_m3[which(Q_all_ag$Stream == "st17"),]
Q_allhver <- Q_all_m3[which(Q_all_ag$Stream == "hver"),]

###estimating width with discharge

Q_all1$width <- predict(w_rating1, Q_all1)
Q_all5$width <- predict(w_rating5, Q_all5)
Q_all6 <- transform(Q_all6, width = mean(st6$width, na.rm = T))
Q_all8 <- transform(Q_all8, width = mean(st8$width, na.rm = T))
Q_all9 <- transform(Q_all9, width = mean(st9$width, na.rm = T))
Q_all11L$width <- predict(w_rating11L, Q_all11L)
Q_all13 <- transform(Q_all13, width = mean(st13$width, na.rm = T))
Q_all14$width <- predict(w_rating14, Q_all14)
Q_all17$width <- predict(w_rating17, Q_all17)
Q_allhver$width <- predict(w_ratinghver, Q_allhver)

##merging all the Q and LWAD data

Q_all1$tt.s <- exp(predict(tt_rating1, Q_all1))
Q_all5$tt.s <- predict(tt_rating5, Q_all5)
Q_all6$tt.s <- exp(predict(tt_rating6, Q_all6))
Q_all8$tt.s <- exp(predict(tt_rating8, Q_all8))
Q_all9$tt.s <- exp(predict(tt_rating9, Q_all9))
Q_all11L$tt.s <- exp(predict(tt_rating11L, Q_all11L))
Q_all13 <- transform(Q_all13, tt.s = mean(st13$tt.s, na.rm=T))
Q_all14$tt.s <- exp(predict(tt_rating14, Q_all14))
Q_all17$tt.s <- exp(predict(tt_rating17, Q_all17))
Q_allhver$tt.s <- exp(predict(tt_ratinghver, Q_allhver))

##Getting the reach lengths for each read
Q_all1 <- transform(Q_all1, length = mean(st1$length, na.rm = T))
Q_all5 <- transform(Q_all5, length = mean(st5$length, na.rm = T))
Q_all6 <- transform(Q_all6, length = mean(st6$length, na.rm = T))
Q_all8 <- transform(Q_all8, length = mean(st8$length, na.rm = T))
Q_all9 <- transform(Q_all9, length = mean(st9$length, na.rm = T))
Q_all11L <- transform(Q_all11L, length = mean(st11L$length, na.rm = T))
Q_all13 <- transform(Q_all13, length = mean(st13$length, na.rm = T))
Q_all14 <- transform(Q_all14, length = mean(st14$length, na.rm = T))
Q_all17 <- transform(Q_all17, length = mean(st17$length, na.rm = T))
Q_allhver <- transform(Q_allhver, length = mean(hver$length, na.rm = T))


### modeling depth with disrcharge and width  !!!! need to add in length!!
Q_all1 <- transform(Q_all1, depth = (Discharge * tt.s)/(width*length))
Q_all5 <- transform(Q_all5, depth = (Discharge * tt.s)/(length*width))
Q_all6 <- transform(Q_all6, depth = (Discharge * tt.s)/(length*width))
Q_all8 <- transform(Q_all8, depth = (Discharge * tt.s)/(length*width))
Q_all9 <- transform(Q_all9, depth = (Discharge * tt.s)/(length*width))
Q_all11L <- transform(Q_all11L, depth = (Discharge * tt.s)/(length*width))
Q_all13 <- transform(Q_all13, depth = (Discharge * tt.s)/(length*width))
Q_all14 <- transform(Q_all14, depth = (Discharge * tt.s)/(length*width))
Q_all17 <- transform(Q_all17, depth = (Discharge * tt.s)/(length*width))
Q_allhver <- transform(Q_allhver, depth = (Discharge * tt.s)/(length*width))

##bind these all into data.frame

Depth_all <- rbind(Q_all1, Q_all5, Q_all6, Q_all8, Q_all9, Q_all11L, Q_all13, Q_all14, Q_all17, Q_allhver)


##plotting all the Q-depth relationships
dev.new()
Depth.plot <- ggplot(Depth_all, aes(x = log(Discharge), y = log(depth))) + geom_line(aes(group = Stream, colour = Stream), lwd = 2); Depth.plot



Q1 <- Q[which(Q$Stream == "st1"),]
Q1 <- Q1[order(Q1$day),]
Q1 <- Q1[-8,]

lwad1 <- LWAD[which(LWAD$stream == "st1"),]

Q_lwad1 <- merge(Q1, Q_all_ag, by = c("Stream", "day"))

#Q1_lwad <- with(Q, zoo(Q.mod, Pd))
#f <- function(u) which.min(abs(as.numeric(index(LWAD


