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
library(data.table)
library(corrplot)
theme_set(theme_bw(20))

#load data
datetime <- read.csv("./stream-data/All_DateTime.csv")
Q <- read.csv("./stream-data/Q_data_summary_working.csv")
presL <- read.csv("./stream-data/9736059_7LO.csv")
presH <- read.csv("./stream-data/9736163_7HI_noNAs.csv")
pres6 <- read.csv("./stream-data/9736057_ST6.csv")
light <- read.csv("./stream-data/lux_par_final.csv")
lightmod <- read.csv("./stream-data/Light-est_full.csv")


#Combine the upper and lower logger data if NA on Lower logger

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
Q <- Q[,c(1:13,16)]


#convert times to posix objects
datetime$Pd <- as.POSIXct(paste(datetime$Date, datetime$Time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
Q$Pd <- as.POSIXct(paste(Q$Qdate, Q$Qtime), format = "%m/%d/%y %H:%M:%S", tz = "UTC")
Q <- Q[!is.na(Q$Pd),]
presL$Pd <- as.POSIXct(paste(presL$Date, presL$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
presH$Pd <- as.POSIXct(paste(presH$Date, presH$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres6$Pd <- as.POSIXct(paste(pres6$Date, pres6$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
light$Pd <- as.POSIXct(paste(light$date, light$time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
lightmod$Pd <- as.POSIXct(paste(lightmod$date, lightmod$time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")


#merging the full datetime file for 15min intervals from 7/20/2010 18:00:00 to Oct14
#this works

mylist <- list(pres6, datetime)
pres6 <- do.call(rbind.fill, mylist)

mylist <- list(presL, datetime)
presL <- do.call(rbind.fill, mylist)

mylist <- list(presH, datetime)
presH <- do.call(rbind.fill, mylist)

mylist <- list(light, datetime)
light <- do.call(rbind.fill, mylist)

mylist <- list(lightmod, datetime)
lightmod <- do.call(rbind.fill, mylist)


#pres1 <- pres1[,2:6] #cleaning up row# column


#Make hourly means
	presLhr_d <- data.frame(presL$Pd, presL$Depthm, presL$TempC)
	presHhr_d <- data.frame(presH$Pd, presH$Depthm, presH$TempC)
	pres6hr_d <- data.frame(pres6$Pd, pres6$Depthm, pres6$TempC)
	lighthr_d <- data.frame(light$Pd, light$PAR_H)
	lightmodhr_d <- data.frame(lightmod$Pd, lightmod$light)

	names(presLhr_d) <- c("time", "depthm", "tempC")
	names(presHhr_d) <- c("time", "depthm", "tempC")
	names(pres6hr_d) <- c("time", "depthm", "tempC")
	names(lighthr_d) <- c("time", "light")
	names(lightmodhr_d) <- c("time", "light.est")

##First merge all the depth data by time
	
	presLhr <- aggregate(presLhr_d[c("depthm", "tempC")],
					list(hour = cut(presLhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)
					
	presHhr <- aggregate(presHhr_d[c("depthm", "tempC")],
					list(hour = cut(presHhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres6hr <- aggregate(pres6hr_d[c("depthm", "tempC")],
					list(hour = cut(pres6hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	lighthr <- aggregate(lighthr_d["light"],
					list(hour = cut(lighthr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	lightmodhr <- aggregate(lightmodhr_d["light.est"], list(hour = cut(lightmodhr_d$time, breaks = "hour")),
					sum, na.rm= TRUE)

#convert times to posix object

	presLhr$time <- as.POSIXct(presLhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	presHhr$time <- as.POSIXct(presHhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres6hr$time <- as.POSIXct(pres6hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	lighthr$time <- as.POSIXct(lighthr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	lightmodhr$time <- as.POSIXct(lightmodhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")

#creating a moving average within the year


##make summer light cumulative variable for each season
light_year <- as.numeric(format(lightmodhr$time, "%Y"))
lightmodhr <- cbind(lightmodhr, light_year)

lightmodhr <- lightmodhr %>% group_by(light_year) %>% mutate(cum_light = cumsum(light.est))

lightmodhr <- ungroup(lightmodhr)
lightmodhr <- data.frame(lightmodhr)

write.csv(lightmodhr, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/lightmodhr.csv")

# # RUNNING ONE TIME, THEN EXPORTING TO GET PT TEMPS
# #get temp at times of slugs
	# #create zoo objects

		 temp_L <- with(presLhr, zoo(tempC, time))
		 temp_H <- with(presHhr, zoo(tempC, time))
		 temp_6 <- with(pres6hr, zoo(tempC, time))
		 light <- with(lighthr, zoo(light, time))
		 lightmod <- with(lightmodhr, zoo(cum_light, time))

		 d6 <- with(pres6hr, zoo(depthm, time))
		 dL <- with(presLhr, zoo(depthm, time))
		 dH <- with(presHhr, zoo(depthm, time))

######Looking at the correlations among the depths of each stream #######

Q_cor <- merge(dL, d6)
Q_cor <- merge(Q_cor, dH)
Q_cor <- na.omit(Q_cor)

Q_cor_df <- data.frame(Q_cor)
cor(Q_cor_df)

##consider those streams that are highly correlated with ST7 
	#ST9 - ST7Lo 
	#ST11D - ST7Lo
	#ST6 - ST7Lo

		#zoo objects 
		
			Q6 <- Q[which(Q$Qstream == "st6"),]
			Q6 <- Q6[!is.na(Q6$Q.mod),]
			Q6 <- Q6[order(Q6$Pd),]
		 	Q6z <- with(Q6, zoo(Q.mod, Pd))
			#Q6z <- Q6z[!is.na(index(Q6z)),]
			
			Q7 <- Q[which(Q$Qstream == "st7"),]
			Q7 <- Q7[order(Q7$Pd),]
			Q7z <- with(Q7, zoo(Q.mod, Pd))
			
	# #merge Q and PT temp data
		
			#ST6
		 f <-  function(u) which.min(abs(as.numeric(index(temp_6)) - as.numeric(u)))
		 ix <- vapply(index(Q6z), f, integer(1))
		 QP <- cbind(Q6, temp_6 = coredata(temp_6)[ix])
		 Qw6 <- data.frame(QP)

		

##  merging the Q and PT depth data
		#ST6
		 f <- function(u) which.min(abs(as.numeric(index(d6)) - as.numeric(u)))
		 dx <- sapply(index(Q6z), f)
		 QP <- cbind(Qw6, d6 = coredata(d6) [dx])
		 Q6_full <- data.frame(QP)

		
		#merging Temp data
		 f <-  function(u) which.min(abs(as.numeric(index(temp_L)) - as.numeric(u)))
		 ix <- vapply(index(Q6z), f, integer(1))
		 QP <- cbind(Q6_full, temp_L = coredata(temp_L)[ix])
		 Q6_full <- data.frame(QP)

 		 f <-  function(u) which.min(abs(as.numeric(index(temp_H)) - as.numeric(u)))
		 ix <- vapply(index(Q6z), f, integer(1))
		 QP <- cbind(Q6_full, temp_H = coredata(temp_H)[ix])
		 Q6_full <- data.frame(QP)
			
#Merging the Depths of each stream
		 	
		f <- function(u) which.min(abs(as.numeric(index(dH)) - as.numeric(u)))
		dx <- sapply(index(Q6z), f)
		QP <- cbind(Q6_full, dH = coredata(dH) [dx])
		Q6_full <- data.frame(QP)

		f <- function(u) which.min(abs(as.numeric(index(light)) - as.numeric(u)))
		dx <- sapply(index(Q6z), f)
		QP <- cbind(Q6_full, light = coredata(light) [dx])
		Q6_full <- data.frame(QP)    

		f <- function(u) which.min(abs(as.numeric(index(d6)) - as.numeric(u)))
		dx <- sapply(index(Q6z), f)
		QP <- cbind(Q6_full, cum.light = coredata(lightmod) [dx])
		Q6_full <- data.frame(QP)  

#adding in a season variable

summer6 <- 	ifelse(as.numeric(format(Q6_full$Pd, "%m")) >= 6 & as.numeric(format(Q6_full$Pd, "%m"))<= 9, 1, 0) 		

Q6_full <- cbind(Q6_full, summer6)

year6 <- as.numeric(format(Q6_full$Pd, "%y"))
Q6_full <- cbind(Q6_full, year6)

month6 <- as.numeric(format(Q6_full$Pd, "%m"))

Q6_full <- cbind(Q6_full, month6)

					# #just check to make sure files are matched up right
				 ggplot(Qw6, aes(x = Q.mod, y = Q_DS)) + geom_point()
				 #ggplot(Qw1, aes(x = Q.mod, y = Q_DS)) + geom_point()
	# #Export
	# write.csv(Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/Qw2.csv")


# combine PT files
 
 #ST6
depths <- merge(presLhr[,2:4], presHhr[,2:4], by = "time", all = TRUE)
depths <- merge(depths, pres6hr[,2:4], by = "time", all = T)
depths <- depths[!as.numeric(format(depths$time, "%y")) == 1,]
names(depths) <- c("time","L_depthm", "L_tempC", "H_depthm", "H_tempC", "st6_depthm", "st6_tempC")


# write.csv(depths, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Landscape_all.csv")


#Need to see the correlation between ST7 and landscape streams
	ggplot(depths, aes(x = L_depthm, y = st1_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,0.5)+
		ylim(0,0.5)

ggplot(depths, aes(x = H_depthm, y = st6_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,0.6)+
		ylim(0,0.5)

ggplot(depths, aes(x = st6_tempC, y = st6_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		ylim(0,0.5)

ggplot(depths, aes(x = L_tempC, y = st6_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		ylim(0,0.5)

#multiple regression
 #season <- S = June - Sept; NS = other months

#NEED TO FIGURE OUT WHICH PD TEMP DATA TO USE
	#US DS comp with cond probes
	#comparing temperature data between the cond loggers
	ggplot(Q1_full, aes(x = temp_US_m, y = temp_DS_m))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,30)+
		ylim(0,30)
		
	#Temp data between US logger and PT <- This is biased slightly low
	ggplot(Q1_full, aes(x = temp_US_m, y = temp_1))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,30)+
		ylim(0,30)
		
	#Temp data between DS and PT <- This is biased slightly high
	ggplot(Q6_full, aes(y = temp_DS_m, x = temp_1))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,30)+
		ylim(0,30)
		
	#How does the relatioship look btw Q and Depth
	ggplot(Q6_full, aes(y = Q6_full$Q.mod, x = Q6_full$d6, group = summer6, label = Qdate))+
		geom_point()+
		geom_text() +
		scale_color_brewer (palette = "Set1") +
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)
		#xlim()+
		#ylim()

 ##ST6
	ggplot(Q6_full, aes(y = Q6_full$Q.mod, x = Q6_full$d6))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0.28, 0.32)+
		ylim(16,30)

	ggplot(Q6_full, aes(y = Q6_full$Q.mod, x = Q6_full$temp_6))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		#xlim(0,25)+
		ylim(15,30)

	ggplot(pres6hr_d, aes(y = depthm, x = tempC)) +
		geom_point() + 
		stat_smooth(method = "lm") +
		geom_abline(intercept = 0, line = 1)+
		xlim(0,25)+
		ylim(0,1)

#model selection to determine the best model
	#Q <- Q[2:34,]
	library(MuMIn)
	
#st6

	#Q6_gm <- lm(log(Q.mod) ~ log(d6) + temp_6 + log(dH) + year6, Q6_full, na.action = "na.fail")
	Q6_gm <- lm(log(Q.mod) ~ log(d6), Q6_full, na.action = "na.fail")
	Q6_full_mod <- Q6_full[!is.na(Q6_full$Q.mod),]
	Q6_gm_mod <- lm(log(Q.mod) ~ log(d6) * month6  + year6 * month6 + log(d6) * year6, Q6_full_mod, na.action = "na.fail")

	Q6_MS <- dredge(Q6_gm, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	Q6_MS_mod <- dredge(Q6_gm_mod, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	
	subset(Q6_MS, delta < 6)
	subset(Q6_MS)
	subset(Q6_MS_mod)

#looking at colinearity
	ggplot(Q1_full, aes(x =d1, y = temp_1)) +geom_point()
	
	ggplot(Q, aes(x = season, y = T_US_PD)) + geom_boxplot()
	
	T_seas_tt <- t.test(T_US_PD ~ season, Q); T_seas_tt
	
	T_warm_tt <- t.test(T_US_PD ~ warming, Q); T_warm_tt

				
		
	mrbias <- lm(log(Q.mod)~ fitted, Q6_full); summary(mrbias)
	
#ST6	
sm_rating6_mod <- lm(log(Q.mod) ~ log(d6) + year6 + temp_6 + log(d6) * month6, Q6_full_mod); summary(sm_rating6_mod)
sm_rating6 <- lm(log(Q.mod) ~ log(d6), Q6_full_mod); summary(sm_rating6)

Q6_full_mod$fitted <- fitted(sm_rating6)
	ggplot(Q6_full, aes(x = fitted, y =log(Q.mod)))+
		geom_point()+
		geom_abline(intercept = 0, slope = 1)

	ggplot(Q6_full_mod, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,35)+
		ylim(0,35)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")")))


Q6_full_mod$fitted <- fitted(sm_rating6_mod)
	ggplot(Q6_full_mod, aes(x = fitted, y =log(Q.mod)))+
		geom_point()+
		geom_abline(intercept = 0, slope = 1)
	
	ggplot(Q6_full_mod, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,35)+
		ylim(0,35)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")")))

	mrbias <- lm(log(Q.mod)~ fitted, Q6_full_mod); summary(mrbias)

#Applying MR to predict new data in depths
	#first need to code for season
		depths$summer6 <- as.factor(ifelse(as.numeric(format(depths$time, "%m")) >= 6 & as.numeric(format(depths$time, "%m"))<= 9, 1, 0))
		depths$year6  <- as.numeric(format(depths$time, "%y"))
		depths$month6 <- as.numeric(format(depths$time, "%m"))
#ST6
	depths_m <- depths
	names(depths_m) <- c("time", "dL", "temp_L", "dH", "temp_H", "d6", "temp_6", "summer6", "year6", "month6") #needs to be renamed so it matches the names in the equation.
	
	
	#predict Q
	depths_m$Q_mr <- exp(predict(sm_rating6, depths_m))
	
 write.csv(depths_m, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/depth_m6.csv")
	


##ST6

dev.new()
	#look at Q over time
		ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q6_full_mod, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste("Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))
		
	ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q6_full, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste(log[10]," Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))+
		scale_y_log10()


save.image(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st6/st6.RData")
load(	"C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st6/st6.RData")
	
	#export data
	write.csv(depths_m, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/st7_LandH_PTs2.csv")
		


F1 = 	ggplot(depths, aes(x = time, y = st6_depthm)) +
		geom_point(size = 3) +
		ggtitle("All depth measurements")


F2 = ggplot(Q6_full_mod, aes(y = Q.mod, x = d6, label= Qdate))+
		geom_point()+
		geom_text() +
		scale_color_brewer(palette = "Set1") +
		#stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1) +
		#xlim(0.35, 0.45) +
		ggtitle("Depth - Q relationship")
		#ylim(0.25,.50)

data.labels <- data.frame(
	fitted = 13,
	Q.mod = 44,
	label = "R^2 = 0.39 \n p =0.02 \n eqn = log(Q) ~ log(depth)")

F3 = ggplot(Q6_full_mod, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		#xlim(0,45)+
		#ylim(0,35)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")"))) +
		geom_text(data = data.labels, aes(x = fitted, y = Q.mod, label = label)) +
		ggtitle("Q - Fitted")

F4 = ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q6_full, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste("Q (L ", s^-1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))


pdf("Stream6plots.pdf", height = 15, width = 8)
grid.arrange(F1, F2, F3, F4, nrow = 4, ncol = 1)
dev.off()		
		
	#adding Q predicted estimates to Qw file	
st7_Qw2 <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw2.csv")

st7_Qw2$Q_DS_predicted <- exp(predict(sm_rating, st7_Qw2))

write.csv(st7_Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw3.csv")
	
#################################################################################################################################################################
#Working Log
#Feb-6-2015. added full date-time file and merged to get 15min intervals from Jul 2010