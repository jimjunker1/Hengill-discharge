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
library(MuMIn)
theme_set(theme_bw(20))

#load data
datetime <- read.csv("./stream-data/All_DateTime.csv")
Q <- read.csv("./stream-data/Q_data_summary_working.csv")
presL <- read.csv("./stream-data/9736059_7LO.csv")
presH <- read.csv("./stream-data/9736163_7HI_noNAs.csv")
pres11U <- read.csv("./stream-data/2451129_ST11U.csv")
pres17 <- read.csv("./stream-data/9736062_ST17.csv")

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
pres11U$Pd <- as.POSIXct(paste(pres11U$Date, pres11U$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres17$Pd <- as.POSIXct(paste(pres17$Date, pres17$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

#merging the full datetime file for 15min intervals from 7/20/2010 18:00:00 to Oct14
mylist <- list(pres11U, datetime)
pres11U <- do.call(rbind.fill, mylist)

mylist <- list(presL, datetime)
presL <- do.call(rbind.fill, mylist)

mylist <- list(presH, datetime)
presH <- do.call(rbind.fill, mylist)

mylist <- list(pres17, datetime)
pres17 <- do.call(rbind.fill, mylist)

#pres1 <- pres1[,2:6] #cleaning up row# column


#Make hourly means
	presLhr_d <- data.frame(presL$Pd, presL$Depthm, presL$TempC)
	presHhr_d <- data.frame(presH$Pd, presH$Depthm, presH$TempC)
	pres11Uhr_d <- data.frame(pres11U$Pd, pres11U$Depthm, pres11U$TempC)
	pres17hr_d <- data.frame(pres17$Pd, pres17$Depthm, pres17$TempC)
	
	names(presLhr_d) <- c("time", "depthm", "tempC")
	names(presHhr_d) <- c("time", "depthm", "tempC")
	names(pres11Uhr_d) <- c("time", "depthm", "tempC")
	names(pres17hr_d) <- c("time", "depthm", "tempC")

##First merge all the depth data by time
	
	presLhr <- aggregate(presLhr_d[c("depthm", "tempC")],
					list(hour = cut(presLhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)
					
	presHhr <- aggregate(presHhr_d[c("depthm", "tempC")],
					list(hour = cut(presHhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres11Uhr <- aggregate(pres11Uhr_d[c("depthm", "tempC")],
					list(hour = cut(pres11Uhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres17hr <- aggregate(pres17hr_d[c("depthm", "tempC")],
					list(hour = cut(pres17hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)


#convert times to posix object

	presLhr$time <- as.POSIXct(presLhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	presHhr$time <- as.POSIXct(presHhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres11Uhr$time <- as.POSIXct(pres11Uhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres17hr$time <- as.POSIXct(pres17hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")


	
# # RUNNING ONE TIME, THEN EXPORTING TO GET PT TEMPS
# #get temp at times of slugs
	# #create zoo objects

		 temp_L <- with(presLhr, zoo(tempC, time))
		 temp_H <- with(presHhr, zoo(tempC, time))
		 temp_11U <- with(pres11Uhr, zoo(tempC, time))
		 temp_17 <- with(pres17hr, zoo(tempC, time))
		 
		 d11U <- with(pres11Uhr, zoo(depthm, time))
		 dL <- with(presLhr, zoo(depthm, time))
		 dH <- with(presHhr, zoo(depthm, time))
		 d17 <- with(pres17hr, zoo(depthm, time))

######Looking at the correlations among the depths of each stream #######

Q_cor <- merge(d11U, d17)
Q_cor <- merge(Q_cor, dH)
Q_cor <- na.omit(Q_cor)

Q_cor_df <- data.frame(Q_cor)
cor(Q_cor_df)

##consider those streams that are highly correlated with ST7 
	#ST9- ST7Lo 
	#ST11D - ST7Lo
	#ST6 - ST7Lo

		#zoo objects 
			
			Q11U <- Q[which(Q$Qstream == "st11U"),]
			Q11U <- Q11U[!is.na(Q11U$Q.mod),]
			Q11U <- Q11U[order(Q11U$Pd),]
		 	Q11Uz <- with(Q11U, zoo(Q.mod, Pd))

			Q17 <- Q[which(Q$Qstream == "st17"),]
			Q17 <- Q17[!is.na(Q17$Q.mod),]
			Q17 <- Q17[order(Q17$Pd),]
		 	Q17z <- with(Q17, zoo(Q.mod, Pd))

			Q7 <- Q[which(Q$Qstream == "st7"),]
			Q7 <- Q7[order(Q7$Pd),]
			Q7z <- with(Q7, zoo(Q.mod, Pd))
			
	# #merge Q and PT temp data
	
		#ST11U
		 f <-  function(u) which.min(abs(as.numeric(index(temp_11U)) - as.numeric(u)))
		 ix <- vapply(index(Q11Uz), f, integer(1))
		 QP <- cbind(Q11U, temp_11U = coredata(temp_11U)[ix])
		 Qw11U <- data.frame(QP)

		

	##  merging the Q and PT depth data
		
		#ST11U
		 f <- function(u) which.min(abs(as.numeric(index(d11U)) - as.numeric(u)))
		 dx <- sapply(index(Q11Uz), f)
		 QP <- cbind(Qw11U, d11U = coredata(d11U) [dx])
		 Q11U_full <- data.frame(QP)

		

#merging Temp data
		 f <-  function(u) which.min(abs(as.numeric(index(temp_L)) - as.numeric(u)))
		 ix <- vapply(index(Q11Uz), f, integer(1))
		 QP <- cbind(Q11U_full, temp_L = coredata(temp_L)[ix])
		 Q11U_full <- data.frame(QP)

 		 f <-  function(u) which.min(abs(as.numeric(index(temp_H)) - as.numeric(u)))
		 ix <- vapply(index(Q11Uz), f, integer(1))
		 QP <- cbind(Q11U_full, temp_H = coredata(temp_H)[ix])
		 Q11U_full <- data.frame(QP)
			
		 #f <- function(u) which.min(abs(as.numeric(index(d1)) - as.numeric(u)))
		 #dx <- sapply(index(Q1z), f)
		 #QP <- cbind(Q_full, dL = coredata(dL) [dx])
		 #Q_full <- data.frame(QP)
		
		

#merging the Depths of each stream 
		 f <- function(u) which.min(abs(as.numeric(index(dH)) - as.numeric(u)))
		 dx <- sapply(index(Q11Uz), f)
		 QP <- cbind(Q11U_full, dH = coredata(dH) [dx])
		 Q11U_full <- data.frame(QP)

		 f <- function(u) which.min(abs(as.numeric(index(d17)) - as.numeric(u)))
		 dx <- sapply(index(Q11Uz), f)
		 QP <- cbind(Q11U_full, d17 = coredata(d17) [dx])
		 Q11U_full <- data.frame(QP)
	

#adding in a season variable
		
summer11U <- 	ifelse(as.numeric(format(Q11U_full$Pd, "%m")) >= 6 & as.numeric(format(Q11U_full$Pd, "%m"))<= 9, 1, 0) 		 		

Q11U_full <- cbind(Q11U_full, summer11U)

year11U <- as.numeric(format(Q11U_full$Pd, "%y"))

Q11U_full <- cbind(Q11U_full, year11U)

					# #just check to make sure files are matched up right
				 ggplot(Qw11U, aes(x = Q.mod, y = Q_DS)) + geom_point()
				 #ggplot(Qw1, aes(x = Q.mod, y = Q_DS)) + geom_point()
	# #Export
	# write.csv(Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/Qw2.csv")


# combine PT files
 
 #ST11U
depths <- merge(presLhr[,2:4], presHhr[,2:4], by = "time", all = TRUE)
depths <- merge(depths, pres11Uhr[,2:4], by = "time", all = T)
depths = merge(depths, pres17hr[,2:4], by = 'time', all = T)
depths <- depths[!as.numeric(format(depths$time, "%y")) == 1,]
names(depths) <- c("time","L_depthm", "L_tempC", "H_depthm", "H_tempC", "st11U_depthm", "st11U_tempC", "st17_depthm", "st17_tempC")


# write.csv(depths, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Landscape_all.csv")


#Need to see the correlation between ST7 and landscape streams
	ggplot(depths, aes(x = L_depthm, y = st11U_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,0.5)+
		ylim(0,0.5)

ggplot(depths, aes(x = H_depthm, y = st11U_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,0.5)+
		ylim(0,0.5)

ggplot(depths, aes(x = st11U_tempC, y = st11U_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		ylim(0,0.5)

ggplot(depths, aes(x = L_tempC, y = st11U_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		ylim(0,0.5)

ggplot(depths, aes(x = st17_depthm, y = st11U_depthm))+
  geom_point()+
  stat_smooth(method = "lm")+
  geom_abline(intercept = 0, slope = 1)+
  ylim(0,0.5)

#multiple regression
 #season <- S = June - Sept; NS = other months

#NEED TO FIGURE OUT WHICH PD TEMP DATA TO USE
	#US DS comp with cond probes
	#comparing temperature data between the cond loggers <- Looks darn good
	ggplot(Q11U_full, aes(x = temp_US_m, y = temp_DS_m))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,30)+
		ylim(0,30)
		
	#Temp data between US logger and PT <- PT is biased low at higher temps
	ggplot(Q11U_full, aes(x = temp_US_m, y = temp_11U))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(-10,30)+
		ylim(-10,40)
		
	#Temp data between DS and PT <- PT is biased high at higher temps
	ggplot(Q11U_full, aes(y = temp_DS_m, x = temp_11U))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,30)+
		ylim(0,30)
		
	#How does the relatioship look btw Q and Depth
	ggplot(Q11U_full, aes(y = Q11U_full$Q.mod, x = Q11U_full$d11U))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0.2, 0.3)+
		ylim(0,50)

 
#model selection to determine the best model
	#Q <- Q[2:34,]

	Q11U_full_mod <- Q11U_full[!is.na(Q11U_full$d11U),]
	Q11U_gm_mod <- lm(log(Q.mod) ~ log(d11U), Q11U_full_mod, na.action = "na.fail")
	Q11U_MS_mod <- dredge(Q11U_gm_mod, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))

	Q11U_gm <- lm(log(Q.mod) ~ log(d11U) + temp_11U + log(dL) + temp_L + temp_H + as.factor(summer11U), Q11U_full_mod, na.action = "na.fail")
	
	Q11U_MS <- dredge(Q11U_gm, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	
	subset(Q11U_MS, delta < 5)
	subset(Q11U_MS)
	subset(Q11U_MS_mod)


#looking at colinearity
	ggplot(Q11U_full_mod, aes(x =d11U, y = temp_11U)) +geom_point()
	
	#ggplot(Q, aes(x = season, y = T_US_PD)) + geom_boxplot()
	
	#T_seas_tt <- t.test(T_US_PD ~ season, Q); T_seas_tt
	
	#T_warm_tt <- t.test(T_US_PD ~ warming, Q); T_warm_tt

sm_rating11U <- lm(log(Q.mod) ~ log(d11U), Q11U_full_mod); summary(sm_rating11U)

				
# # 				

	Q11U_full_mod$fitted <- fitted(sm_rating11U)
	ggplot(Q11U_full_mod, aes(x = fitted, y =log(Q.mod)))+
		geom_point()+
		geom_abline(intercept = 0, slope = 1)
	
	ggplot(Q11U_full_mod, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,30)+
		ylim(0,50)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")")))
	
		
	mrbias11U <- lm(log(Q.mod)~ fitted, Q11U_full_mod); summary(mrbias11U)
				# Call:
				# lm(formula = log(Q_DS) ~ fitted, data = Q)
				
				# Residuals:
				     # Min       1Q   Median       3Q      Max 
				# -0.45961 -0.14513 -0.03547  0.11831  0.69621 
				
				# Coefficients:
				              # Estimate Std. Error t value Pr(>|t|)    
				# (Intercept) -3.092e-16  1.031e-01    0.00        1    
				# fitted       1.000e+00  5.892e-02   16.97   <2e-16 ***
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
				
				# Residual standard error: 0.2733 on 31 degrees of freedom
				# Multiple R-squared:  0.9028,	Adjusted R-squared:  0.8997 
				# F-statistic: 288.1 on 1 and 31 DF,  p-value: < 2.2e-16

#Applying MR to predict new data in depths
	#first need to code for season
		depths$summer11U <- as.factor(ifelse(as.numeric(format(depths$time, "%m")) >= 6 & as.numeric(format(depths$time, "%m"))<= 9, 1, 0))
		
	#merge new files	
	depths_m <- depths
	names(depths_m) <- c("time", "dL", "temp_L", "dH", "temp_H", "d11U", "temp_11U") #needs to be renamed so it matches the names in the equation.
	
	#predict Q
	depths_m$Q_mr <- exp(predict(sm_rating11U, depths_m))
	
 write.csv(depths_m, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/depth_m5.csv")
	

##ST11U
	#look at Q over time
		ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q11U_full_mod, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste("Q (L ", s^1,")")))
		#scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))
		
	ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q11U_full_mod, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste(log[10]," Q (L ", s^1,")")))+
		#scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))+
		scale_y_log10()
	
	#export data
	write.csv(depths_m, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st11U/depths_m11U.csv")
		
		
save.image(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st11U/st11U.RData")
load(	"C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st11U/st11U.RData")

	
	#adding Q predicted estimates to Qw file	
st7_Qw2 <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw2.csv")

st7_Qw2$Q_DS_predicted <- exp(predict(sm_rating, st7_Qw2))

write.csv(st7_Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw3.csv")
	
#################################################################################################################################################################
#Working Log
#Feb-6-2015. added full date-time file and merged to get 15min intervals from Jul 2010