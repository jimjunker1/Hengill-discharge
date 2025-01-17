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
theme_set(theme_bw(20))

#load data
datetime <- read.csv("./stream-data/All_DateTime.csv")
Q <- read.csv("./stream-data/Q_data_summary_working.csv")
presL <- read.csv("./stream-data/9736059_7LO.csv")
presH <- read.csv("./stream-data/9736163_7HI_noNAs.csv")
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
pres17$Pd <- as.POSIXct(paste(pres17$Date, pres17$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")


#merging the full datetime file for 15min intervals from 7/20/2010 18:00:00 to Oct14
mylist <- list(pres17, datetime)
pres17 <- do.call(rbind.fill, mylist)

mylist <- list(presL, datetime)
presL <- do.call(rbind.fill, mylist)

mylist <- list(presH, datetime)
presH <- do.call(rbind.fill, mylist)

#pres1 <- pres1[,2:6] #cleaning up row# column


#Make hourly means
	presLhr_d <- data.frame(presL$Pd, presL$Depthm, presL$TempC)
	presHhr_d <- data.frame(presH$Pd, presH$Depthm, presH$TempC)
	pres17hr_d <- data.frame(pres17$Pd, pres17$Depthm, pres17$TempC)
	
	
	names(presLhr_d) <- c("time", "depthm", "tempC")
	names(presHhr_d) <- c("time", "depthm", "tempC")
	names(pres17hr_d) <- c("time", "depthm", "tempC")
	

##First merge all the depth data by time
	
	presLhr <- aggregate(presLhr_d[c("depthm", "tempC")],
					list(hour = cut(presLhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)
					
	presHhr <- aggregate(presHhr_d[c("depthm", "tempC")],
					list(hour = cut(presHhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres17hr <- aggregate(pres17hr_d[c("depthm", "tempC")],
					list(hour = cut(pres17hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	

#convert times to posix object

	presLhr$time <- as.POSIXct(presLhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	presHhr$time <- as.POSIXct(presHhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres17hr$time <- as.POSIXct(pres17hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	
# # RUNNING ONE TIME, THEN EXPORTING TO GET PT TEMPS
# #get temp at times of slugs
	# #create zoo objects

		 temp_L <- with(presLhr, zoo(tempC, time))
		 temp_H <- with(presHhr, zoo(tempC, time))
		 temp_17 <- with(pres17hr, zoo(tempC, time))
		 
		 d17 <- with(pres17hr, zoo(depthm, time))
		 dL <- with(presLhr, zoo(depthm, time))
		 dH <- with(presHhr, zoo(depthm, time))

######Looking at the correlations among the depths of each stream #######

Q_cor <- merge(d17, dL)
Q_cor <- merge(Q_cor, dH)
Q_cor <- na.omit(Q_cor)

Q_cor_df <- data.frame(Q_cor)
cor(Q_cor_df)

##consider those streams that are highly correlated with ST7 
	#ST9- ST7Lo 
	#ST11D - ST7Lo
	#ST6 - ST7Lo

		#zoo objects 
			
			Q17 <- Q[which(Q$Qstream == "st17"),]
			Q17 <- Q17[!is.na(Q17$Q.mod),]
			Q17 <- Q17[order(Q17$Pd),]
		 	Q17z <- with(Q17, zoo(Q.mod, Pd))

			Q7 <- Q[which(Q$Qstream == "st7"),]
			Q7 <- Q7[order(Q7$Pd),]
			Q7z <- with(Q7, zoo(Q.mod, Pd))
			
	# #merge Q and PT temp data
	
		#ST17
		 f <-  function(u) which.min(abs(as.numeric(index(temp_17)) - as.numeric(u)))
		 ix <- vapply(index(Q17z), f, integer(1))
		 QP <- cbind(Q17, temp_17 = coredata(temp_17)[ix])
		 Qw17 <- data.frame(QP)

		

	##  merging the Q and PT depth data
		
		#ST17
		 f <- function(u) which.min(abs(as.numeric(index(d17)) - as.numeric(u)))
		 dx <- sapply(index(Q17z), f)
		 QP <- cbind(Qw17, d17 = coredata(d17) [dx])
		 Q17_full <- data.frame(QP)

		

#merging Temp data
		 f <-  function(u) which.min(abs(as.numeric(index(temp_L)) - as.numeric(u)))
		 ix <- vapply(index(Q17z), f, integer(1))
		 QP <- cbind(Q17_full, temp_L = coredata(temp_L)[ix])
		 Q17_full <- data.frame(QP)

 		 f <-  function(u) which.min(abs(as.numeric(index(temp_H)) - as.numeric(u)))
		 ix <- vapply(index(Q17z), f, integer(1))
		 QP <- cbind(Q17_full, temp_H = coredata(temp_H)[ix])
		 Q17_full <- data.frame(QP)
			
		 #f <- function(u) which.min(abs(as.numeric(index(d1)) - as.numeric(u)))
		 #dx <- sapply(index(Q1z), f)
		 #QP <- cbind(Q_full, dL = coredata(dL) [dx])
		 #Q_full <- data.frame(QP)
		
		

#merging the Depths of each stream 
		 f <- function(u) which.min(abs(as.numeric(index(dL)) - as.numeric(u)))
		 dx <- sapply(index(Q17z), f)
		 QP <- cbind(Q17_full, dL = coredata(dL) [dx])
		 Q17_full <- data.frame(QP)
	

#adding in a season variable
		
summer17 <- 	ifelse(as.numeric(format(Q17_full$Pd, "%m")) >= 6 & as.numeric(format(Q17_full$Pd, "%m"))<= 9, 1, 0) 		 		

Q17_full <- cbind(Q17_full, summer17)

year17 <- as.numeric(format(Q17_full$Pd, "%y"))

Q17_full <- cbind(Q17_full, year17)

					# #just check to make sure files are matched up right
				 ggplot(Qw17, aes(x = Q.mod, y = Q_DS)) + geom_point()
				 #ggplot(Qw1, aes(x = Q.mod, y = Q_DS)) + geom_point()
	# #Export
	# write.csv(Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/Qw2.csv")


# combine PT files
 
 #ST17
depths <- merge(presLhr[,2:4], presHhr[,2:4], by = "time", all = TRUE)
depths <- merge(depths, pres17hr[,2:4], by = "time", all = T)
depths <- depths[!as.numeric(format(depths$time, "%y")) == 1,]
names(depths) <- c("time","L_depthm", "L_tempC", "H_depthm", "H_tempC", "st17_depthm", "st17_tempC")


# write.csv(depths, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Landscape_all.csv")


#Need to see the correlation between ST7 and landscape streams
	ggplot(depths, aes(x = L_depthm, y = st17_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,1)+
		ylim(0,1)

ggplot(depths, aes(x = H_depthm, y = st17_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,1)+
		ylim(0,1)

ggplot(depths, aes(x = st17_tempC, y = st17_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		ylim(0,1)

ggplot(depths, aes(x = L_tempC, y = st17_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		ylim(0,2)

#multiple regression
 #season <- S = June - Sept; NS = other months

#NEED TO FIGURE OUT WHICH PD TEMP DATA TO USE
	#US DS comp with cond probes
	#comparing temperature data between the cond loggers <- Looks darn good
	ggplot(Q17_full, aes(x = temp_US_m, y = temp_DS_m))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(-10,30)+
		ylim(-10,30)
		
	#Temp data between US logger and PT <- PT is biased low at higher temps
	ggplot(Q17_full, aes(x = temp_US_m, y = temp_17))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(-10,30)+
		ylim(-10,40)
		
	#Temp data between DS and PT <- PT is biased high at higher temps
	ggplot(Q17_full, aes(y = temp_DS_m, x = temp_17))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,30)+
		ylim(0,30)
		
	#How does the relatioship look btw Q and Depth
	ggplot(Q17_full, aes(y = Q17_full$Q.mod, x = Q17_full$d17))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0.2,0.35)+
		ylim(0,500)

 
#model selection to determine the best model
	#Q <- Q[2:34,]
	library(MuMIn)

	Q17_full_mod <- Q17_full[!is.na(Q17_full$d17),]
	Q17_gm_mod <- lm(log(Q.mod) ~ log(d17) + temp_17 + as.factor(summer17) + year17, Q17_full_mod, na.action = "na.fail")
	Q17_MS_mod <- dredge(Q17_gm_mod, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))

	Q17_gm <- lm(log(Q.mod) ~ log(d17) + temp_17 + log(dL) + temp_L + temp_H + as.factor(summer17), Q17_full, na.action = "na.fail")
	
	Q17_MS <- dredge(Q17_gm, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	
	subset(Q17_MS, delta < 5)
	subset(Q17_MS)
	subset(Q17_MS_mod)


#looking at colinearity
	ggplot(Q17_full, aes(x =d17, y = temp_17)) +geom_point()
	
	#ggplot(Q, aes(x = season, y = T_US_PD)) + geom_boxplot()
	
	#T_seas_tt <- t.test(T_US_PD ~ season, Q); T_seas_tt
	
	#T_warm_tt <- t.test(T_US_PD ~ warming, Q); T_warm_tt

sm_rating17_mod <- lm(log(Q.mod) ~ log(d17) + temp_17, Q17_full_mod); summary(sm_rating17)
sm_rating17 <- lm(log(Q.mod) ~ log(d17), Q17_full_mod); summary(sm_rating17_mod)
				
# # 				

	Q17_full_mod$fitted <- fitted(sm_rating17)
	ggplot(Q17_full_mod, aes(x = fitted, y =log(Q.mod)))+
		geom_point()+
		geom_abline(intercept = 0, slope = 1)
	
	ggplot(Q17_full_mod, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,500)+
		ylim(0,500)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")")))
	
		
	mrbias17 <- lm(log(Q.mod)~ fitted, Q17_full); summary(mrbias17)
				# Call:
				# lm(formula = log(Q_DS) ~ fitted, data = Q)
				
				# Residuals:
				     # Min       1Q   Median       3Q      Max 
				# -0.45961 -0.14514 -0.03547  0.11831  0.69621 
				
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
		depths$summer17 <- as.factor(ifelse(as.numeric(format(depths$time, "%m")) >= 6 & as.numeric(format(depths$time, "%m"))<= 9, 1, 0))
		
	#merge new files	
	depths_m <- depths
	names(depths_m) <- c("time", "dL", "temp_L", "dH", "temp_H", "d17", "temp_17") #needs to be renamed so it matches the names in the equation.
	
	#predict Q
	depths_m$Q_mr <- exp(predict(sm_rating17, depths_m))
	
 write.csv(depths_m, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/depth_m5.csv")
	

##ST17
	#look at Q over time
		ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q17_full, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste("Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))
		
	ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q17_full, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste(log[10]," Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))+
		scale_y_log10()
	
	#export data
	write.csv(depths_m, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st17/depths_m17.csv")
		
		
save.image(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st17/st17.RData")
load(	"C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st17/st17.RData")

F1 = 	ggplot(depths, aes(x = time, y = st17_depthm)) +
		geom_point(size = 3) +
		ggtitle("All depth measurements")


F2 = ggplot(Q17_full_mod, aes(y = Q.mod, x = d17, label= Qdate))+
		geom_point( size = 3, fill = "green", pch = 21, )+
		geom_text() +
		scale_color_brewer(palette = "Set1") +
		#stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1) +
		#xlim(0.35, 0.45) +
		ggtitle("Depth - Q relationship")
		#ylim(0.25,.50)


data.labels <- data.frame(
	fitted = 60,
	Q.mod = 420,
	label = "R^2 = 0.78 \n p < 0.001 \n eqn = log(Q) ~ log(depth)")

F3 = ggplot(Q17_full_mod, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,500)+
		ylim(0,500)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")"))) +
		geom_text(data = data.labels, aes(x = fitted, y = Q.mod, label = label)) +
		ggtitle("Q - Fitted")

F4 = 		ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q17_full_mod, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste("Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y")) +
		theme(axis.text = element_text(size = 12))

pdf("Stream17plots.pdf", height = 15, width = 8)
grid.arrange(F1, F2, F3, F4, nrow = 4, ncol = 1)
dev.off()
	
	#adding Q predicted estimates to Qw file	
st7_Qw2 <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw2.csv")

st7_Qw2$Q_DS_predicted <- exp(predict(sm_rating, st7_Qw2))

write.csv(st7_Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw3.csv")
	
#################################################################################################################################################################
#Working Log
#Feb-6-2015. added full date-time file and merged to get 15min intervals from Jul 2010