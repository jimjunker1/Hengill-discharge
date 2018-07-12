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
library(dplyr)
library(MuMIn)
theme_set(theme_bw(20))


#load data
datetime <- read.csv("./stream-data/All_DateTime.csv")
Q <- read.csv("./stream-data/Q_data_summary_working.csv")
presL <- read.csv("./stream-data/9736059_7LO.csv")
presH <- read.csv("./stream-data/9736163_7HI_noNAs.csv")
pres11D <- read.csv("./stream-data/9736055_ST11L.csv")
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
pres11D$Pd <- as.POSIXct(paste(pres11D$Date, pres11D$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
light$Pd <- as.POSIXct(paste(light$date, light$time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
lightmod$Pd <- as.POSIXct(paste(lightmod$date, lightmod$time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")


#merging the full datetime file for 15min intervals from 7/20/2010 18:00:00 to Oct14
mylist <- list(pres11D, datetime)
pres11D <- do.call(rbind.fill, mylist)

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
	pres11Dhr_d <- data.frame(pres11D$Pd, pres11D$Depthm, pres11D$TempC)
	lighthr_d <- data.frame(light$Pd, light$PAR_H)
	lightmodhr_d <- data.frame(lightmod$Pd, lightmod$light)
	
	names(presLhr_d) <- c("time", "depthm", "tempC")
	names(presHhr_d) <- c("time", "depthm", "tempC")
	names(pres11Dhr_d) <- c("time", "depthm", "tempC")
	names(lighthr_d) <- c("time", "light")
	names(lightmodhr_d) <- c("time", "light.est")

##First merge all the depth data by time
	
	presLhr <- aggregate(presLhr_d[c("depthm", "tempC")],
					list(hour = cut(presLhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)
					
	presHhr <- aggregate(presHhr_d[c("depthm", "tempC")],
					list(hour = cut(presHhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres11Dhr <- aggregate(pres11Dhr_d[c("depthm", "tempC")],
					list(hour = cut(pres11Dhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	lighthr <- aggregate(lighthr_d["light"],
					list(hour = cut(lighthr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	lightmodhr <- aggregate(lightmodhr_d["light.est"], list(hour = cut(lightmodhr_d$time, breaks = "hour")),
					sum, na.rm= TRUE)

#convert times to posix object

	presLhr$time <- as.POSIXct(presLhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	presHhr$time <- as.POSIXct(presHhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres11Dhr$time <- as.POSIXct(pres11Dhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	lighthr$time <- as.POSIXct(lighthr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	lightmodhr$time <- as.POSIXct(lightmodhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")

#creating a moving average within the yearl

##make summer light cumulative variable for each season
	lightmod <- read.csv("./output-files/light-est_full.csv")
	
	lightmod$Pd <- as.POSIXct(lightmod$hour,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
	
	lightmodhr_d <- data.frame(lightmod$Pd, lightmod$light)
	
	names(lightmodhr_d) <- c("time", "light.est")
	
	lightmodhr_d$light_year <- as.numeric(format(lightmodhr_d$time, "%Y"))
	
	lightmodhr_d$cum.light <- ave(lightmodhr_d$light.est, lightmodhr_d$light_year, FUN = cumsum)
	lightmodhr_d = lightmodhr_d[,c(1:2,4)]
	
# # RUNNING ONE TIME, THEN EXPORTING TO GET PT TEMPS
# #get temp at times of slugs
	# #create zoo objects

		 temp_L <- with(presLhr, zoo(tempC, time))
		 temp_H <- with(presHhr, zoo(tempC, time))
		 temp_11D <- with(pres11Dhr, zoo(tempC, time))
		 light <- with(lighthr, zoo(light, time))
		 lightmod <- with(lightmodhr_d, zoo(cum.light, time))

		 d11D <- with(pres11Dhr, zoo(depthm, time))
		 dL <- with(presLhr, zoo(depthm, time))
		 dH <- with(presHhr, zoo(depthm, time))

######Looking at the correlations among the depths of each stream #######

Q_cor <- merge(d11D, dL)
Q_cor <- merge(Q_cor, dH)
Q_cor <- na.omit(Q_cor)

Q_cor_df <- data.frame(Q_cor)
cor(Q_cor_df)

##consider those streams that are highly correlated with ST7 
	#ST9- ST7Lo 
	#ST11D - ST7Lo
	#ST6 - ST7Lo

		#zoo objects 
			
			Q11D <- Q[which(Q$Qstream == "st11L"),]
			Q11D <- Q11D[!is.na(Q11D$Q.mod),]
			Q11D <- Q11D[order(Q11D$Pd),]
		 	Q11Dz <- with(Q11D, zoo(Q.mod, Pd))

			Q7 <- Q[which(Q$Qstream == "st7"),]
			Q7 <- Q7[order(Q7$Pd),]
			Q7z <- with(Q7, zoo(Q.mod, Pd))
			
	# #merge Q and PT temp data
	
		#ST11D
		 f <-  function(u) which.min(abs(as.numeric(index(temp_11D)) - as.numeric(u)))
		 ix <- vapply(index(Q11Dz), f, integer(1))
		 QP <- cbind(Q11D, temp_11D = coredata(temp_11D)[ix])
		 Qw11D <- data.frame(QP)
	##  merging the Q and PT depth data
		
		#ST11D
		 f <- function(u) which.min(abs(as.numeric(index(d11D)) - as.numeric(u)))
		 dx <- sapply(index(Q11Dz), f)
		 QP <- cbind(Qw11D, d11D = coredata(d11D) [dx])
		 Q11D_full <- data.frame(QP)

#merging Temp data
		 f <-  function(u) which.min(abs(as.numeric(index(temp_L)) - as.numeric(u)))
		 ix <- vapply(index(Q11Dz), f, integer(1))
		 QP <- cbind(Q11D_full, temp_L = coredata(temp_L)[ix])
		 Q11D_full <- data.frame(QP)

 		 f <-  function(u) which.min(abs(as.numeric(index(temp_H)) - as.numeric(u)))
		 ix <- vapply(index(Q11Dz), f, integer(1))
		 QP <- cbind(Q11D_full, temp_H = coredata(temp_H)[ix])
		 Q11D_full <- data.frame(QP)
			
		 #f <- function(u) which.min(abs(as.numeric(index(d1)) - as.numeric(u)))
		 #dx <- sapply(index(Q1z), f)
		 #QP <- cbind(Q_full, dL = coredata(dL) [dx])
		 #Q_full <- data.frame(QP)

#merging the Depths of each stream 
		 f <- function(u) which.min(abs(as.numeric(index(dL)) - as.numeric(u)))
		 dx <- vapply(index(Q11Dz), f, integer(1))
		 QP <- cbind(Q11D_full, dL = coredata(dL) [dx])
		 Q11D_full <- data.frame(QP)
			
		f <- function(u) which.min(abs(as.numeric(index(d11D)) - as.numeric(u)))
		dx <- vapply(index(Q11Dz), f, integer(1))
		QP <- cbind(Q11D_full, dH = coredata(dH) [dx])
		Q11D_full <- data.frame(QP)
	
		f <- function(u) which.min(abs(as.numeric(index(lightmod)) - as.numeric(u)))
		dx <- vapply(index(Q11Dz), f, integer(1))
		QP <- cbind(Q11D_full, cum.light = coredata(lightmod) [dx])
		Q11D_full <- data.frame(QP)    

#adding in a season variable
		
summer11D <- 	ifelse(as.numeric(format(Q11D_full$Pd, "%m")) >= 6 & as.numeric(format(Q11D_full$Pd, "%m"))<= 9, 1, 0) 		 		

Q11D_full <- cbind(Q11D_full, summer11D)

year11D <- as.numeric(format(Q11D_full$Pd, "%y"))

Q11D_full <- cbind(Q11D_full, year11D)
					# #just check to make sure files are matched up right
				 ggplot(Qw11D, aes(x = Q.mod, y = Q_DS)) + geom_point()
				 #ggplot(Qw1, aes(x = Q.mod, y = Q_DS)) + geom_point()
	# #Export
	# write.csv(Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/Qw2.csv")
# combine PT files
 
 #ST11D
depths <- merge(presLhr[,2:4], presHhr[,2:4], by = "time", all = TRUE)
depths <- merge(depths, pres11Dhr[,2:4], by = "time", all = T)
depths = merge(depths, lightmodhr_d[,c(1,3)], by = "time", all = T)
depths <- depths[!as.numeric(format(depths$time, "%y")) == 1,]
names(depths) <- c("time","L_depthm", "L_tempC", "H_depthm", "H_tempC", "st11D_depthm", "st11D_tempC", "cum.light")


##Need to see the correlation between ST7 and landscape streams
	ggplot(depths, aes(x = L_depthm, y = st11D_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,0.5)+
		ylim(0,0.5)

ggplot(depths, aes(x = H_depthm, y = st11D_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,0.5)+
		ylim(0,0.5)

ggplot(depths, aes(x = st11D_tempC, y = st11D_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		ylim(0,0.5)

ggplot(depths, aes(x = L_tempC, y = st11D_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		ylim(0,0.5)

#multiple regression
 #season <- S = June - Sept; NS = other months

#NEED TO FIGURE OUT WHICH PD TEMP DATA TO USE
	#US DS comp with cond probes
	#comparing temperature data between the cond loggers <- Looks darn good
	ggplot(Q11D_full, aes(x = temp_US_m, y = temp_DS_m))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,20)+
		ylim(0,20)
		
	#Temp data between US logger and PT <- PT is biased low at higher temps
	ggplot(Q11D_full, aes(x = temp_US_m, y = temp_11D))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(-10,30)+
		ylim(-10,40)
		
	#Temp data between DS and PT <- PT is biased high at higher temps
	ggplot(Q11D_full, aes(y = temp_DS_m, x = temp_11D))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,30)+
		ylim(0,30)
		
	#How does the relatioship look btw Q and Depth
	ggplot(Q11D_full, aes(y = Q11D_full$Q.mod, x = Q11D_full$d11D))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0.2, 0.3)+
		ylim(0,50)
	
	ggplot(Q11D_full, aes(y = Q.mod, x = cum.light))+
	  geom_point()+
	  stat_smooth(method = "lm")+
	  geom_abline(intercept = 0, slope = 1)+
	  #xlim(0.2, 0.3)+
	  ylim(0,50)
	
	ggplot(Q11D_full, aes(y = log(Q.mod), x = log(d11D))) +
	  geom_point(size = 4) + geom_smooth(method = "lm", se = F)
	
	ggplot(Q11D_full_mod, aes(x = log(dL), y = log(Q.mod))) +
	  geom_point(size =3)

#model selection to determine the best model

	Q11D_full_mod <- Q11D_full[!is.na(Q11D_full$d11D),]
	Q11D_full_mod = Q11D_full_mod[-1,]
	Q11D_gm_mod <- lm(log(Q.mod) ~ cum.light +log(d11D) * log(dL) + log(d11D)*temp_11D, Q11D_full_mod, na.action = "na.fail")
	Q11D_MS_mod <- dredge(Q11D_gm_mod, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))

	x = subset(Q11D_MS_mod)


#looking at colinearity
	ggplot(Q11D_full_mod, aes(x =d11D, y = temp_11D)) +geom_point()


sm_rating11D <- lm(log(Q.mod) ~ log(d11D) + log(dL), Q11D_full_mod); summary(sm_rating11D)

# # 				

	Q11D_full_mod$fitted <- fitted(sm_rating11D)
	ggplot(Q11D_full_mod, aes(x = fitted, y =log(Q.mod)))+
		geom_point()+
		geom_abline(intercept = 0, slope = 1)
	
	ggplot(Q11D_full_mod, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(-10,50)+
		ylim(-10,50)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")")))
	
		
	mrbias11D <- lm(log(Q.mod)~ fitted, Q11D_full_mod); summary(mrbias11D)
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
	#merge new files	
	depths_m <- depths
	names(depths_m) <- c("time", "dL", "temp_L", "dH", "temp_H", "d11D", "temp_11D","light") #needs to be renamed so it matches the names in the equation.
	
	#predict Q
	depths_m$Q_mr <- exp(predict(sm_rating11D, depths_m))
	depths_m.fix = which(depths_m$Q_mr >= 15000)
	depths_m[depths_m.fix, "Q_mr"] = NA
	
 write.csv(depths_m, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/depth_m5.csv")
	

##ST11D
	#look at Q over time
		ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q11D_full_mod, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste("Q (L ", s^1,")")))
		#scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))
		
	ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q11D_full_mod, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste(log[10]," Q (L ", s^1,")")))+
		#scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))+
		scale_y_log10()
	
	ggplot(depths_m, aes(x = d11D, Q_mr)) +
	  geom_point(size = 3)
	
	#export data
	write.csv(depths_m, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st11D/depths_m11D.csv")
		
		
save.image(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st11D/st11D.RData")
load(	"C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st11D/st11D.RData")



F1 = 	ggplot(depths, aes(x = time, y = st11D_depthm)) +
		geom_point(size = 3) +
		ggtitle("All depth measurements")


F2 = ggplot(Q11D_full_mod, aes(y = Q.mod, x = d11D, label= Qdate))+
		geom_point( size = 3, fill = "green", pch = 21, )+
		geom_text() +
		scale_color_brewer(palette = "Set1") +
		#stat_smooth(method = "lm")+
		geom_abline(intercept = 0, slope = 1) +
		#xlim(0.35, 0.45) +
		ggtitle("Depth - Q relationship")
		#ylim(0.25,.50)


data.labels <- data.frame(
	fitted = 10,
	Q.mod = 35,
	label = "R^2 = 0.42 \n p = 0.15 \n eqn = log(Q) ~ log(depth) + log(dL)")

F3 = ggplot(Q11D_full_mod, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,50)+
		ylim(0,50)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")"))) +
		geom_text(data = data.labels, aes(x = fitted, y = Q.mod, label = label)) +
		ggtitle("Q - Fitted")

F4 = ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q11D_full, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste("Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y")) +
		ggtitle("Predicted Q")

pdf("Stream11Dplots.pdf", height = 15, width = 8)
grid.arrange(F1, F2, F3, F4, nrow = 4, ncol = 1)
dev.off()
	
	#adding Q predicted estimates to Qw file	
st7_Qw2 <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw2.csv")

st7_Qw2$Q_DS_predicted <- exp(predict(sm_rating, st7_Qw2))

write.csv(st7_Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw3.csv")
	
#################################################################################################################################################################
#Working Log
#Feb-6-2015. added full date-time file and merged to get 15min intervals from Jul 2010