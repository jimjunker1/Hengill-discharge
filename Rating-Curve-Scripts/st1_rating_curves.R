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

setwd("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/")

#load data
datetime <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/All_DateTime.csv")
light <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/lux_par_final.csv")
lightmod <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/Light-est_full.csv")
Q <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/Q_data_summary_working.csv")
presL <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736059_7LO.csv")
presH <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736163_7HI_noNAs.csv")
pres1 <- read.csv("C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/9736056_ST1b.csv")

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
light$Pd <- as.POSIXct(paste(light$date, light$time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
lightmod$Pd <- as.POSIXct(paste(lightmod$date, lightmod$time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
Q$Pd <- as.POSIXct(paste(Q$Qdate, Q$Qtime), format = "%m/%d/%y %H:%M:%S", tz = "UTC")
Q <- Q[!is.na(Q$Pd),]
presL$Pd <- as.POSIXct(paste(presL$Date, presL$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
presH$Pd <- as.POSIXct(paste(presH$Date, presH$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
pres1$Pd <- as.POSIXct(paste(pres1$Date, pres1$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

#merging the full datetime file for 15min intervals from 7/20/2010 18:00:00 to Oct14
mylist <- list(pres1, datetime)
pres1 <- do.call(rbind.fill, mylist) #this works

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
	pres1hr_d <- data.frame(pres1$Pd, pres1$Depthm, pres1$TempC)
	lighthr_d <- data.frame(light$Pd, light$PAR_H)
	lightmodhr_d <- data.frame(lightmod$Pd, lightmod$light)


	names(presLhr_d) <- c("time", "depthm", "tempC")
	names(presHhr_d) <- c("time", "depthm", "tempC")
	names(pres1hr_d) <- c("time", "depthm", "tempC")
	names(lighthr_d) <- c("time", "light")
	names(lightmodhr_d) <- c("time", "light.est")



##First merge all the depth data by time
	
	presLhr <- aggregate(presLhr_d[c("depthm", "tempC")],
					list(hour = cut(presLhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)
					
	presHhr <- aggregate(presHhr_d[c("depthm", "tempC")],
					list(hour = cut(presHhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	pres1hr <- aggregate(pres1hr_d[c("depthm", "tempC")],
					list(hour = cut(pres1hr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	lighthr <- aggregate(lighthr_d["light"],
					list(hour = cut(lighthr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

	lightmodhr <- aggregate(lightmodhr_d["light.est"], list(hour = cut(lightmodhr_d$time, breaks = "hour")),
					sum, na.rm= TRUE)

#convert times to posix object

	presLhr$time <- as.POSIXct(presLhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	presHhr$time <- as.POSIXct(presHhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	pres1hr$time <- as.POSIXct(pres1hr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	lighthr$time <- as.POSIXct(lighthr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	lightmodhr$time <- as.POSIXct(lightmodhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")


#creating a moving average within the yearl

#lightmodhr <- within(lightmodhr, cum.light <- cumsum(light.est))

##make summer light cumulative variable for each season
light_year <- as.numeric(format(lightmodhr$time, "%Y"))
lightmodhr <- cbind(lightmodhr, light_year)

lightmodhr <- lightmodhr %>% group_by(light_year) %>% mutate(cum_light = cumsum(light.est))

lightmodhr <- ungroup(lightmodhr)
#lightmodhrDT <- data.table(lightmodhr)
#lightmodhrDT2 <- lightmodhrDT[,cumsum(light.est), by = "light_year"]

#year_light <- mutate(lightmodhr, .light_year, transform, cum_light = cumsum(cum.light))

#lightmodhr <- cbind(lightmodhr, lightmodhrDT2)

#lightmodhr <- lightmodhr[,c(3,2,4,5,8)]
#names(lightmodhr) <- c("time", "light_est", "cum_light", "light_year", "year_light")
#lightmodhr <- data.frame(lightmodhr)
lightmodhr <- data.frame(lightmodhr)

write.csv(lightmodhr, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/lightmodhr.csv")

# # RUNNING ONE TIME, THEN EXPORTING TO GET PT TEMPS
# #get temp at times of slugs
	# #create zoo objects

		 temp_L <- with(presLhr, zoo(tempC, time))
		 temp_H <- with(presHhr, zoo(tempC, time))
		 temp_1 <- with(pres1hr, zoo(tempC, time))
		 light <- with(lighthr, zoo(light, time))
		 lightmod <- with(lightmodhr, zoo(cum_light, time))

		 d1 <- with(pres1hr, zoo(depthm, time))
		 dL <- with(presLhr, zoo(depthm, time))
		 dH <- with(presHhr, zoo(depthm, time))


write.csv(lightmod, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/lightmod.csv")


######Looking at the correlations among the depths of each stream #######

Q_cor <- merge(d1, dL)
Q_cor <- merge(Q_cor, dH)
Q_cor <- na.omit(Q_cor)

Q_cor_df <- data.frame(Q_cor)
cor(Q_cor_df)


write.csv(year_light, file = "C/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/lightmodhr.csv")





##consider those streams that are highly correlated with ST7 
	#ST9 - ST7Lo 
	#ST11D - ST7Lo
	#ST6 - ST7Lo

		#zoo objects 
			Q1 <- Q[which(Q$Qstream == "st1"),]
			Q1 <- Q1[order(Q1$Pd),]
		 	Q1z <- with(Q1, zoo(Q.mod, Pd))

			Q7 <- Q[which(Q$Qstream == "st7"),]
			Q7 <- Q7[order(Q7$Pd),]
			Q7z <- with(Q7, zoo(Q.mod, Pd))
			
	# #merge Q and PT temp data
		
		#ST1
		 f <-  function(u) which.min(abs(as.numeric(index(temp_1)) - as.numeric(u)))
		 ix <- vapply(index(Q1z), f, integer(1))
		 QP <- cbind(Q1, temp_1 = coredata(temp_1)[ix])
		 Qw1 <- data.frame(QP)

	##  merging the Q and PT depth data
		#ST1
		 f <- function(u) which.min(abs(as.numeric(index(d1)) - as.numeric(u)))
		 dx <- sapply(index(Q1z), f)
		 QP <- cbind(Qw1, d1 = coredata(d1) [dx])
		 Q1_full <- data.frame(QP)

		
		#merging Temp data
		 f <-  function(u) which.min(abs(as.numeric(index(temp_L)) - as.numeric(u)))
		 ix <- vapply(index(Q1z), f, integer(1))
		 QP <- cbind(Q1_full, temp_L = coredata(temp_L)[ix])
		 Q1_full <- data.frame(QP)

 		 f <-  function(u) which.min(abs(as.numeric(index(temp_H)) - as.numeric(u)))
		 ix <- vapply(index(Q1z), f, integer(1))
		 QP <- cbind(Q1_full, temp_H = coredata(temp_H)[ix])
		 Q1_full <- data.frame(QP)
			
		 f <- function(u) which.min(abs(as.numeric(index(d1)) - as.numeric(u)))
		 dx <- sapply(index(Q1z), f)
		 QP <- cbind(Q1_full, dL = coredata(dL) [dx])
		 Q1_full <- data.frame(QP)

#merging the Depths of each stream
		 	
		f <- function(u) which.min(abs(as.numeric(index(d1)) - as.numeric(u)))
		dx <- sapply(index(Q1z), f)
		QP <- cbind(Q1_full, dH = coredata(dH) [dx])
		Q1_full <- data.frame(QP)

		f <- function(u) which.min(abs(as.numeric(index(light)) - as.numeric(u)))
		dx <- sapply(index(Q1z), f)
		QP <- cbind(Q1_full, light = coredata(light) [dx])
		Q1_full <- data.frame(QP)    

		f <- function(u) which.min(abs(as.numeric(index(d1)) - as.numeric(u)))
		dx <- sapply(index(Q1z), f)
		QP <- cbind(Q1_full, cum.light = coredata(lightmod) [dx])
		Q1_full <- data.frame(QP)   

#adding in a season variable

summer1 <- 	ifelse(as.numeric(format(Q1_full$Pd, "%m")) >= 6 & as.numeric(format(Q1_full$Pd, "%m")) <= 9, 1, 0)
Q1_full <- cbind(Q1_full, summer1)

year1 <- as.numeric(format(Q1_full$Pd, "%y"))
Q1_full <- cbind(Q1_full, year1)

					# #just check to make sure files are matched up right
				 #ggplot(Qw7, aes(x = Q.mod, y = Q_DS)) + geom_point()
				 #ggplot(Qw1, aes(x = Q.mod, y = Q_DS)) + geom_point()
	# #Export
	# write.csv(Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/Qw2.csv")


# combine PT files
 
 #ST1
depths <- merge(presLhr[,2:4], presHhr[,2:4], by = "time", all = TRUE)
depths <- merge(depths, pres1hr[,2:4], by = "time", all = T)
depths <- merge(depths, lightmodhr[,2:3] , by = "time", all = T)
depths <- depths[!as.numeric(format(depths$time, "%y")) == 1,]
names(depths) <- c("time","L_depthm", "L_tempC", "H_depthm", "H_tempC", "st1_depthm", "st1_tempC", "light")
 
# write.csv(depths, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Landscape_all.csv")


#Need to see the correlation between ST7 and landscape streams
	ggplot(depths, aes(x = L_depthm, y = st1_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,0.5)+
		ylim(0,0.5)

ggplot(depths, aes(x = H_depthm, y = st1_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,0.5)+
		ylim(0,0.5)

ggplot(depths, aes(x = st1_tempC, y = st1_depthm))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		ylim(0,0.5)

ggplot(depths, aes(x = L_tempC, y = st1_depthm))+
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
	ggplot(Q1_full, aes(y = temp_DS_m, x = temp_1))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,30)+
		ylim(0,30)
		
	#How does the relatioship look btw Q and Depth
   ggplot(Q1_full, aes(y = Q.mod, x = d1, group = cum.light, colour = cum.light, label = Qdate))+
		geom_point()+
		geom_text() +
		#scale_color_brewer(palette = "Set1") +
		#stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1) +
		#xlim(.35, .45) +
		ylab("Discharge (L/s)") +
		xlab("Depth (m)")
		#ylim(0.25,0.50)
		
	#If we remove a single 'outlier'

	Q1_full_mod <- Q1_full[-9,]
	ggplot(Q1_full_mod, aes(y = Q.mod, x = d1, group = summer1, color = factor(summer1)), show_guide = FALSE)+
		geom_point()+
		scale_color_brewer(palette = "Set1") +
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0.25, .50)+
		ylim(0,50)


#model selection to determine the best model
	#Q <- Q[2:34,]
	library(MuMIn)

	Q1_full_summer <- Q1_full[which(Q1_full$summer1 == 1),]
	Q1_full_nosummer <- Q1_full[which(Q1_full$summer1 == 0),]	


	Q1_full_summer_gm <- lm(log(Q.mod)~ log(d1), Q1_full_summer, na.action = "na.fail")
	Q1_full_summer_MS <- dredge(Q1_full_summer_gm, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	subset(Q1_full_summer_MS)

	Q1_full_nosummer_gm <- lm(log(Q.mod) ~ log(d1), Q1_full_nosummer, na.action = "na.fail")
	Q1_full_nosummer_MS <- dredge(Q1_full_nosummer_gm, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	subset(Q1_full_nosummer_MS)

	Q1_full_mod <- Q1_full[-9,]
	Q1_gm_mod <- lm(log(Q.mod) ~  log(d1) + temp_1 + as.factor(summer1) + log(d1)*as.factor(summer1), Q1_full_mod, na.action = "na.fail")
	Q1_MS_mod <- dredge(Q1_gm_mod, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	subset(Q1_MS_mod)

	Q1_light1_gm <- lm(log(Q.mod) ~ log(d1) + light, Q1_full, na.action = "na.omit")
	Q1_light1_MS <- dredge(Q1_light1_gm, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	subset(Q1_light1_MS)

	Q1_light_gm <- lm(log(Q.mod) ~ log(d1) * cum.light, Q1_full, na.action = "na.fail")
	Q1_light_MS <- dredge(Q1_light_gm, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	subset(Q1_light_MS)

	Q1_full_light <- Q1_full[!is.na(Q1_full$light),]

	Q1_gm_1 <- lm(log(Q.mod) ~ log(d1) + light + temp_1, Q1_full_light, na.action = "na.fail")

	Q1_MS_1 <- dredge(Q1_gm_1, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))

	subset(Q1_MS_1)

	Q1_full <- Q1_full[is.na(Q1_full$d1),]
	
	Q1_gm <- lm(log(Q.mod) ~  log(d1) + temp_1 + as.factor(summer1) , Q1_full, na.action ="na.fail")
	
	Q1_MS <- dredge(Q1_gm, extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	
	subset(Q1_MS, delta < 5)
	subset(Q1_MS)

#looking at colinearity
	ggplot(Q1_full, aes(x =d1, y = temp_1)) +geom_point()
	
	ggplot(Q1_full, aes(x = as.factor(summer1), y = d1)) + geom_boxplot()
	
	T_seas_tt <- t.test(T_US_PD ~ season, Q); T_seas_tt
	
	T_warm_tt <- t.test(T_US_PD ~ warming, Q); T_warm_tt

sm_rating1 <- lm(log(Q.mod) ~ log(d1) , Q1_full); summary(sm_rating1)
sm_rating1_mod <- lm(log(Q.mod) ~ log(d1) + as.factor(summer1) + temp_1, Q1_full_mod); summary(sm_rating1_mod)

sm_light_rating1 <- lm(log(Q.mod) ~ log(d1), Q1_full_summer); summary(sm_summer_rating1)
sm_nosummer_rating1 <- lm(log(Q.mod) ~ log(d1), Q1_full_nosummer); summary(sm_nosummer_rating1)


sm_light_rating1 <- lm(log(Q.mod) ~ log(d1) + light, Q1_full_light); summary(sm_light_rating1)
dev.new()
				
# # 				
	Q1_full$fitted <- fitted(sm_rating1)
	ggplot(Q1_full, aes(x = fitted, y =log(Q.mod), group = cum.light, colour = cum.light))+
		geom_point(size = 5.5)+
		geom_abline(intercept = 0, slope = 1)

	Q1_full_nosummer$fitted <- fitted(sm_nosummer_rating1)
	ggplot(Q1_full_nosummer, aes(x = fitted, y =log(Q.mod)))+
		geom_point()+
		geom_abline(intercept = 0, slope = 1)

	Q1_full_nosummer$fitted <- fitted(sm_nosummer_rating1)
	ggplot(Q1_full_nosummer, aes(x = exp(fitted), y = Q.mod)) +
		geom_point() +
		geom_abline(intercept = 0, slope = 1)



	Q1_full$fitted <- fitted(sm_rating1)
	ggplot(Q1_full, aes(x = fitted, y =log(Q.mod)))+
		geom_point()+
		geom_abline(intercept = 0, slope = 1)
	
F2 =	ggplot(Q1_full, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,40)+
		ylim(0,60)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")")))
	
		
	mrbias <- lm(log(Q.mod)~ fitted, Q1_full); summary(mrbias)
				##Call:
				##lm(formula = log(Q.mod) ~ fitted, data = Q1_full)

				#Residuals:
				#    Min      1Q  Median      3Q     Max 
				#-0.6711 -0.2111  0.1256  0.2252  0.4604 

				#Coefficients:
				#             Estimate Std. Error t value Pr(>|t|)    
				#(Intercept) 1.184e-15  5.190e-01   0.000  1.00000    
				#fitted      1.000e+00  1.722e-01   5.806  0.00066 ***
				#---
				#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				#Residual standard error: 0.3859 on 7 degrees of freedom
				#Multiple R-squared:  0.828,     Adjusted R-squared:  0.8035 
				#F-statistic: 33.71 on 1 and 7 DF,  p-value: 0.0006596

##working with the modified Q1_full

Q1_full_mod$fitted <- fitted(sm_rating1_mod)
	ggplot(Q1_full_mod, aes(x = fitted, y =log(Q.mod)))+
		geom_point()+
		geom_abline(intercept = 0, slope = 1)
	
	ggplot(Q1_full_mod, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,75)+
		ylim(0,75)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")")))
	
		
	mrbias_mod <- lm(log(Q.mod)~ fitted, Q1_full_mod); summary(mrbias_mod)


#Applying MR to predict new data in depths
	#first need to code for season
		depths$summer1 <- as.factor(ifelse(as.numeric(format(depths$time, "%m")) >= 6 & as.numeric(format(depths$time, "%m")) <= 10, 1, 0))
		depths$year1 <- as.numeric(format(depths$time, "%y"))


#depths$summer1 <- 	ifelse(as.numeric(format(depths$Pd, "%m")) <= 3, 1, ifelse(as.numeric(format(depths$Pd, "%m")) > 3 & as.numeric(format(depths$Pd, "%m")) <= 6, 2, 
#			ifelse(as.numeric(format(depths$Pd, "%m")) > 6 & as.numeric(format(depths$Pd, "%m")) <= 9, 3, 4)))


		
	#merge new files	
	depths_m <- depths
	names(depths_m) <- c("time", "dL", "temp_L", "dH", "temp_H", "d1", "temp_1", "cum.light", "summer1", "year1") #needs to be renamed so it matches the names in the equation.
	
depths_m$Q_mr <- exp(predict(sm_rating1, depths_m))
depths_m$Q_mr.mod <- exp(predict(sm_rating1_mod, depths_m))

 write.csv(depths_m, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/depth_m1.csv")
	

##ST1
	#look at Q over time
		ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q1_full, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste("Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))
		
	ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q1_full, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste(log[10]," Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))+
		scale_y_log10()

###plotting the modified and unmodified full Q data
dev.new()
	
ggplot(depths_m, aes(x = time, y = Q_mr.mod)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q1_full_mod, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste(log[10]," Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))+
		scale_y_log10()


#export data
	write.csv(depths_m, file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st1/depths_m1.csv")


save.image(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st1/st1.RData")
load(	"C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/st1/st1.RData")


F1 = 	ggplot(depths, aes(x = time, y = st1_depthm)) +
		geom_point(size = 3) +
		ggtitle("All depth measurements")


F2 = ggplot(Q1_full, aes(y = Q.mod, x = d1, label= Qdate))+
		geom_point()+
		geom_text() +
		scale_color_brewer(palette = "Set1") +
		#stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1) +
		xlim(.35, .45) +
		ggtitle("Depth - Q relationship")
		#ylim(0.25,.50)


data.labels <- data.frame(
	fitted = 13,
	Q.mod = 50,
	label = "R^2 = 0.21 \n p =0.3 \n eqn = log(Q) ~ log(depth)")

F3 = ggplot(Q1_full, aes(x =exp(fitted), y = Q.mod))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,50)+
		ylim(0,65)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")"))) +
		geom_text(data = data.labels, aes(x = fitted, y = Q.mod, label = label)) +
		ggtitle("Q - Fitted")

F4 = ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q1_full, aes(x = Pd, y = Q.mod), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste("Q (L ", s^-1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))


pdf("Stream1plots.pdf", height = 15, width = 8)
grid.arrange(F1, F2, F3, F4, nrow = 4, ncol = 1)
dev.off()


		
	#adding Q predicted estimates to Qw file	
st7_Qw2 <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw2.csv")

st7_Qw2$Q_DS_predicted <- exp(predict(sm_rating, st7_Qw2))

write.csv(st7_Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw3.csv")
	
#################################################################################################################################################################
#Working Log
#Feb-6-2015. added full date-time file and merged to get 15min intervals from Jul 2010