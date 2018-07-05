#load libraries
library(ggplot2)
library(chron)
library(gridExtra)
library(zoo)
library(scales)
library(GGally)
theme_set(theme_bw(20))

#load data
Q <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw2.csv")
presL <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/9736059_7LO.csv")
presH <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/9736163_7HI_noNAs.csv")


#convert times to posix objects
Q$Pd <- as.POSIXct(paste(Q$Qdate, Q$Qtime), format = "%m/%d/%y %H:%M:%S", tz = "UTC")
presL$Pd <- as.POSIXct(paste(presL$Date, presL$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")
presH$Pd <- as.POSIXct(paste(presH$Date, presH$Time), format = "%m/%d/%y %H:%M:%S", tz ="UTC")

#Make hourly means
	presLhr_d <- data.frame(presL$Pd, presL$Depthm, presL$TempC)
	presHhr_d <- data.frame(presH$Pd, presH$Depthm, presH$TempC)
	
	names(presLhr_d) <- c("time", "depthm", "tempC")
	names(presHhr_d) <- c("time", "depthm", "tempC")
	
	presLhr <- aggregate(presLhr_d[c("depthm", "tempC")],
					list(hour = cut(presLhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)
					
	presHhr <- aggregate(presHhr_d[c("depthm", "tempC")],
					list(hour = cut(presHhr_d$time, breaks = "hour")),
					mean, na.rm = TRUE)

#convert times to posix object
	presLhr$time <- as.POSIXct(presLhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
	presHhr$time <- as.POSIXct(presHhr$hour, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")


# # RUNNING ONE TIME, THEN EXPORTING TO GET PT TEMPS
# #get temp at times of slugs
	# #create zoo objects
		# temp_L <- with(presLhr, zoo(tempC, time))
		# temp_H <- with(presHhr, zoo(tempC, time))
		# Qz <- with(Q, zoo(Q_DS, Pd))
	
	# #merge Q and PT temp data
		# f <-  function(u) which.min(abs(as.numeric(index(temp_L)) - as.numeric(u)))
		# ix <- sapply(index(Qz), f)
		# QP <- cbind(Qz, temp_L = coredata(temp_L)[ix])
		# QP_df <- data.frame(QP)
		# Qw <- cbind(Q, QP_df)
		
		# f <-  function(u) which.min(abs(as.numeric(index(temp_H)) - as.numeric(u)))
		# ix <- sapply(index(Qz), f)
		# QP <- cbind(Qz, temp_H = coredata(temp_H)[ix])
		# QP_df2 <- data.frame(QP)
		# Qw2 <- cbind(Qw, QP_df2)
					# #just check to make sure files are matched up right
				# ggplot(Qw, aes(x = Qz, y = Q_DS)) + geom_point()

	# #Export
	# write.csv(Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/Qw2.csv")


# combine PT files
depths <- merge(presLhr[,2:4], presHhr[,2:4], by = "time", all = TRUE)
names(depths) <- c("time","L_depthm", "L_tempC", "H_depthm", "H_tempC")

# write.csv(depths, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/st7_LandH_PTs.csv")


#multiple regression
# season <- S = June - Sept; NS = other months

#NEED TO FIGURE OUT WHICH PD TEMP DATA TO USE
	#US DS comp with cond probes
	ggplot(Q, aes(x = T_US_cond, y = T_DS_cond))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,20)+
		ylim(0,20)
		
	#US DS comp with PDs
	ggplot(Q, aes(x = T_DS_PD, y = T_US_PD))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,20)+
		ylim(0,20)
		
	#US PD cond comparison <-THIS IS PRETTY GOOD
	ggplot(Q, aes(y = T_US_cond, x = T_US_PD))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,20)+
		ylim(0,20)
		
	#DS PD cond comparison <-THIS IS BIASED AT LOW TEMPS
	ggplot(Q, aes(y = T_DS_cond, x = T_DS_PD))+
		geom_point()+
		stat_smooth(method = "lm")+
		geom_abline(intercept = 0, line = 1)+
		xlim(0,20)+
		ylim(0,20)


#model selection to determine the best model
	Q <- Q[2:34,]
	library(MuMIn)
	
	Q_DS_gm <- lm(log(Q_DS) ~ log(H_d_hr) + season + T_US_PD + warming, Q)
	
	Q_DS_MS <- dredge(Q_DS_gm,  extra = c("R^2", F = function(x) summary(x)$fstatistic[[1]]))
	
	subset(Q_DS_MS, delta < 5)

#looking at colinearity
	ggplot(Q, aes(x =log(H_d_hr), y = T_US_PD)) +geom_point()
	
	ggplot(Q, aes(x = season, y = T_US_PD)) + geom_boxplot()
	
	T_seas_tt <- t.test(T_US_PD ~ season, Q); T_seas_tt
	
	T_warm_tt <- t.test(T_US_PD ~ warming, Q); T_warm_tt

sm_rating <- lm(log(Q_DS) ~ log(H_d_hr) + season + T_US_PD + warming, Q); summary(sm_rating)

				
# # 				Call:
				# lm(formula = log(Q_DS) ~ log(H_d_hr) + season + T_US_PD + warming, 
				    # data = Q)
				
				# Residuals:
				     # Min       1Q   Median       3Q      Max 
				# -0.45861 -0.14513 -0.03547  0.11831  0.69621 
				
				# Coefficients:
				            # Estimate Std. Error t value Pr(>|t|)    
				# (Intercept) 23.20638    2.11042  10.996 1.14e-11 ***
				# log(H_d_hr) 17.71696    1.86474   9.501 2.94e-10 ***
				# seasonS     -0.69217    0.12866  -5.380 9.82e-06 ***
				# T_US_PD     -0.17822    0.02862  -6.228 9.94e-07 ***
				# warmingpre  -0.47795    0.14074  -3.396  0.00206 ** 
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
				
				# Residual standard error: 0.2876 on 28 degrees of freedom
				# Multiple R-squared:  0.9028,	Adjusted R-squared:  0.889 
				# F-statistic: 65.05 on 4 and 28 DF,  p-value: 9.104e-14

	Q$fitted <- fitted(sm_rating)
	ggplot(Q, aes(x = fitted, y =log(Q_DS)))+
		geom_point()+
		geom_abline(intercept = 0, slope = 1)
	
	ggplot(Q, aes(x =exp(fitted), y = Q_DS))+
		geom_point(shape = 21, fill = "green", size = 3)+
		geom_abline(intercept = 0, slope = 1)+
		xlim(0,25)+
		ylim(0,25)+
		stat_smooth(method = "lm")+
		ylab(expression(paste("Measured DS Q (L",s^-1,")")))+
		xlab(expression(paste("Fit DS Q (L",s^-1,")")))
	
		
	mrbias <- lm(log(Q_DS)~ fitted, Q); summary(mrbias)
				# Call:
				# lm(formula = log(Q_DS) ~ fitted, data = Q)
				
				# Residuals:
				     # Min       1Q   Median       3Q      Max 
				# -0.45861 -0.14513 -0.03547  0.11831  0.69621 
				
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
		depths$month <- strftime(depths$time, format = "%m")
		season_S <- depths[depths$month == "06" |depths$month == "07" | depths$month == "08" | depths$month == "09",]
		season_S$season <- rep("S", length(dim(season_S)[1]))
		
		season_NS <- depths[depths$month != "06" & depths$month != "07" & depths$month != "08" & depths$month != "09",]
		season_NS$season <- rep("NS", length(dim(season_NS)[1]))
	
	#merge new files	
	depths_m <- rbind(season_S, season_NS)
	names(depths_m) <- c("time", "L_d_hr", "T_DS_PD", "H_d_hr", "T_US_PD", "month", "season") #needs to be renamed so it matches the names in the equation.
	
	#second need to code for warming
	depths_prew <- depths_m[depths_m$time <= strptime("2011-10-22 00:00:00", "%Y-%m-%d %H:%M:%S"),]
	
	depths_prew$warming <- rep("pre", length(dim(depths_prew)[1]))
	
	depths_postw <- depths_m[depths_m$time > strptime("2011-10-22 00:00:00", "%Y-%m-%d %H:%M:%S"),]
	depths_postw$warming <- rep("post", length(dim(depths_postw)[1]))
	depths_m <- rbind(depths_prew, depths_postw)
	
	


	#predict Q
	depths_m$Q_mr <- exp(predict(sm_rating, depths_m))
	
	#look at Q over time
		ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q, aes(x = Pd, y = Q_DS), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste("Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))
		
	ggplot(depths_m, aes(x = time, y = Q_mr)) +
		geom_line(color = "blue", size = 0.25)+
		geom_point(data =  Q, aes(x = Pd, y = Q_DS), shape = 21, fill = "red")+
		xlab("Date")+
		ylab(expression(paste(log[10]," Q (L ", s^1,")")))+
		scale_x_datetime(breaks = "6 months", labels = date_format("%b-%y"))+
		scale_y_log10()
		
	#export data
	write.csv(depths_m, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/transducer data/st7_LandH_PTs2.csv")
		
		
		
	#adding Q predicted estimates to Qw file	
st7_Qw2 <- read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw2.csv")

st7_Qw2$Q_DS_predicted <- exp(predict(sm_rating, st7_Qw2))

write.csv(st7_Qw2, file = "~/Dropbox/JMH_dropbox/stephanson2/Projects/Cross Postdoc/Data/NaCl & nutrient slugs/rating curves/summary_data/st7_Qw3.csv")
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	